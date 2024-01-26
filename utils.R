## Packages ----
#--------------------------------------------------------------------------#
suppressWarnings(library(tidyverse))
suppressWarnings(library(parallel))
suppressWarnings(library(here))
suppressWarnings(library(reticulate))

# Reading & downloading
suppressWarnings(library(ncdf4))
suppressWarnings(library(R.matlab))
suppressWarnings(library(ecotaxarapi))

# Processing
suppressWarnings(library(glue))
suppressWarnings(library(castr))
suppressWarnings(library(fields))
suppressWarnings(library(abind))
suppressWarnings(library(oce))
suppressWarnings(library(vegan))
suppressWarnings(library(morphr))
suppressWarnings(library(mFD))

# Plots
suppressWarnings(library(cmocean))
suppressWarnings(library(chroma))
suppressWarnings(library(ggrepel))

# Modeling
suppressWarnings(library(tidymodels))
suppressWarnings(library(rpart.plot))
suppressWarnings(library(vip))
suppressWarnings(library(DALEXtra))




## Default values ----
#--------------------------------------------------------------------------#

# Number of cores for parallel computing
n_cores <- 14

# GGplot theme
theme_set(theme_minimal())

# Path to data folder
data_dir <- "data"

## WOA
# Path to local WOA links
woa_loc <- here(file.path(data_dir, "raw/woa"))
dir.create(woa_loc, showWarnings=FALSE, recursive=TRUE)

# Path to WOA dataset
woa_dir <- "~/Documents/work/datasets/woa"

# Whether to perform download for WOA data
download_woa <- FALSE

# Max depth for WOA data, this data will be used to compute clines
max_depth_woa <- 500

# Max depth of the layer we consider for predictors
# Data will be averaged from the surface to this layer
layer_bottom <- 10

## UVP
# Depth above which to keep UVP objects
max_depth_uvp <- 1000

# Minimum number of objects in a UVP profile to consider it
n_min_uvp <- 10



## Colour palettes ----
#--------------------------------------------------------------------------#

## Colour palettes for image.plot
col_temp = cmocean("thermal")(100)
col_sal  = cmocean("haline")(100)
col_dens = cmocean("dense")(100)
col_oxy  = brewer_colors(100, "Blues")
col_nit  = cmocean("tempo")(100)
col_phos = brewer_colors(100, "BuPu")
col_sil  = brewer_colors(100, "PuBu")
col_chl  = cmocean("algae")(100)
col_irr  = cmocean("solar")(100)
col_depth  = cmocean("deep")(100)
col_poc  = cmocean("matter")(100)
col_misc  = viridis_colors(100)

## Colour palettes for ggplot
div_pal <- scale_colour_gradient2(low = "#4575b4", mid = "#ffffbf", high = "#d73027") # diverging palette centered at 0


## Get world map data ----
#--------------------------------------------------------------------------#
# For plots
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon = long)

# To compute inland points
coast <- read_csv(here("data/raw/gshhg_world_c.csv"), col_types = cols())


## Compute percentage of NA in a vector ----
#--------------------------------------------------------------------------#
#' Compute percentage of NA in a vector
#' @param x input vector
#' @return number in [0,1] = the percentage of NA; returns 1 is x is empty.
percent_na <- function(x) {
  if (length(x) == 0) {
    res <- 1
  } else {
    res <- sum(is.na(x)) / length(x)
  }
  return(res)
}

## Plot a nice ggplot map ----
#--------------------------------------------------------------------------#
#' Plot a map, whether raster or points
#'
#' Generate a ggplot raster map from a dataframe and a variable name.
#' Palette can be inferred from the name of the variable or provided in the arguments.
#' If no palette is found, it defaults to viridis.
#' Land is plotted by default but this can be changed in arguments.
#'
#' @param df a dataframe, must contain at least 3 columns: `lon`, `lat` and var to plot
#' @param var a character, name of variable to plot
#' @param type a character, defining type of map (raster or points)
#' @param land a boolean, whether to plot land or not
#' @param palette a filling palette, will be generated if none is
#'
#' @return a ggplot object
#' @export
#'
#' @examples ggmap(df, var = "temperature", type = "raster")
ggmap <- function(df, var, type = c("raster", "point"), land = TRUE, palette = NULL) {
  ## Check args
  # df is a dataframe containing "lon", "lat" and var
  checkmate::assert_data_frame(df)
  checkmate::assert_names(names(df), must.include = c("lon", "lat", var))
  # var is a character
  checkmate::assert_character(var)
  # type is either "raster" or "point"
  checkmate::assert_choice(type, c("raster", "point"))
  # land is a boolean
  checkmate::assert_logical(land)
  # palette is a palette or NULL
  checkmate::assert_multi_class(palette, c("ScaleContinuous", "Scale", "NULL"))

  ## Palettes
  # To look for palette, remove "_mean" in case we are working with annual climatology values
  var_pal <- str_remove(var, "_mean")

  # If no palette is supplied, generate one
  if(is.null(palette)){

    # List of palettes for common variables
    pals <- tribble(
      ~vars, ~raster, ~point,
      "temperature", scale_fill_cmocean(name = "thermal", na.value = NA),                  scale_colour_cmocean(name = "thermal", na.value = NA),
      "salinity",    scale_fill_cmocean(name = "haline", na.value = NA),                   scale_colour_cmocean(name = "haline", na.value = NA),
      "density",     scale_fill_cmocean(name = "dense", na.value = NA),                    scale_colour_cmocean(name = "dense", na.value = NA),
      "oxygen",      scale_fill_distiller(palette = "Blues", na.value = NA),               scale_colour_distiller(palette = "Blues", na.value = NA),
      "nitrate",     scale_fill_cmocean(name = "tempo", na.value = NA),                    scale_colour_cmocean(name = "tempo", na.value = NA),
      "phosphate",   scale_fill_distiller(palette = "BuPu", na.value = NA, direction = 1), scale_colour_distiller(palette = "BuPu", na.value = NA, direction = 1),
      "silicate",    scale_fill_distiller(palette = "PuBu", na.value = NA, direction = 1), scale_colour_distiller(palette = "PuBu", na.value = NA, direction = 1),
      "chl",         scale_fill_cmocean(name = "algae", na.value = NA),                    scale_colour_cmocean(name = "algae", na.value = NA),
      "par",         scale_fill_cmocean(name = "solar", na.value = NA),                    scale_colour_cmocean(name = "solar", na.value = NA),
      "mld",         scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "mld_argo",    scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "pyc",         scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "z_eu",        scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "s_cline",     scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "p_cline",     scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "n_cline",     scale_fill_cmocean(name = "deep", na.value = NA),                     scale_colour_cmocean(name = "deep", na.value = NA),
      "poc",         scale_fill_cmocean(name = "matter", na.value = NA),                   scale_colour_cmocean(name = "matter", na.value = NA)
    )

    # Find the matching palette for variable to plot
    pal <- pals %>% filter(vars == var_pal) %>% pull(all_of(type))

    # If no palette is found, use viridis
    if (length(pal) == 0 & type == "raster"){
      pal <- scale_fill_viridis_c(na.value = NA)
    } else if (length(pal) == 0 & type == "point"){
      pal <- scale_colour_viridis_c(na.value = NA)
    }

  } else { # If palette is supplied, use it!
    pal <- palette
  }

  ## Plot
  # Base plot
  p <- ggplot(df) +
    coord_quickmap(expand = FALSE) +
    theme_minimal()

  # add raster or point layer
  if (type == "raster"){
    p <- p + geom_raster(aes(x = lon, y = lat, fill = .data[[var]]), na.rm = TRUE)
  } else {
    p <- p + geom_point(aes(x = lon, y = lat, colour = .data[[var]]), na.rm = TRUE, size = 0.5)
  }

  # add land if required
  if (land){p <- p + geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray")}

  # add palette
  p <- p + pal

  ## Return plot
  return(p)
}


## Preprocess images for morphr ----
#--------------------------------------------------------------------------#
# Function to pre-process images
preprocess <- function(x) {
  x |>
    # remove 31 pixels from the bottom (=the scale bar)
    img_chop(bottom=31) |>
    # change the gamma to see the light objects better
    img_adjust_gamma(gamma=0.7)
}

## Draw a circle ----
#--------------------------------------------------------------------------#
# To draw a circle
circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}


## Plot variable importance ----
#--------------------------------------------------------------------------#
ggplot_imp <- function(...) {
  obj <- list(...)
  metric_name <- attr(obj[[1]], "loss_name")
  metric_lab <- paste(metric_name,
                      "after permutations\n(higher indicates more important)")

  full_vip <- bind_rows(obj) %>%
    filter(variable != "_baseline_")

  perm_vals <- full_vip %>%
    filter(variable == "_full_model_") %>%
    group_by(label) %>%
    summarise(dropout_loss = mean(dropout_loss))

  p <- full_vip %>%
    filter(variable != "_full_model_") %>%
    mutate(variable = fct_reorder(variable, dropout_loss)) %>%
    ggplot(aes(dropout_loss, variable))
  if(length(obj) > 1) {
    p <- p +
      facet_wrap(vars(label)) +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss, color = label),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(aes(color = label, fill = label), alpha = 0.2)
  } else {
    p <- p +
      geom_vline(data = perm_vals, aes(xintercept = dropout_loss),
                 linewidth = 1.4, lty = 2, alpha = 0.7) +
      geom_boxplot(fill = "#91CBD765", alpha = 0.4)

  }
  p +
    theme(legend.position = "none") +
    labs(x = metric_lab,
         y = NULL,  fill = NULL,  color = NULL)
}


## Plot partial dependence plot ----
#--------------------------------------------------------------------------#
ggplot_pdp <- function(obj, x) {

  p <-
    as_tibble(obj$agr_profiles) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "^[^_]*_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(data = as_tibble(obj$cp_profiles),
              aes(x = {{ x }}, group = `_ids_`),
              linewidth = 0.5, alpha = 0.05, color = "gray50")

  num_colors <- n_distinct(obj$agr_profiles$`_label_`)

  if (num_colors > 1) {
    p <- p + geom_line(aes(color = `_label_`), linewidth = 1.2, alpha = 0.8)
  } else {
    p <- p + geom_line(color = "midnightblue", linewidth = 1.2, alpha = 0.8)
  }

  p
}

## Functional diversity metrics ----
#--------------------------------------------------------------------------#
#' Compute functional diversity indices
#'
#' @param coord morphological space (morphs x axes)
#' @param weight weight matrix (date x morphs)
#' @param verb print information about the progress of computation
multidimFD4  <-  function(coord, weight, verb=TRUE) {

  # library required for indices computation
  library("geometry")
  library("ape")
  # library("foreach")
  # library("doParallel")

  # registerDoParallel(cores=4)

  # saving name of current working directory
  # current_wd <- getwd()


  ##############################################################################
  # info on study case


  ###############################

  # number and names of axes
  nm_axes <- colnames(coord)
  nb_axes <- length(nm_axes)

  # number and names of assemblages
  nm_asb <- row.names(weight)
  nb_asb <- length(nm_asb)

  # matrix to store results
  indices <- c( "Nb_sp", "Tot_weight", c("FRic","FDiv","FEve") )
  FD <- matrix(NA, nb_asb, length(indices), dimnames=list(nm_asb,indices))

  ##############################################################################
  # preliminary computation at the species pool level

  #######################################

  # convex hull volume of the species pool
  FRic_pool <- convhulln(coord,"FA")$vol

  ##############################################################################

  for (k in nm_asb) {

    # FD[k, "assemblage"] <- k

    ###########################################################
    # preparing data

    # names, number, weight and coordinates of of species present
    weight_k <- weight[k,]
    nm_sp_k <- row.names(coord)[which(weight_k>0)]
    nb_sp_k <- length(nm_sp_k)
    weight_sp_k <- weight[k,nm_sp_k]
    coord_sp_k <- coord[nm_sp_k,]
    if(nb_sp_k==1) { coord_sp_k <- matrix(coord_sp_k,nrow=1,dimnames=list(nm_sp_k,colnames(coord)) ) } # matrix object

    # names of species absent
    nm_sp_absent_k <- names(which(weight[k,]==0))

    # total weight
    FD[k, "Tot_weight"] <- sum(weight_sp_k)

    #relative weight
    rel_weight_sp_k <- weight_sp_k/sum(weight_sp_k)

    # species richness
    FD[k, "Nb_sp"] <- nb_sp_k

    ###########################################################
    # multivariate indices
    # indices based on vertices and volume of convex hull, only if more species than number of axes
    ########################
    # Functional richness = convex hull volume
    FD[k, "FRic"] <- round(convhulln(coord_sp_k,"FA")$vol/FRic_pool,6)

    ########################
    # Functional Divergence

    # identity of vertices
    vert0 <- convhulln(coord_sp_k,"Fx TO 'vert.txt'")
    vert1 <- scan("vert.txt",quiet=T)
    vertices_k <- (vert1+1)[-1]

    # coordinates of the center of gravity of the vertices (B)
    B_k <- apply(coord_sp_k[vertices_k,],2,mean)

    # Euclidean dstance to B (dB)
    dB_k <- apply(coord_sp_k, 1, function(x) { (sum((x-B_k)^2) )^0.5} )

    # mean of dB values and deviations to mean
    meandB_k <- mean(dB_k)
    devdB_k <- dB_k-meandB_k

    # abundance-weighted mean deviation
    abdev_k <-  rel_weight_sp_k*devdB_k
    ababsdev_k <-  rel_weight_sp_k*abs(devdB_k)

    FD[k, "FDiv"] <- round( (sum(abdev_k)+meandB_k) / (sum(ababsdev_k)+meandB_k) ,6)


    # end of if more species than number of axes

    ##########################
    # Functional Evenness

    # inter-species Euclidean distance
    distT_k <- dist(coord_sp_k, method="euclidian")

    # topology of Minimum Spanning Tree and conversion of the 'mst' matrix into 'dist' class
    linkmst_k <- mst(distT_k)
    mstvect_k <- as.dist(linkmst_k)

    # pairwise cumulative relative abundances and conversion into 'dist' class
    ab2_k <- matrix(0,nrow=nb_sp_k,ncol=nb_sp_k)
    for (q in 1:nb_sp_k)
      for (r in 1:nb_sp_k)
        ab2_k[q,r] <- rel_weight_sp_k[q]+rel_weight_sp_k[r] # end of q,r
    ab2vect_k <- as.dist(ab2_k)

    # EW index for the (S-1) segments
    EW_k <- rep(0,nb_sp_k-1)
    flag <- 1
    for (m in 1:((nb_sp_k-1)*nb_sp_k/2))
    {if (mstvect_k[m]!=0) {EW_k[flag] <- distT_k[m]/(ab2vect_k[m]) ; flag <- flag+1}}  # end of m

    # PEW index and comparison with 1/S-1
    minPEW_k <- rep(0,nb_sp_k-1)  ;  OdSmO_k <- 1/(nb_sp_k-1)
    for (l in 1:(nb_sp_k-1))
      minPEW_k[l] <- min( (EW_k[l]/sum(EW_k)) , OdSmO_k )  # end of l

    # FEve
    FD[k, "FEve"] <- round( ( (sum(minPEW_k))- OdSmO_k) / (1-OdSmO_k ) ,6)


    # printing step achieved
    if (verb==TRUE) print(paste("FD of assemblage '",k,"' computed",sep="") )
  }# end of working on assemblage k

  # loop on assemblages for computing and plotting functional diversity indices


  # End of indices computation
  ######################################################################################################################

  ######################################################################################################################

  # returning to current working directory
  # setwd(current_wd)

  # returning results
  return(FD)

}# end of function multidimFD
########################################################################################################################################
########################################################################################################################################
