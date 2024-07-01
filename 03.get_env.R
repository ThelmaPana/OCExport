#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Prepare environmental data
# Date: 20/12/2023
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#
# (c) 2023 T Panaïotis, L Drago, JO Irisson, GNU General Public License v3


## Set up ----
#--------------------------------------------------------------------------#
source("utils.R")


## Download WOA data ----
#--------------------------------------------------------------------------#
if (download_woa){
  # define all combinations of variables to download
  df <- read_csv(
    "var,abbrv,period
    temperature,t,A5B7
    salinity,s,A5B7
    AOU,A,all
    silicate,i,all
    phosphate,p,all
    nitrate,n,all
    oxygen,o,all
  ")
  month <- sprintf("%02i",1:12)
  combi <- crossing(df, month)

  # define download URLs
  urls <- glue("https://data.nodc.noaa.gov/thredds/fileServer/ncei/woa/{var}/{period}/1.00/woa18_{period}_{abbrv}{month}_01.nc", .envir=combi)

  # and download files
  lapply(urls, function(url) {
    message(basename(url))
    download.file(url, destfile=file.path(woa_dir, basename(url)))
    Sys.sleep(10)
  })
  message("Done downloading WOA data")

  # create links to the downloaded files with easier names
  ok <- file.symlink(
    from = glue("{woa_dir}/woa18_{period}_{abbrv}{month}_01.nc", .envir=combi),
    to = file.path(woa_loc, glue("{var}_{month}.nc", .envir=mutate(combi, month = as.numeric(month))))
  )
  all(ok)
}

orig_files <- glue("{woa_dir}/woa18_{period}_{abbrv}{month}_01.nc", .envir=combi)
f <- orig_files[1]
nc_open(f)

## Read WOA data ----
#--------------------------------------------------------------------------#

# List WOA variables
vars <- c("temperature", "salinity", "AOU", "silicate", "phosphate", "nitrate", "oxygen")

# Open one file to get all coordinates (lon, lat, depth)
nc <- nc_open(file.path(woa_loc, "temperature_1.nc"))
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
depth <- ncvar_get(nc, "depth")
# Get indexes of relevant depth
depth_idx <- which(depth <= max_depth_WOA)
# Limit depth to chosen max depth
depth <- ncvar_get(nc, "depth", count = max(depth_idx))
# Number of depth values
depth_count <- max(depth_idx)
# Close the file
nc_close(nc)

# Read all files in parallel
woa <- mclapply(vars, function(var) {
  # prepare storage for one variable at n depths for 12 months
  block <- array(NA, c(360, 180, depth_count, 12))

  for (month in 1:12) {
    # define the file to read
    file <- str_c(woa_loc, "/", var, "_", month, ".nc")
    # open the file and read the data in it
    nc <- nc_open(file)
    block[,,,month] <- ncvar_get(nc, varid=names(nc$var)[6], count = c(360, 180, depth_count, 1))
    nc_close(nc)
  }
  return(block)
}, mc.cores = min(length(vars), n_cores))

# Add variable names
names(woa) <- vars



dim(woa$temperature)
image.plot(woa$temperature[,,1,1], col = cmocean("thermal")(100))
image.plot(woa$temperature[,,1,1], col = brewer_colors(100, "Blues"))

brewer_colors(100, "Blues")


## Compute density ----
#--------------------------------------------------------------------------#
# Compute pressure rather than depth
press <- array(NA, dim = dim(woa$temperature)[1:3])

# compute for one longitude
for (j in seq(along = lat)) {
  press[1,j,] <- swPressure(depth[1:depth_count], latitude = lat[j])
}
# replicate for all longitudes
for (i in seq(along = lon)) {
  press[i,,] <- press[1,,]
}

# derive potential density anomaly using oce package
woa$density <- mclapply(1:12, function(m) { # in parallel
  dens <- array(NA, dim = dim(woa$temperature)[1:3])
  for (i in seq(along = lon)) {
    for (j in seq(along = lat)) {
      dens[i,j,] <- swSigma0(
        woa$salinity[i,j,,m],
        woa$temperature[i,j,,m],
        press[i,j,], lon[i], lat[j]
        )
    }
  }
  return(dens)
}, mc.cores = n_cores)
woa$density <- do.call(abind, list(woa$density, along = 4))


image.plot(woa$density[,, 10, 1])


## Compute MLD ----
#--------------------------------------------------------------------------#
# Look at a few profiles for MLP and pycnocline
sub <- data.frame()
for (y in c(30, 60, 90, 120, 170)) {
  for (m in c(1,3,5,7,9,11)) {
    sub <- bind_rows(sub, data.frame(dens = woa$density[50,y,,m], y = y, m = m))
  }
}

# compute clines for these profiles
ssub <- sub %>%
  group_by(y, m) %>%
  do({
    d <- .$dens
    ok <- !is.na(d)
    depths_i <- seq(0, max_depth_woa, by = 5)
    dens_i <- interpolate(depth[ok], d[ok], depths_i, method = "spline", extrapolate=FALSE)
    pyc <- clined(dens_i, depths_i, n.smooth = 2, k = 4)
    mld <- mld(dens_i, depths_i, ref.depths = 0:15, n.smooth = 2, k = 4)
    data.frame(depth = depths_i, dens = dens_i, pyc = pyc, mld = mld, id = str_c(.$y[1], "-",.$m[1]))
})

# and plot
ggplot(ssub) +
  geom_path(aes(x = dens, y = -depth)) +
  geom_hline(aes(yintercept = -pyc), data = distinct(ssub, id, pyc), colour="red") +
  geom_hline(aes(yintercept = -mld), data = distinct(ssub, id, mld), colour="blue") +
  facet_wrap(~id)
# -> we indeed want the pycnocline

# for each pixel of each month, interpolate density over depth and derive pycnocline depth
depths_i <- seq(0, max_depth_woa, by = 5)

pycno <- mclapply(1:12, function(m) { # in parallel
  # pycno <- mclapply(c(1,7), function(m) { # test
  apply(woa$density[,,,m], c(1,2), function(dens) {
    # apply(woa$density[1:60,,,m], c(1,2), function(dens) { # test
    # dens <- woa$density[8, 30, ,9] # test
    # check number of available data points
    ok <- !is.na(dens)
    if (sum(ok) > 3) {
      # interpolate density on 5 m steps
      dens_i <- castr::interpolate(depth[ok], dens[ok], depths_i, method = "spline", extrapolate = FALSE)
      # compute pycnocline depth
      ok <- !is.na(dens_i)
      pyc <- clined(dens_i[ok], depths_i[ok], n.smooth = 2, k = 4)
    } else {
      pyc <- NA
    }
    return(pyc)
  })
}, mc.cores = n_cores)  # looooong, even in parallel

# smooth the result to avoid local artefacts
pycno <- lapply(pycno, function(x) {
  xs <- image.smooth(x, theta = 1)$z
  return(xs)
})
walk(pycno, image.plot, col = chroma::viridis_colors(100))

# combine all months into a matrix
pycno <- do.call(abind, list(pycno, along = 3))


## Average over surface layer ----
#--------------------------------------------------------------------------#
# For each variable, compute average in the surface layer if >80% of data is present in this layer
env_monthly <- mclapply(vars, function(var) {
  message(var)

  # extract variable
  X <- woa[[var]]

  # prepare storage
  res <- array(NA, dim(X)[-3])

  # for each pixel of each month
  for (i in seq(along = lon)) {
    for (j in seq(along = lat)) {
      for (m in 1:12) {
        # compute average if >80% of data is present
        keep <- X[i, j, depth <= layer_bottom, m]
        if (percent_na(keep) <= 0.2) {
          res[i, j, m] <- mean(keep, na.rm=TRUE)
        }
      }
    }
  }
  return(res)
}, mc.cores = min(length(vars), n_cores))

# Add variable names
names(env_monthly) <- vars


## Combine all env variables ----
#--------------------------------------------------------------------------#
# Add other relevant variables to the list
#env_monthly$ze <- ze
env_monthly$pycno <- pycno
#env_monthly$chla <- chla

# Remove dimnames (which are empty anyway) just to be clean
#dimnames(env_monthly$ze) <- NULL
dimnames(env_monthly$pycno) <- NULL
#dimnames(env_monthly$lim_epi_meso) <- NULL
#dimnames(env_monthly$chla) <- NULL


## Convert to dataframe ----
#--------------------------------------------------------------------------#
# unroll each matrix
env_m <- lapply(env_monthly, function(e) { as.vector(e) })
# combine as columns
env_m <- do.call(cbind, env_m) %>% as.data.frame() %>% setNames(names(env_m))
# add coordinates (NB: shorter elements are recycled automatically)
env_m$lon <- lon
env_m$lat <- rep(lat, each=length(lon))
env_m$month <- rep(1:12, each=length(lon)*length(lat))



# check
env_m %>%
  filter(month == 8) %>%
  ggplot() +
  geom_raster(aes(lon, lat, fill = temperature)) +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray") +
  scale_fill_distiller(palette="Spectral") +
  scale_fill_cmocean(name = "thermal", na.value = NA) +
  scale_xy_map() +
  theme_minimal() +
  coord_quickmap()



## Remove internal seas and lakes ----
#--------------------------------------------------------------------------#
coast %>%
  ggplot() +
  geom_polygon(aes(x = lon, y = lat)) +
  coord_quickmap()

# determine which points are in land
env_m1 <- env_m %>% filter(month == 1)
# NB: do it for one month only because those are the same for all months
lons <- env_m1$lon
lats <- env_m1$lat
inland <- sp::point.in.polygon(lons, lats, coast$lon, coast$lat) == 1
ggplot(env_m1) +
  geom_raster(aes(x = lon, y = lat, fill = inland)) +
  scale_fill_viridis_d() +
  coord_quickmap()

# remove South Pole
inland[lats < -85] <- TRUE

# remove Black Sea too
inland[between(lons, 20, 50) & between(lats, 41, 50)] <- TRUE
ggplot(env_m1) +
  geom_raster(aes(x = lon, y = lat, fill = inland)) +
  scale_fill_viridis_d() +
  coord_quickmap()

# remove Baltic Sea too
#inland[between(lons, 12, 30) & between(lats, 52, 66)] <- TRUE

# blankout points in land
env_m[inland, ! names(env_m) %in% c("lon", "lat", "month")] <- NA
# NB: this works because `inland` gets automatically repeated for everymonth

# check
env_m %>%
  filter(month == 7) %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = world, aes(group = group), fill = "gray") +
  geom_raster(aes(fill=temperature)) +
  scale_fill_cmocean(name = "thermal", na.value = NA) +
  scale_xy_map() +
  coord_quickmap() +
  theme_minimal()


## Compute yearly climatology ----
#--------------------------------------------------------------------------#
# Compute mean and sd per pixel
env_y <- env_m %>%
  select(-month) %>%
  group_by(lon, lat) %>%
  summarise_all(.funs=list(mean = mean, sd = sd), na.rm = TRUE) %>%
  ungroup()

# Give better names
#names(env_y) <- names(env_y) %>% str_replace("(.*?)_mean", "mean.\\1") %>% str_replace("(.*?)_sd", "sd.\\1")

## Add to the env_y the bathymetry filtered (without the different seas)
#bathy_filtered <- env_m %>% select(lon, lat, bathymetry) %>%
#  group_by(lon, lat) %>%
#  summarise(bathymetry = mean(bathymetry, na.rm=TRUE)) %>%
#  ungroup()
#env_y <- env_y %>% inner_join(bathy_filtered)


env_y %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = world, aes(group = group), fill = "gray") +
  geom_raster(aes(fill = temperature_mean)) +
  scale_fill_cmocean(name = "thermal", na.value = NA) +
  scale_xy_map() +
  coord_quickmap()

env_y %>%
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(data = world, aes(group = group), fill = "gray") +
  geom_raster(aes(fill = temperature_sd)) +
  scale_fill_viridis_c(na.value = NA) +
  scale_xy_map() +
  coord_quickmap()


## Convert to tibble ----
#--------------------------------------------------------------------------#
env_m <- env_m %>%
  as_tibble() %>%
  select(lon, lat, month, everything())


## Save ----
#--------------------------------------------------------------------------#
save(env_m, env_y, coast, file = file.path(data_dir, "01.env_data.Rdata"))


## HDF5 ----
#--------------------------------------------------------------------------#
#library(tidync)
#
#tidync("data/gshhg-gmt-2.3.7/binned_GSHHS_c.nc") %>%
#  activate("D5") %>%
#  hyper_tibble()
