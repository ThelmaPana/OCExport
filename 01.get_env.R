#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Prepare environmental data
# Date: 20/12/2023
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

library(tidyverse)
library(castr)
library(cmocean)
library(oce)
library(ncdf4)
library(fields)
library(chroma)

source("utils.R")

# Get world map
#world <- fortify(map_data('world')) %>% rename(lon=long)
coast <- read_csv("data/gshhg_world_c.csv", col_types=cols())




df <- read_csv("data/raw/woa18_decav_t00mn01.csv.gz", skip = 1)
colnames(df)[1:3] <- c("lat", "lon", "0")
df <- df %>%
  pivot_longer(cols = -c(lat, lon), names_to = "depth", values_to = "temp") %>%
  mutate(depth = as.numeric(depth))

cl <- df %>%
  group_by(lon, lat) %>%
  summarise(thermo = clined(temp)) %>%
  ungroup()

df %>%
  filter(depth == 0) %>%
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = temp)) +
  scale_fill_cmocean(name = "thermal", na.value = NA) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  coord_quickmap() +
  theme_minimal()

h5read("data/chl.m.2022/chl.2022001.hdf", name = "chl")

library(terra)
s <- rast("data/chl.m.2022/chl.2022032.hdf", nrows = 1080, ncols = 2160)
s <- rast("data/chl.m.2022/chl.2022032.hdf")
str(s)
s$chl.2022001
terra::as.data.frame(s) %>%
  as_tibble() %>%
  mutate(chl.2022032 = ifelse(chl.2022032 == -9999, NA, chl.2022032))

# 1080 rows * 1/6 degree per row = 180 degrees of latitude (+90 to -90).
# 2160 columns * 1/6 degree per column = 360 degrees of longitude (-180 to +180).



library(ncdf4)
nc <- nc_open("data/chl.m.2022/chl.2022001.hdf")
foo <- raster::raster("data/chl.m.2022/chl.2022001.hdf")
foo
bar <- foo %>% raster::as.data.frame(xy = TRUE) %>% as_tibble()
summary(bar)

bar %>%
  rename(lon = x, lat = y, chl = chl.2022001) %>%
  mutate(chl = ifelse(chl == -9999, NA, chl)) %>%
  mutate(
    lon = lon * (1/6),
    lat = lat * (1/6),
    lon = roundp(lon, precision = 1, f = floor),
    lat = roundp(lat, precision = 1, f = floor)
  ) %>%
  group_by(lon, lat) %>%
  summarise(chl = mean(chl, na.rm = TRUE)) %>%
  ungroup()



## Download WOA with Laetitia’s script ----
#--------------------------------------------------------------------------#
# (c) 2023 T Panaïotis, L Drago, JO Irisson, GNU General Public License v3
library(glue)
library(tidyverse)
library(ncdf4)
library(parallel)
library(fields)
library(abind)
library(castr)

n_cores <- 8

# define all combinations of variables to download
df <- read_csv("var,abbrv,period
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
data_dir <- "data"
woa_dir <- file.path(data_dir, "raw/woa")
dir.create(woa_dir, showWarnings=FALSE, recursive=TRUE)

lapply(urls, function(url) {
  message(basename(url))
  # If the file was not downloaded yet, download it
  if (!file.exists(file.path(woa_dir, basename(url)))){
    download.file(url, destfile=file.path(woa_dir, basename(url)))
    Sys.sleep(10)
    }
})
message("Done downloading WOA data")

# create links to the downloaded files with easier names
ok <- file.symlink(
  from=basename(urls),
  # from=basename(path.to.complex.woa) # for shared database
  to=file.path(woa_dir, glue("{var}_{month}.nc", .envir=mutate(combi, month=as.numeric(month))))
)
all(ok)


## Consolidate WOA data ----
#--------------------------------------------------------------------------#


vars <- c("temperature","salinity","AOU","silicate","phosphate","nitrate","oxygen")
max_depth_WOA <- 500

# get vectors of coordinates (lat, lon, depth)
nc <- nc_open(file.path(woa_dir, "woa18_A5B7_t06_01.nc"))
lon <- ncvar_get(nc, "lon")
lon # -179.5 to 179.5
lat <- ncvar_get(nc, "lat")
lat # -89.5 to 89.5
depth <- ncvar_get(nc, "depth")
depth_idx <- which(depth <= max_depth_WOA)
depth <- ncvar_get(nc, "depth", count=max(depth_idx))
#if (config$element == "All_C") {depth <- ncvar_get(nc, "depth", count=37)} # 0 to 500m
#if (config$element == "Rhizaria_Si") {depth <- ncvar_get(nc, "depth")} #0 to 800m
nc_close(nc)

depth_count <- max(depth_idx)


#woa18_{period}_{abbrv}{month}_01.nc

woa <- mclapply(vars, function(var) {

  # prepare storage for one variable at n depths for 12 months
  block <- array(NA, c(360,180,depth_count,12))

  for (month in 1:12) {
    # define the file to read
    file <- str_c(woa_dir, "/", var, "_", month, ".nc")
    # open the file and read the data in it
    nc <- nc_open(file)
    block[,,,month] <- ncvar_get(nc, varid=names(nc$var)[6], count=c(360, 180, depth_count, 1))
    # setwd(data_dir)
    nc_close(nc)
  }
  return(block)
}, mc.cores=min(length(vars), n_cores))

names(woa) <- vars

# plot a few images at the surface
m <- month <- 4
image.plot(woa$temperature[,,1,m], col=brewer_colors(100, "Spectral", rev=T))
# temp >= 30
image.plot(woa$temperature[,,1,m]>30, col=c("#67a9cf","#eb9494"))
image.plot(woa$salinity[,,1,m], col=brewer_colors(100, "GnBu"))
image.plot(woa$AOU[,,1,m], col=brewer_colors(100, "Blues"))
image.plot(woa$silicate[,,1,m], col=brewer_colors(100, "YlOrBr"))
image.plot(woa$phosphate[,,1,m], col=brewer_colors(100, "BuPu"))
image.plot(woa$nitrate[,,1,m], col=brewer_colors(100, "PuBu"))


## Compute density ----
#--------------------------------------------------------------------------#
# Compute pressure rather than depth
press <- array(NA, dim=dim(woa$temperature)[1:3])

# compute for one longitude
for (j in seq(along=lat)) {
  press[1,j,] <- oce::swPressure(depth[1:depth_count], latitude=lat[j])
}
# replicate for all longitudes
for (i in seq(along=lon)) {
  press[i,,] <- press[1,,]
}

# derive potential density anomaly
woa$density <- mclapply(1:12, function(m) { # in parallel
  dens <- array(NA, dim=dim(woa$temperature)[1:3])
  for (i in seq(along=lon)) {
    for (j in seq(along=lat)) {
      dens[i,j,] <- oce::swSigma0(
        woa$salinity[i,j,,m],
        woa$temperature[i,j,,m],
        press[i,j,], lon[i], lat[j]
        )
    }
  }
  return(dens)
}, mc.cores = n_cores)
woa$density <- do.call(abind, list(woa$density, along=4))

# check how density looks
image.plot(1:360, 1:180, woa$density[,,1,4], col=brewer_colors(100, "YlGnBu"))


## Compute MLD ----
#--------------------------------------------------------------------------#
# pick some profiles in various locations and various months
sub <- data.frame()
for (y in c(30, 60, 90, 120, 170)) {
  for (m in c(1,3,5,7,9,11)) {
    sub <- bind_rows(sub, data.frame(dens=woa$density[50,y,,m], y=y, m=m))
  }
}

# compute

ssub <- sub %>% group_by(y, m) %>% do({
  d <- .$dens
  ok <- !is.na(d)
  depths_i <- seq(0, 500, by=5)
  dens_i <- interpolate(depth[ok], d[ok], depths_i, method="spline", extrapolate=FALSE)
  pyc <- clined(dens_i, depths_i, n.smooth=2, k=4)
  mld <- mld(dens_i, depths_i, ref.depths=0:15, n.smooth=2, k=4)
  data.frame(depth=depths_i, dens=dens_i, pyc=pyc, mld=mld, id=str_c(.$y[1], "-",.$m[1]))
})

# and plot
ggplot(ssub) + facet_wrap(~id) +
  geom_path(aes(x=dens, y=-depth)) +
  geom_hline(aes(yintercept=-pyc), data=distinct(ssub, id, pyc), colour="red") +
  geom_hline(aes(yintercept=-mld), data=distinct(ssub, id, mld), colour="blue")
# -> we indeed want the pycnocline


# for each pixel of each month, interpolate density over depth and derive pycnocline depth
depths_i <- seq(0, depth_count, by=5)

pycno <- mclapply(1:12, function(m) { # in parallel
  # pycno <- mclapply(c(1,7), function(m) { # test
  apply(woa$density[,,,m], c(1,2), function(dens) {
    # apply(woa$density[1:60,,,m], c(1,2), function(dens) { # test
    # dens <- woa$density[8, 30, ,9] # test
    # check number of available data points
    ok <- !is.na(dens)
    if (sum(ok) > 3) {
      # interpolate density on 5 m steps
      dens_i <- castr::interpolate(depth[ok], dens[ok], depths_i, method="spline", extrapolate=FALSE)
      # dens_i <- interpolate(depth[1:depth_count], dens[ok], depths_i, method="spline", extrapolate=FALSE)
      # compute pycnocline depth
      ok <- !is.na(dens_i)
      pyc <- clined(dens_i[ok], depths_i[ok], n.smooth=2, k=4)
    } else {
      pyc <- NA
    }
    return(pyc)
  })
}, mc.cores=n_cores)  # looooong, even in parallel
walk(pycno, image.plot, col=viridis_colors(100))

# smooth the result to avoid local artefacts
pycno <- lapply(pycno, function(x) {
  xs <- image.smooth(x, theta=1)$z
  return(xs)
})
walk(pycno, image.plot, col=viridis_colors(100))

# combine all months into a matrix
pycno <- do.call(abind, list(pycno, along=3))


## Average over layer ----
#--------------------------------------------------------------------------#
if (config$element == "All_C") {
  # limit epi-meso = max Ze and pycnocline
  lim_epi_meso <- pmax(ze, pycno, na.rm=TRUE)

  # smooth again
  lim_epi_meso <- lapply(1:12, function(m) { image.smooth(lim_epi_meso[,,m], theta=1)$z })
  lim_epi_meso <- do.call(abind, list(lim_epi_meso, along=3))

  # plot it
  for (m in 1:12) image.plot(lim_epi_meso[,,m], col=viridis_colors(100), main=m)
}

layer_bottom <- 100

# for each variable, compute average in each layer if >80% of data is present in this layer
vars <- c("temperature","salinity","AOU","silicate","phosphate","nitrate","oxygen")

env_monthly <- mclapply(vars, function(var) {
  message(var)

  # extract variable
  X <- woa[[var]]

  # prepare storage
  res <- array(NA, dim(X)[-3])

  # for each pixel of each month
  for (i in seq(along=lon)) {
    for (j in seq(along=lat)) {
      for (m in 1:12) {
        # compute average if >80% of data is present
        keep <- X[i,j,depth <= layer_bottom,m]
        if (percent_na(keep)<=0.2) {
          res[i,j,m] <- mean(keep, na.rm=TRUE)
        }
      }
    }
  }
  return(res)
}, mc.cores=min(length(vars), n_cores))
names(env_monthly) <- vars


## Combine all env variables ----
#--------------------------------------------------------------------------#
# add other relevant variables to the list
#env_monthly$ze <- ze
env_monthly$pycno <- pycno
#env_monthly$chla <- chla

# remove dimnames (which are empty anyway) just to be clean
#dimnames(env_monthly$ze) <- NULL
dimnames(env_monthly$pycno) <- NULL
#dimnames(env_monthly$lim_epi_meso) <- NULL
#dimnames(env_monthly$chla) <- NULL

## Convert to dataframe ----
#--------------------------------------------------------------------------#

toto <- data.table::rbindlist(env_monthly)

env_monthly["temperature"]

# unroll each matrix
env_m <- lapply(env_monthly, function(e) { as.vector(e) })
# combine as columns
env_m <- do.call(cbind, env_m) %>% as.data.frame() %>% setNames(names(env_m))
# add coordinates (NB: shorter elements are recycled automatically)
env_m$lon   <- lon
env_m$lat   <- rep(lat, each=length(lon))
env_m$month <- rep(1:12, each=length(lon)*length(lat))

as_tibble(env_m)

# check
ggplot(filter(env_m, month==8)) + coord_quickmap() + scale_xy_map() +
  geom_raster(aes(lon, lat, fill=temperature)) +
  scale_fill_distiller(palette="Spectral")

ggplot(filter(env_m, month==8)) + coord_quickmap() + scale_xy_map() +
  geom_raster(aes(lon, lat, fill=oxygen)) +
  scale_fill_distiller(palette="Spectral")

## Remove internal seas and lakes ----
#--------------------------------------------------------------------------#
# Read coarse coastline
ggplot(coast) + geom_polygon(aes(x=lon, y=lat))

# determine which points are in land
env_m1 <- filter(env_m, month == 1)
# NB: do it for one month only because those are the same for all months
lons <- env_m1$lon
lats <- env_m1$lat
inland <- sp::point.in.polygon(lons, lats, coast$lon, coast$lat) == 1
ggplot(env_m1) + geom_raster(aes(lon, lat, fill=inland))

# remove South Pole
inland[lats < -85] <- TRUE

# remove Black Sea too
inland[between(lons, 20, 50) & between(lats, 41, 50)] <- TRUE
ggplot(env_m1) + geom_raster(aes(lon, lat, fill=inland))

# remove Baltic Sea too
inland[between(lons, 12, 30) & between(lats, 52, 66)] <- TRUE
ggplot(env_m1) + geom_raster(aes(lon, lat, fill=inland))

# blankout points in land
env_m[inland, ! names(env_m) %in% c("lon", "lat", "month")] <- NA
# NB: this works because `inland` gets automatically repeated for everymonth

# check
ggplot(filter(env_m, month==7)) + coord_quickmap() + scale_xy_map() +
  geom_raster(aes(lon, lat, fill=temperature)) +
  scale_fill_distiller(palette="Spectral")


## Compute yearly climatology ----
#--------------------------------------------------------------------------#

# Compute mean and sd per pixel
env_y <- env_m %>% select(-month) %>%
  group_by(lon, lat) %>%
  summarise_all(.funs=list(mean=mean, sd=sd), na.rm=TRUE) %>%
  ungroup()

# Give better names
names(env_y) <- names(env_y) %>% str_replace("(.*?)_mean", "mean.\\1") %>% str_replace("(.*?)_sd", "sd.\\1")

## Add to the env_y the bathymetry filtered (without the different seas)
#bathy_filtered <- env_m %>% select(lon, lat, bathymetry) %>%
#  group_by(lon, lat) %>%
#  summarise(bathymetry = mean(bathymetry, na.rm=TRUE)) %>%
#  ungroup()
#env_y <- env_y %>% inner_join(bathy_filtered)


ggplot(env_y) + coord_quickmap() + scale_xy_map() +
  geom_raster(aes(lon, lat, fill=mean.temperature)) +
  scale_fill_distiller(palette="Spectral")
ggplot(env_y) + coord_quickmap() + scale_xy_map() +
  geom_raster(aes(lon, lat, fill=sd.temperature)) +
  scale_fill_distiller(palette="Spectral")


## Save ----
#--------------------------------------------------------------------------#
# redorder columns
#env_m <- select(env_m, month, lon, lat, everything())

# and save monthly and yearly climatologies
#save(env_m, env_y, coast, file=paste0("Data/",config$element,"/2.env.Rdata"))
