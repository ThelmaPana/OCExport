## Packages ----
#--------------------------------------------------------------------------#
library(tidyverse)
library(castr)
library(cmocean)
library(oce)
library(ncdf4)
library(fields)
library(glue)
library(parallel)
library(abind)
library(castr)
library(chroma)
library(R.matlab)

# Modeling
library(tidymodels)
library(rpart.plot)
library(vip)


## Default values ----
#--------------------------------------------------------------------------#

# GGplot theme
theme_set(theme_minimal())

# Path to data folder
data_dir <- "data"

# Path to local WOA links
woa_loc <- dir <- file.path(data_dir, "raw/woa")
dir.create(woa_loc, showWarnings=FALSE, recursive=TRUE)

# Path to WOA dataset
woa_dir <- "~/Documents/work/datasets/woa"

# Number of cores for parallel computing
n_cores <- 14

# Whether to perform download for WOA data
download_woa <- FALSE

# Max depth for WOA data
max_depth_WOA <- 500

# Max depth of the layer we consider for predictors
layer_bottom <- 100

## Get world map data ----
#--------------------------------------------------------------------------#
# For plots
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon=long)

# To compute inland points
coast <- read_csv("data/raw/gshhg_world_c.csv", col_types=cols())


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
