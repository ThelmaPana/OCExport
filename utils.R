## Packages ----
#--------------------------------------------------------------------------#
library(tidyverse)
library(parallel)

# Reading & downloading
library(ncdf4)
library(R.matlab)
library(ecotaxarapi)

# Processing
library(glue)
library(castr)
library(fields)
library(abind)
library(oce)
library(vegan)
library(morphr)

# Plots
library(cmocean)
library(chroma)

# Modeling
library(tidymodels)
library(rpart.plot)
library(vip)
library(DALEXtra)


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
