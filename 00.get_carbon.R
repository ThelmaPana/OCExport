#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Download carbon export data
# Date: 20/12/2023
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#


library(tidyverse)
library(R.matlab)

toto <- readMat("data/raw/Cexp_CAFE_kl24h.mat")
titi <- toto$EXP
str(titi)
tata <- titi[,,1]$POCexp
image(t(tata))

dim(tata)

lat <- dim(tata)[1]
lon <- dim(tata)[2]

# Reformat data as akima::interp output
x <- unique(df_int$dist)
y <- unique(df_int$depth)
z <- matrix(df_int[[variable]], nrow = length(x), byrow = TRUE)
list_int <- list(x=x, y=y, z=z)

length(c(1:180))
length(seq(from = 2, to = 360, by = 2))





colnames(tata) <- (c(0.5:179.5) * 2)
rownames(tata) <- (c(0:90) * 2) - 90


foo <- tata %>%
  as.data.frame() %>%
  rownames_to_column(var = "lat") %>%
  as_tibble() %>%
  pivot_longer(cols = -lat, names_to = "lon", values_to = "poc") %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
  mutate(poc = ifelse(poc == 0, NA, poc))

foo %>%
  ggplot() +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group)) +
  geom_raster(aes(x = lon, y = lat, fill = poc)) +
  scale_fill_viridis_c(na.value = NA)

foo %>%
  ggplot() +
  geom_raster(aes(x = lon, y = lat, fill = poc==0))

world <- fortify(map_data('world')) %>% rename(lon=long)
summary(world)
