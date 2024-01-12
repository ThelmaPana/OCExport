#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Download carbon export data
# Date: 20/12/2023
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")


## Read data as matrix ----
#--------------------------------------------------------------------------#
d_mat <- readMat("data/raw/Cexp_CAFE_kl24h.mat")$EXP[,,1]$POC1000

# Generate colnames as longitudes and rownames as latitudes
colnames(d_mat) <- (c(0.5:179.5) * 2)
rownames(d_mat) <- (c(0:90) * 2) - 90


## Convert to dataframe ----
#--------------------------------------------------------------------------#
df_c <- d_mat %>%
  as.data.frame() %>%
  rownames_to_column(var = "lat") %>%
  as_tibble() %>%
  pivot_longer(cols = -lat, names_to = "lon", values_to = "poc") %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>%
  mutate(lon = ifelse(lon > 180, lon - 360, lon)) %>%
  mutate(poc = ifelse(poc == 0, NA, poc)) %>%
  arrange(lon, lat)

df_c %>%
  ggplot() +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray") +
  geom_raster(aes(x = lon, y = lat, fill = poc)) +
  scale_fill_viridis_c(na.value = NA) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  coord_quickmap()


## Save ----
#--------------------------------------------------------------------------#
save(df_c, file = "data/00.carbon_data.Rdata")
