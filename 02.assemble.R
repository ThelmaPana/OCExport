#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Assemble datasets of env and carbon data
# Date: 02/01/2024
# Author: Thelma Panaiotis
#--------------------------------------------------------------------------#

source("utils.R")

load("data/00.carbon_data.Rdata")
load("data/01.env_data.Rdata")
rm(env_m, coast)


## Round env data to match with carbon data ----
#--------------------------------------------------------------------------#
env <- env_y %>%
  arrange(lon, lat) %>%
  mutate(
    # floor longitude and add 1 because carbon longitudes are odd
    lon = roundp(lon, precision = 2, f = floor) + 1,
    # round latitude because carbon latitudes are even
    lat = roundp(lat, precision = 2, f = round)
  ) %>%
  # Average all values on carbon pixels
  group_by(lon, lat) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  ungroup()


## Match with carbon data ----
#--------------------------------------------------------------------------#
df <- env %>%
  left_join(df_c, by = join_by(lon, lat))


## Keep only means ----
#--------------------------------------------------------------------------#
df <- df %>%
  select(lon, lat, poc, contains("mean"))

## Save ----
#--------------------------------------------------------------------------#
save(df, file = file.path(data_dir, "03.all_data.Rdata"))
