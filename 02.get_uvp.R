#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Download UVP data
# Date: 05/01/2024
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")


load("data/raw/uvp/extraction.Rdata")


## List UVP profiles with coordinates ----
#--------------------------------------------------------------------------#
profiles <- o %>% select(sampleid, lon, lat) %>% unique()



## Remove non-living objects ----
#--------------------------------------------------------------------------#
o <- o %>%
  filter(str_starts(lineage, "living")) %>%
  filter(!taxon %in% c("artefact", "detritus"))
unique(o$taxon)


## Compute diversity indices per profile ----
#--------------------------------------------------------------------------#
# Taxonomic richness
# Shannon Diversity
# Pielou Evenness

# Generate a contingency table as a matrix to feed to vegan
cont <- o %>%
  count(sampleid, taxon) %>%
  pivot_wider(names_from = "taxon", values_from = "n", values_fill = 0) %>%
  as.data.frame() %>%
  column_to_rownames(var = "sampleid") %>%
  as.matrix()

# Compute diversity metrics
div <- tibble(
  sampleid = rownames(cont) %>% as.numeric(),
  t_ric = specnumber(cont),
  t_shannon = diversity(cont, index = "shannon"),
  t_pielou = t_shannon/t_ric
) %>%
  left_join(profiles, by = join_by(sampleid)) %>%
  select(sampleid, lon, lat, everything())
div

div %>%
  ggplot() +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray") +
  geom_point(aes(x = lon, y = lat, colour = t_ric)) +
  scale_colour_viridis_c() +
  coord_quickmap()

div %>%
  ggplot() +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray") +
  geom_point(aes(x = lon, y = lat, colour = t_shannon)) +
  scale_colour_viridis_c() +
  coord_quickmap()

div %>%
  ggplot() +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "gray") +
  geom_point(aes(x = lon, y = lat, colour = t_pielou)) +
  scale_colour_viridis_c() +
  coord_quickmap()

# Join metrics with profiles
profiles <- profiles %>% left_join(div, by = join_by(sampleid, lon, lat))


# Save
save(profiles, file = "data/02.uvp_profiles.Rdata")
