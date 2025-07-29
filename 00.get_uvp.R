#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Download UVP data
# Date: 05/01/2024
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")


## UVP objects ----
#--------------------------------------------------------------------------#
# Read file of objects
uvp_o <- read_tsv("data/raw/uvp/objects.tsv.gz", show_col_types = FALSE)

# Select and rename columns
uvp_o <- uvp_o %>% select(
    # both object_id and profile_id are necessary to retrieve images
    object_id,
    profile_id,
    depth,
    taxon = group,
    area:skeleton_area
  ) %>%
  mutate(profile_id = as.character(profile_id), object_id = as.character(object_id))


## Taxonomy ----
#--------------------------------------------------------------------------#
# Read file of selected taxa
sel_taxa <- read_tsv("data/raw/uvp/selected_taxa.tsv", show_col_types = FALSE) %>% select(-lineage)

# Drop unwanted taxa
uvp_o <- uvp_o %>%
  left_join(sel_taxa, by = join_by(taxon)) %>%
  filter(!is.na(use_as)) %>%
  mutate(taxon = use_as) %>%
  relocate(large_group, .after = taxon) %>%
  select(-use_as)


## UVP profiles ----
#--------------------------------------------------------------------------#
# Read samples
uvp_s <- read_tsv("data/raw/uvp/new_samples.tsv.gz", show_col_types = FALSE) %>%
  select(profile_id = sample_id, lon, lat, datetime, pixel_size, uvp_model) %>%
  mutate(profile_id = as.character(profile_id))

# Join with profiles info
uvp_o <- uvp_o %>%
  left_join(uvp_s, by = join_by(profile_id)) %>%
  select(object_id, profile_id, lon, lat, datetime, depth, taxon, large_group, pixel_size, uvp_model, everything())

# Keep only SD UVP
uvp_s <- uvp_s %>% filter(uvp_model == "SD")
uvp_o <- uvp_o %>% filter(uvp_model == "SD")


## Process descriptors ----
#--------------------------------------------------------------------------#
# Convert px to mm for size descriptors
# Compute esd
# Remove non meaningful descriptors
uvp_o <- uvp_o %>%
  # px to mm
  mutate(
    # length²
    area     = area     * pixel_size^2,
    area_exc = area_exc * pixel_size^2,
    skelarea = skelarea * pixel_size^2,
    convarea = convarea * pixel_size^2,
    # length
    perim.    = perim.    * pixel_size,
    width     = width     * pixel_size,
    height    = height    * pixel_size,
    major     = major     * pixel_size,
    minor     = minor     * pixel_size,
    feret     = feret     * pixel_size,
    convperim = convperim * pixel_size,
    # 1 / length²
    symetrieh_area = symetrieh_area / pixel_size^2,
    symetriev_area = symetriev_area / pixel_size^2,
    nb1_area       = nb1_area       / pixel_size^2,
    nb2_area       = nb2_area       / pixel_size^2,
    nb3_area       = nb3_area       / pixel_size^2
  ) %>%
  # compute esd
  mutate(esd = sqrt(area/pi), .after = area) %>%
  # remove meaningless for morphology
  select(-c(xmg5, ymg5, centroids, angle, areai, tag, pixel_size))


## Extract profiles ----
#--------------------------------------------------------------------------#
uvp_prof <- uvp_o %>%
  select(profile_id, lon, lat, datetime) %>%
  distinct()


## Save ----
#--------------------------------------------------------------------------#
# Save
save(uvp_o, uvp_prof, file = "data/00.all_uvp.Rdata")

