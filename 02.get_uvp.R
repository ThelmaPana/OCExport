#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Download UVP data
# Date: 05/01/2024
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")


# For now, we process with TARA data only


## Load all UVP5 formatted dataset ----
#--------------------------------------------------------------------------#
# We need this to get definitive taxa
load("data/raw/uvp/extraction.Rdata")
o_dataset <- o
rm(o)
# Keep only objects_ids and taxa
o_dataset <- o_dataset %>%
  select(object_ids = objid) %>%
  # convert object_ids to character for future join
  mutate(object_ids = as.character(object_ids))

## Get project info ----
#--------------------------------------------------------------------------#
# Query Tara project
my_proj <- project_query(579)


## Set-up configuration for extraction ----
#--------------------------------------------------------------------------#
## Filter for only validated and living objects
# taxo = 1 means living, so we take living and all children
filter <- ProjectFilters(taxo = 1, taxochild = "Y", statusfilter = "V")


## Features
# Get list of features available from this project
features <- names(my_proj$obj_free_cols)
# Drop feature names starting by "x" or "y" as these are position features within the original image and have no morphological meaning
features <- features[!(str_starts(features, "x") | str_starts(features, "y"))]
# also drop "bx" and "by"
features <- features[!(features == "bx" | features == "by")]


## Additional fields on top of features
# Display name
# Depth
# Lon
# Lat
# List fields to extract + features
fields <- str_flatten(c("txo.display_name,obj.depth_max,obj.longitude,obj.latitude", str_c("fre.", features)), collapse = ",")
# Generate nice column names from fields
col_names <- str_split_fixed(str_split(fields, ",")[[1]], "\\.", n = 2)[,2]



## Perform extraction ----
#--------------------------------------------------------------------------#
o <- get_object_set(
  project_id = 579,
  fields = fields,
  ProjectFilters = filter
)
toto <- o

# Format the "details" part of the output
o$details <- o$details %>%
  # convert to tibble
  as_tibble(.name_repair = "minimal") %>%
  # use our nice names
  setNames(col_names)%>%
  # convert all column except first two (classif_qual and display name) to numeric
  mutate(across(col_names[-c(1,2)], as.integer))

# Bind all outputs in a tibble
o <- as_tibble(o[1:4]) %>%
  bind_cols(o$details) %>%
  # Drop unwanted columns
  select(-acquisition_ids) %>%
  # Rename columns
  rename(taxon = display_name, depth = depth_max, lon = longitude, lat = latitude) %>%
  # Set ids as character
  mutate(across(object_ids:project_ids, as.character))


## Replace ecotaxa taxonomy by the one of the UVP5 dataset ----
#--------------------------------------------------------------------------#
# Perform a join with the formatted UVP5 dataset
o <- o %>%
  select(-taxon) %>%
  left_join(o_dataset, by = join_by(object_ids)) %>%
  select(object_ids:project_ids, taxon, depth, lon, lat, everything())

# Drop object which were not found in the UVP5 dataset because they were omitted on purpose
o <- o %>% filter(!is.na(taxon))

## List profiles and taxa ----
#--------------------------------------------------------------------------#
taxa <- o %>% pull(taxon) %>% unique() %>% sort()

profiles <- o %>%
  select(sample_ids, lon, lat) %>%
  unique()


## Save data ----
#--------------------------------------------------------------------------#
save(o, profiles, taxa, file = "data/02.tara_uvp.Rdata")


# ## Load downloaded data ----
# #--------------------------------------------------------------------------#
# # This will be the workflow when the UVP5 dataset will be available.
# # In the meantime, we have to download objects from ecotaxa
# load("data/raw/uvp/extraction.Rdata")
# o_dataset <- o
# rm(o)
#
# ## List UVP profiles with coordinates
# profiles <- o_dataset %>% select(sampleid, lon, lat) %>% unique()
#
# ## Remove non-living objects
# o_dataset <- o_dataset %>%
#   filter(str_starts(lineage, "living")) %>%
#   filter(!taxon %in% c("artefact", "detritus"))
#
# ## List taxa
# taxa <- o_dataset %>% pull(taxon) %>% unique() %>% sort()
#
# ## Save
# save(o_dataset, profiles, taxa, file = "data/02.all_uvp.Rdata")
