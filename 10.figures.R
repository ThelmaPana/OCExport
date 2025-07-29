#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Generate figures for the paper
# Date: 23/10/2024
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#


source("utils.R")


## List of figures ----
#--------------------------------------------------------------------------#
# Main
# 1 - Map of UVP profiles and b value
# 2 - Variable importance
# 3 - Partial dependence plots
# 4 - Illustration of top predictors

# Supp
# 5 - latitudinal distribution of plankton diversity metrics values and POC attenuation
# 6 - pairwise correlation of plankton diversity metrics


## Figure 1: Map of UVP profiles and b values ----
#--------------------------------------------------------------------------#
load("data/03.all_data.Rdata")
load("data/01.argo_map_coef.Rdata")

# Clean storage and save data
df1_glob <- argo_map_coef
df1_uvp <- df_all
write_csv(df1_glob, file = "figures/figures_data/df1_glob.csv")
write_csv(df1_uvp, file = "figures/figures_data/df1_uvp.csv")

# Plot
p1 <- ggplot() +
  geom_tile(data = df1_glob, aes(x = lon, y = lat, fill = b)) +
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "grey") +
  geom_point(data = df1_uvp, aes(x = lon, y = lat), size = 0.2, colour = "black", alpha = 0.5) +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(x = "Longitude", y = "Latitude", fill = "b") +
  coord_quickmap(expand = 0)

#ggplot() +
#  geom_tile(data = argo_map_coef, aes(x = lon, y = lat, fill = b)) +
#  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "grey") +
#  geom_point(data = df_all, aes(x = lon, y = lat), shape = 21, size = 0.8, colour = "black", alpha = 0.5) +
#  scale_fill_distiller(palette = "Blues", direction = 1) +
#  labs(x = "Longitude", y = "Latitude", fill = "b") +
#  coord_quickmap(expand = 0)

p1
#ggsave(p1, file = "figures/figure_1.png", width = 180, height = 90, units = "mm", bg = "white")


## Figure 2: Variable importance ----
#--------------------------------------------------------------------------#
load("data/04.att_from_plankton_all_fit.Rdata")

# Unnest variable importance
full_vip <- res |>
  select(cv_type, fold, importance) |>
  unnest(importance) |>
  mutate(variable = forcats::fct_reorder(variable, dropout_loss))

# Get RMSE for full model
full_rmse <- full_vip |>
  filter(variable == "_full_model_") |>
  select(cv_type, fold, full_dropout_loss = dropout_loss) |>
  distinct()

# Compute RMSE increase between loss and full model loss
full_vip_avg <- full_vip |>
  filter(!str_starts(variable, "_")) |> # Keep only variables of interest
  # Join with full model RMSE
  left_join(full_rmse, by = join_by(cv_type, fold), relationship = "many-to-many") |>
  # Compute difference
  mutate(diff_loss = dropout_loss - full_dropout_loss) |>
  group_by(cv_type, fold, variable) |>
  summarise(diff_loss = mean(diff_loss), .groups = "drop")

# Keep only 3 variables and group other
full_vip_avg <- full_vip_avg |>
  mutate(
    variable = fct_drop(variable),
    variable = fct_reorder(variable, diff_loss),
    variable = fct_other(variable, keep = tail(levels(variable), n = 3), other_level = "other"),
    variable = fct_reorder(variable, diff_loss)
  )

# Rename variables
full_vip_avg <- full_vip_avg |>
  mutate(
    # rename
    variable = case_when(
      variable == "other" ~ "Other",
      variable == "abund" ~ "Abund.",
      variable == "ta_eve_1" ~ "Tax. evenness",
      variable == "mo_grey_mean" ~ "Grey mean",
    ),
    # reorder
    variable = fct_reorder(variable, diff_loss)
  )

# Clean storage and save
df2 <- full_vip_avg
write_csv(df2, file = "figures/figures_data/df2.csv")

# Plot
p2 <- ggplot(df2) +
  geom_vline(xintercept = 0, linewidth = 1, colour = "grey") +
  geom_boxplot(aes(x = diff_loss, y = variable, colour = cv_type), linewidth = 0.3, outlier.size = 0.5) +
  scale_colour_manual(values = c("#298c8c", "#f1a226")) +
  labs(x = "Increase in RMSE after permutation", y = "Predictor", colour = "CV type")
p2
#ggsave(p2, file = "figures/figure_2.png", width = 120, height = 60, units = "mm", bg = "white")


## Figure 3: Partial dependence plots ----
#--------------------------------------------------------------------------#
# Variables for which to plot pdp
n_pdp <- 3
vars_pdp <- full_vip |>
  filter(variable != "_full_model_") |>
  mutate(variable = as.character(variable)) |>
  group_by(cv_type, variable) |>
  summarise(dropout_loss = mean(dropout_loss), .groups = "drop") |>
  arrange(desc(dropout_loss)) |>
  group_by(cv_type) |>
  slice_head(n = n_pdp) |>
  ungroup()

# Unnest cp_profiles
cp_profiles <- res |> select(cv_type, fold, cp_profiles) |> unnest(cp_profiles)

## Let’s generate averaged cp profile across folds for each cv-type and propagating uncertainties.
## The difficulty is that x values differ between each fold, the solution is to interpolate yhat on a common set of x values across folds.
## Steps as follows for each cv_type and each variable
## 1- compute the mean and spread of cp profiles within each fold
## 2- interpolate yhat value and spread within each fold using a common set of x values
## 3- perform a weighted average of yhat value and spread, using 1/var as weights

# Get names of folds, for later use
folds <- sort(unique(full_vip$fold))

# Apply on each cv_type and variable
mean_pdp <- lapply(1:nrow(vars_pdp), function(r){

  # Get variable and cvtype
  var_name <- vars_pdp[r,]$variable
  cv_type_name <- vars_pdp[r,]$cv_type

  ## Get corresponding CP profiles, compute mean and spread for each fold (step 1)
  d_pdp <- cp_profiles |>
    filter(cv_type == cv_type_name & `_vname_` == var_name) |>
    select(cv_type, fold, `_yhat_`, `_vname_`, `_ids_`, all_of(var_name)) |>
    arrange(`_ids_`, across(all_of(var_name))) |>
    # center each cp profiles across fold, variable and ids
    group_by(cv_type, fold, `_vname_`, `_ids_`) |>
    mutate(yhat_cent = `_yhat_` - mean(`_yhat_`)) |> # center cp profiles
    ungroup() |>
    # compute mean and sd of centered cp profiles for each fold and value of the variable of interest
    group_by(cv_type, fold, across(all_of(var_name))) |>
    summarise(
      yhat_loc = mean(`_yhat_`), # compute mean of profiles
      yhat_spr = sd(yhat_cent), # compute sd of cp profiles
      .groups = "keep"
    ) |>
    ungroup() |>
    setNames(c("cv_type", "fold", "x", "yhat_loc", "yhat_spr"))

  ## Interpolate yhat values and spread on a common x distribution (step 2)
  # Regularise across folds: need a common x distribution, and interpolate y on this new x
  new_x <- quantile(d_pdp$x, probs = seq(0, 1, 0.01), names = FALSE)
  # x is different within each fold, so interpolation is performed on each fold

  int_pdp <- lapply(1:length(folds), function(i){
    # Get data corresponding to this fold
    fold_name <- folds[i]
    this_fold <- d_pdp |> filter(fold == fold_name)

    # Extract original x values
    x <- this_fold$x
    # Extract values to interpolate (yhat_loc and yhat_spr)
    yhat_loc <- this_fold$yhat_loc
    yhat_spr <- this_fold$yhat_spr
    # Interpolate yhat_loc and yhat_spr on new x values
    int <- tibble(
      new_x = new_x,
      yhat_loc = castr::interpolate(x = x, y = yhat_loc, xout = new_x),
      yhat_spr = castr::interpolate(x = x, y = yhat_spr, xout = new_x),
    ) |>
      rename(x = new_x) |>
      mutate(
        cv_type = cv_type_name,
        fold = fold_name,
        var_name = var_name,
        .before = x
      )
    # Return the result
    return(int)

  }) |>
    bind_rows()

  ## Across fold, compute the weighted mean, using 1/var as weights (step 3)
  mean_pdp <- int_pdp |>
    group_by(cv_type, var_name, x) |>
    summarise(
      yhat_loc = wtd.mean(yhat_loc, weights = 1/(yhat_spr)^2),
      yhat_spr = wtd.mean(yhat_spr, weights = 1/(yhat_spr)^2),
      .groups = "drop"
    ) |>
    arrange(x)

  # Return the result
  return(mean_pdp)
}) |>
  bind_rows()

# Arrange in order of most important variables
mean_pdp <- vars_pdp |>
  rename(var_name = variable) |>
  left_join(mean_pdp, by = join_by(cv_type, var_name)) |>
  mutate(var_name = fct_inorder(var_name)) |>
  select(-dropout_loss)


# Log transform abundance values and rename variables
mean_pdp <- mean_pdp |>
  mutate(x = ifelse(var_name == "abund", log10(x), x)) |>
  mutate(
    # Rename predictors with nice names
    nice_name = case_when(
      var_name == "abund" ~ "Abund. (log10)",
      var_name == "ta_eve_1" ~ "Tax. evenness",
      var_name == "mo_grey_mean" ~ "Grey mean",
    ),
    # reorder
    nice_name = fct_inorder(nice_name)
  )

# Clean storage and save data
df3 <- mean_pdp
write_csv(df3, file = "figures/figures_data/df3.csv")

# Plot it!
p3 <- ggplot(df3) +
  geom_path(aes(x = x, y = yhat_loc, colour = cv_type)) +
  geom_ribbon(aes(x = x, ymin = yhat_loc - yhat_spr, ymax = yhat_loc + yhat_spr, fill = cv_type), alpha = 0.2) +
  scale_colour_manual(values = c("#298c8c", "#f1a226")) +
  scale_fill_manual(values = c("#298c8c", "#f1a226")) +
  labs(colour = "CV type", fill = "CV type", y = "Predicted log(b)") +
  facet_wrap(~nice_name, scales = "free_x", strip.position = "bottom") +
  theme(
    axis.title.x = element_blank(),
    strip.placement = "outside"
  )

p3
ggsave(p3, file = "figures/figure_3.png", width = 180, height = 45, units = "mm", bg = "white")


## Figure 4: Illustration of top predictors ----
#--------------------------------------------------------------------------#
# Find profiles to illustrate ta_eve, mo_grey_mean and abund

# Load UVP objects and profiles
load("data/03.all_data.Rdata")
load("data/02.uvp_o.Rdata") # UVP objects and profiles (original coordinates)
load("data/02.uvp_profiles.Rdata") # UVP objects and profiles (original coordinates)
load("data/01.uvp_poc.Rdata") # Processed data with regridded coordinates

## Taxonomic evenness patterns
# Prepare data for analysis
evenness_data <- df_all |> select(lon, lat, att, ta_eve_1)
summary(evenness_data)

# Get extreme cases (pixels with lowest/highest taxonomic evenness)
low_evenness_pixels <- evenness_data |> arrange(ta_eve_1) |> slice(1:10)
high_evenness_pixels <- evenness_data |> arrange(desc(ta_eve_1)) |> slice(1:10)
# Low evenness = unbalanced communities dominated by few taxonomic groups

# Extract objects from profiles within selected pixels
extract_pixel_objects <- function(pixel_data, case_label) {
  pixel_data |>
    rename(lon_r = lon, lat_r = lat) |>
    # Match pixels to profiles using regridded coordinates
    left_join(uvp_prof_c |> select(profile_id, lon, lat, lon_r, lat_r), by = join_by(lon_r, lat_r)) |>
    select(profile_id, lon_r, lat_r) |>
    # Get all objects within these profiles
    left_join(uvp_o, by = join_by(profile_id)) |>
    select(profile_id:large_group, lon_r, lat_r) |>
    # Create pixel IDs for plotting
    group_by(lon_r, lat_r) |>
    mutate(pixel_id = cur_group_id()) |>
    ungroup() |>
    mutate(case = case_label) |>
    # Some UVP profiles might be joined with coordinates rounding but should be excluded based on filters used in 02.
    # Drop these
    drop_na()
}

# Extract objects for both cases
low_evenness_objects <- extract_pixel_objects(low_evenness_pixels, "Low taxonomic evenness")
high_evenness_objects <- extract_pixel_objects(high_evenness_pixels, "High taxonomic evenness")

# Combine data
taxonomic_objects <- bind_rows(low_evenness_objects, high_evenness_objects)

# Clean storage and save
df4a <- taxonomic_objects
write_csv(df4a, file = "figures/figures_data/df4a.csv")

# Plot taxonomic composition by evenness case
p4a <- ggplot(df4a) +
  geom_bar(aes(x = pixel_id, fill = taxon), position = "fill", show.legend = FALSE) +
  labs(x = "Ocean pixel", y = "Proportion") +
  scale_fill_paletteer_d("khroma::discreterainbow") +
  scale_x_continuous(breaks = c(1:10)) +
  facet_wrap(~case) +
  theme_classic() +
  theme(strip.background = element_blank())
p4a
# Perfect!

## Grey level
grey_data <- df_all |> select(lon, lat, att, mo_grey_mean)
summary(grey_data)

# Get extreme cases (pixels with lowest/highest grey levels)
low_grey_pixels <- grey_data |> arrange(mo_grey_mean) |> slice(1:10)
high_grey_pixels <- grey_data |> arrange(desc(mo_grey_mean)) |> slice(1:10)

# High mo_grey_mean is supposed to be transparent
# Low mo_grey_mean is supposed to be dark

# Extract objects for both cases
low_grey_objects <- extract_pixel_objects(low_grey_pixels, "Low grey levels")
high_grey_objects <- extract_pixel_objects(high_grey_pixels, "High grey levels")

# Now we want to plot objects, see with morphr
# Folder containing images
img_dir <- "~/Documents/Data/UVP5/images/"

# Generate path to image
low_grey_objects <- low_grey_objects |> mutate(path_to_img = str_c(img_dir, profile_id, "/", object_id, ".jpg"), .before = object_id)
high_grey_objects <- high_grey_objects |> mutate(path_to_img = str_c(img_dir, profile_id, "/", object_id, ".jpg"), .before = object_id)

# Assemble together
df_grey <- bind_rows(
  low_grey_objects |> mutate(grey = "low") |> left_join(uvp_o |> select(object_id, mean), by = join_by(object_id)),
  high_grey_objects |> mutate(grey = "high") |> left_join(uvp_o |> select(object_id, mean), by = join_by(object_id))
) |>
  mutate(
    grey = factor(grey, levels = c("low", "high"))
  )

# Clean storage and save
df4b <- df_grey
write_csv(df4b, file = "figures/figures_data/df4b.csv")

# Plot violin distribution of grey levels for both how and high
p4b <- ggplot(df4b) +
  geom_violin(aes(x = mean, y = grey, fill = grey), show.legend = FALSE) +
  scale_fill_manual(values = c("grey80", "grey95")) +
  labs(x = "Mean grey level", y = "Grey level scenario") +
  scale_x_continuous(position = "top") +
  theme_classic()
p4b

## Generate image mosaics to illustrate grey level patterns
# Function to create a single image plot (without displaying)
create_image_plot <- function(img_path) {
  tryCatch({
    img <- img_read(img_path)
    # Create a plot object instead of displaying directly
    p <- ggplot() +
      annotation_raster(img |> img_chop(bottom = 35), xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      theme_void() +
      theme(plot.margin = unit(c(0,0,0,0), "mm"))
    return(p)
  }, error = function(e) {
    # Return empty plot if image fails
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Failed", size = 2) +
      theme_void() +
      theme(plot.margin = unit(c(0,0,0,0), "mm"))
  })
}

# Function to create image mosaic and return both plot and sampled data
create_image_mosaic <- function(object_data, mosaic_size = 10) {
  n_images <- mosaic_size^2

  # Sample images
  if (nrow(object_data) < n_images) {
    warning(paste("Only", nrow(object_data), "images available"))
    sampled_objects <- object_data
    n_missing <- n_images - nrow(object_data)
  } else {
    sampled_objects <- object_data |> slice_sample(n = n_images)
    n_missing <- 0
  }

  # Create list of plots
  plot_list <- list()

  # Add actual image plots
  for (i in 1:nrow(sampled_objects)) {
    plot_list[[i]] <- create_image_plot(sampled_objects$path_to_img[i])
  }

  # Add empty plots if needed
  if (n_missing > 0) {
    for (i in (nrow(sampled_objects) + 1):n_images) {
      plot_list[[i]] <- ggplot() + theme_void()
    }
  }

  # Arrange in grid
  mosaic_plot <- do.call(arrangeGrob, c(plot_list, list(ncol = mosaic_size)))

  # Convert to ggplot object for patchwork compatibility
  mosaic_ggplot <- ggplot() +
    annotation_custom(mosaic_plot) +
    theme_void() +
    coord_fixed()

  # Return both plot and data
  return(list(
    plot = mosaic_ggplot,
    sampled_data = sampled_objects
  ))
}

# Low grey levels
set.seed(18)
low_grey_result <- create_image_mosaic(low_grey_objects, mosaic_size = 10)
p4c <- low_grey_result$plot
p4c
low_grey_result$sampled_data |>
  count(taxon)

# Generate mosaics
# High grey levels
set.seed(6)
high_grey_result <- create_image_mosaic(high_grey_objects, mosaic_size = 10)
p4d <- high_grey_result$plot
p4d
high_grey_result$sampled_data |>
  count(taxon)


## Assemble panels of fig 4
layout <- "
AAAAA
BBBBB
CC#DD
CC#DD
"
p4 <- p4a + p4b + p4c + p4d + plot_layout(design = layout) + plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(hjust = 0, vjust = 2))
#p4
ggsave(p4, file = "figures/figure_4.png", width = 180, height = 200, units = "mm", bg = "white")

# Save also raw mosaics
ggsave(p4c, file = "figures/figures_data/mosaic_dark.png", width = 40, height = 40, units = "mm", bg = "white")
ggsave(p4d, file = "figures/figures_data/mosaic_light.png", width = 40, height = 40, units = "mm", bg = "white")


## Figure 5: Latitudinal pattern of attenuation and top predictors ----
#--------------------------------------------------------------------------#
load("data/03.all_data.Rdata")
df5 <- df_all |>
  # Keep only columns of interest
  select(lat, att, ta_eve_1, mo_grey_mean, abund) |>
  # Reshape to long format
  pivot_longer(att:abund) |>
  # Log transform abundance
  mutate(value = ifelse(name == "abund", log10(value), value)) |>
  # Rename predictors with nice names
  mutate(
    nice_name = case_when(
      name == "att" ~ "Att.",
      name == "ta_eve_1" ~ "Tax. evenness",
      name == "mo_grey_mean" ~ "Grey mean",
      name == "abund" ~ "Abund. (log10)"
    ),
    # make this a factor for ordered plot
    nice_name = fct_inorder(nice_name)
  )

# Save
write_csv(df5, file = "figures/figures_data/df5.csv")

# Plot latitudinal trend
p5 <- ggplot(df5, aes(x = value, y = lat)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(orientation = "y", se = FALSE, color = "dodgerblue") +
  labs(x = "", y = "Latitude") +
  facet_wrap(~nice_name, ncol = 4, scales = "free_x", strip.position = "bottom") +
  theme_classic() +
  theme(
    axis.title.x = element_blank(),
    strip.placement = "outside",
    strip.background = element_blank()
  )
p5
ggsave(p5, file = "figures/figure_5.png", width = 120, height = 120, units = "mm", bg = "white")


## Figure 6: correlations among predictors ----
#--------------------------------------------------------------------------#
load("data/03.all_data.Rdata")
# Build correlation matrix, using Spearman correlation
mat_corr <- cor(df_all |> select(prop_crustacea:mo_eve), method = "spearman")

# Generate nicer names
# Get current names
mat_names <- colnames(mat_corr)
# List of new names
new_names <- c(
  "Proportion crustacea",
  "Proportion rhizaria",
  "Proportion gelatinous",
  "Proportion mollusca",
  "Proportion other",
  "Abundance",
  "Biovolume",
  "Taxonomic richness",
  "Taxonomic diversity",
  "Taxonomic evenness",
  "Size spectrum slope",
  "Mean size",
  "Mean grey level",
  "Morphological richness",
  "Morphological diversity",
  "Morphological evenness"
)
colnames(mat_corr) <- new_names
rownames(mat_corr) <- new_names

# Save data
# Clean storage and save
write.csv(as.data.frame(mat_corr), file = "figures/figures_data/df6.csv")

# Compute significance
testRes <- cor.mtest(mat_corr, conf.level = 0.95)

# Plot correlation matrix
# Open a PNG device
png("figures/figure_6.png", width = 150, height = 150, units = "mm", res = 300, bg = "white")
corrplot(
  mat_corr,
  p.mat = testRes$p,
  sig.level = c(0.001, 0.01, 0.05),
  insig = 'label_sig',
  pch.cex = 0.9,
  method = "circle",
  type = "upper",
  diag = FALSE,
  tl.col = "black",
  tl.cex = 0.7,
  order = "original",
)
# Close the device
dev.off()

