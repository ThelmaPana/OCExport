#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Fit a XGBoost model to predict POC values
# Date: 02/01/2024
# Author: Thelma Panaiotis
#--------------------------------------------------------------------------#

source("utils.R")
load(file.path(data_dir, "03.all_data.Rdata"))

## Sample 1000 points ----
#--------------------------------------------------------------------------#
set.seed(12)
df <- df %>% drop_na(poc) %>% sample_n(1000)


## Split data ----
#--------------------------------------------------------------------------#
# Train VS test, stratified
df_split <- initial_split(df, prop = 0.8, strata = poc)
df_train <- training(df_split)
df_test <- testing(df_split)

# Explore training data
df_train %>% ggplot() + geom_histogram(aes(x = temperature_mean))
df_train %>% ggplot() + geom_histogram(aes(x = poc))

# Cross-validation, 10 folds, stratified
df_folds <- vfold_cv(df_train, v = 10, strata = poc)


## Model ----
#--------------------------------------------------------------------------#
# Define a xgboost model with hyperparameters to tune
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  #loss_reduction = tune(),
  #sample_size = tune(),
  #mtry = tune(),
  learn_rate = tune()
) %>%
  set_mode("regression") %>%
  set_engine("xgboost", num.threads = n_cores)
extract_parameter_set_dials(xgb_spec)

## Workflow ----
#--------------------------------------------------------------------------#
xgb_wflow <- workflow() %>%
  add_formula(poc ~ . - lon - lat) %>%
  add_model(xgb_spec)


## Gridsearch ----
#--------------------------------------------------------------------------#
# Define a filling grid
xgb_grid <- grid_latin_hypercube(
  trees(range = c(100, 2000)),
  learn_rate(range = c(0.01, 0.1), trans = NULL),
  tree_depth(range = c(2, 8)),
  min_n(),
  #loss_reduction(),
  #sample_size = sample_prop(),
  #finalize(mtry(), df_train),
  size = 30
)
xgb_grid


# Tune the grid to find best hyperparameters
doParallel::registerDoParallel()
set.seed(12)
xgb_res <- tune_grid(
  xgb_wflow,
  resamples = df_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
autoplot(xgb_res)

collect_metrics(xgb_res)

# Select best hyperparameters according to rmse
collect_metrics(xgb_res) %>%
  filter(.metric == "rmse") %>%
  pivot_longer(colnames(xgb_grid), names_to = "parameter") %>%
  ggplot() +
  geom_point(aes(x = value, y = mean, color = parameter)) +
  scale_y_reverse() +
  facet_wrap(~parameter, scales = "free_x")

collect_metrics(xgb_res)

show_best(xgb_res)
best_xgb <- select_best(xgb_res)


## Finalize ----
#--------------------------------------------------------------------------#
# Finalize workflow with best hyperparameters
final_xgb <- finalize_workflow(
  xgb_wflow,
  best_xgb
)

# Last fit on training data, also predicts test data
final_res <- last_fit(final_xgb, df_split)


## Evaluate ----
#--------------------------------------------------------------------------#
# Metrics
collect_metrics(final_res)

# Prediction VS truth
final_res %>%
  collect_predictions() %>%
  ggplot() +
  geom_point(aes(x = poc, y = .pred)) +
  geom_abline(slope = 1, color = "red")

# Variable importance
final_res %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)
