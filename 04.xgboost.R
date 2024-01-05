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
#set.seed(12)
#df <- df %>% drop_na(poc) %>% sample_n(1000)


## Inspect data ----
#--------------------------------------------------------------------------#
df %>% ggplot() + geom_histogram(aes(x = temperature_mean))
df %>% ggplot() + geom_histogram(aes(x = poc), bins = 100)
df %>% ggplot() + geom_histogram(aes(x = poc), bins = 100) + scale_x_continuous(trans = "log")
# Log-transformation for poc is not too bad
df <- df %>% mutate(poc_log = log(poc),.after = poc)


## Split data ----
#--------------------------------------------------------------------------#
# Train VS test, stratified
df_split <- initial_split(df, prop = 0.8, strata = poc_log)
df_train <- training(df_split)
df_test <- testing(df_split)

# Cross-validation, 10 folds, stratified
df_folds <- vfold_cv(df_train, v = 10, strata = poc_log)


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

# Dummy model
# xgb_spec <- boost_tree(
#   trees = 500,
#   tree_depth = 4,
#   min_n = 10,
#   #loss_reduction = tune(),
#   #sample_size = tune(),
#   #mtry = tune(),
#   learn_rate = 0.01
# ) %>%
#   set_mode("regression") %>%
#   set_engine("xgboost", num.threads = n_cores)


## Recipe ----
#--------------------------------------------------------------------------#
xgb_rec <- recipe(poc_log ~ ., data = df_train) %>%
  update_role(lon, lat, new_role = "coords") %>% # These variables can be retained in the data but not included in the model
  update_role(poc, new_role = "untransformed outcome")
summary(xgb_rec)


## Workflow ----
#--------------------------------------------------------------------------#
xgb_wflow <- workflow() %>%
  add_recipe(xgb_rec) %>%
  add_model(xgb_spec)


## Gridsearch ----
#--------------------------------------------------------------------------#
# Define a filling grid
# xgb_grid <- grid_latin_hypercube(
#   trees(range = c(100, 2000)),
#   learn_rate(range = c(0.01, 0.1), trans = NULL),
#   tree_depth(range = c(2, 8)),
#   min_n(),
#   #loss_reduction(),
#   #sample_size = sample_prop(),
#   #finalize(mtry(), df_train),
#   size = 30
# )
xgb_grid <- grid_latin_hypercube(
  trees(),
  learn_rate(),
  tree_depth(),
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

# Last fit on training data
#final_res <- last_fit(final_xgb, df_split)
final_res <- fit(final_xgb, df_train)
# Predict test data, and apply exp transformation
preds <- augment(final_res, new_data = df_test) %>%
  rename(.pred_logged = .pred) %>%
  mutate(.pred = exp(.pred_logged))

# Dummy model
#res <- fit(xgb_wflow, df_train)
#preds <- augment(res, new_data = df_test) %>%
#  rename(.pred_logged = .pred) %>%
#  mutate(.pred = exp(.pred_logged))


## Evaluation ----
#--------------------------------------------------------------------------#
# Metrics
# Metrics
rmse(preds, truth = poc_log, estimate = .pred_logged)
rsq(preds, truth = poc_log, estimate = .pred_logged)

# Prediction VS truth
preds %>%
  ggplot() +
  geom_point(aes(x = poc_log, y = .pred_logged)) +
  geom_abline(slope = 1, color = "red") +
  labs(title = "Pred VS truth in log-transformed space")

preds %>%
  ggplot() +
  geom_point(aes(x = poc, y = .pred)) +
  geom_abline(slope = 1, color = "red") +
  labs(title = "Pred VS truth")

# Residuals on test set
preds %>%
  mutate(residuals = .pred_logged - poc_log) %>%
  ggplot() +
  geom_density(aes(x = residuals)) +
  labs(x = "Residuals")



## Interpretation ----
#--------------------------------------------------------------------------#
# Select only predictors
vip_train <- xgb_rec %>% prep() %>% bake(new_data = NULL, all_predictors())


# Explainer
xgb_explain <-
  explain_tidymodels(
    model = extract_fit_parsnip(final_res),
    data = vip_train,
    y = df_train %>%  pull(poc_log)
  )


# Variable importance
xgb_var_imp <- model_parts(xgb_explain)
ggplot_imp(xgb_var_imp)

# Partial dependence plots
xgb_pdp_temp <- model_profile(explainer = xgb_explain, variables = c("temperature_mean"))
ggplot_pdp(xgb_pdp_temp, temperature_mean) +
  labs(x = "Temperature_mean", y = "Logged predicted POC")

xgb_pdp_phos <- model_profile(explainer = xgb_explain, variables = c("phosphate_mean"))
ggplot_pdp(xgb_pdp_phos, phosphate_mean) +
  labs(x = "Phosphate_mean", y = "Logged predicted POC")

xgb_pdp_nit <- model_profile(explainer = xgb_explain, variables = c("nitrate_mean"))
ggplot_pdp(xgb_pdp_nit, nitrate_mean) +
  labs(x = "Nitrate_mean", y = "Logged predicted POC")
