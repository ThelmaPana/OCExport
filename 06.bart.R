#--------------------------------------------------------------------------#
# Project: OCExport
# Script purpose: Purpose
# Date: 02/01/2024
# Author: Thelma Panaiotis
#--------------------------------------------------------------------------#

source("utils.R")
load(file.path(data_dir, "03.all_data.Rdata"))


## Prepare data ----
#--------------------------------------------------------------------------#
df <- df %>% drop_na() %>% select(-c(lon, lat))


## Sample 1000 points ----
#--------------------------------------------------------------------------#
set.seed(12)
df %>% ggplot() + geom_histogram(aes(x = poc))
df <- df %>% sample_n(1000)


## Split data ----
#--------------------------------------------------------------------------#
# Train VS test, stratified
#set.seed(12)
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
# Define a bart model with hyperparameters to tune
bart_spec <- parsnip::bart(
  trees = tune(),
  prior_terminal_node_coef = tune(),
  prior_terminal_node_expo = tune()
) %>%
  set_engine("dbarts") %>%
  set_mode("regression")
extract_parameter_set_dials(bart_spec)


## Workflow ----
#--------------------------------------------------------------------------#
bart_wflow <- workflow() %>%
  add_formula(poc ~ .) %>%
  add_model(bart_spec)


## Gridsearch ----
#--------------------------------------------------------------------------#
# Define a filling grid
bart_grid <- grid_latin_hypercube(
  trees(),
  prior_terminal_node_coef(),
  prior_terminal_node_expo(),
  size = 5
)
bart_grid


# Tune the grid to find best hyperparameters
set.seed(12)
bart_res <- tune_grid(
  bart_wflow,
  resamples = df_folds,
  grid = bart_grid,
  control = control_grid(save_pred = TRUE)
)
autoplot(bart_res)

collect_metrics(bart_res)

# Select best hyperparameters according to rmse
collect_metrics(bart_res) %>%
  filter(.metric == "rmse") %>%
  pivot_longer(colnames(bart_grid), names_to = "parameter") %>%
  ggplot() +
  geom_point(aes(x = value, y = mean, color = parameter)) +
  scale_y_reverse() +
  facet_wrap(~parameter, scales = "free_x")

collect_metrics(bart_res)

show_best(bart_res, "rmse")
show_best(bart_res, "rsq")
best_bart <- select_best(bart_res, "rmse")


## Finalize ----
#--------------------------------------------------------------------------#
# Finalize workflow with best hyperparameters
final_bart <- finalize_workflow(
  bart_wflow,
  best_bart
)

# Last fit on training data, also predicts test data
#final_res <- last_fit(final_bart, df_split)
final_res <- fit(final_bart, df_train)
preds <- augment(final_res, new_data = df_test)


## Evaluate ----
#--------------------------------------------------------------------------#
# Metrics
rmse(preds, truth = poc, estimate = .pred)
rsq(preds, truth = poc, estimate = .pred)

# Prediction VS truth
preds %>%
  ggplot() +
  geom_point(aes(x = poc, y = .pred)) +
  geom_abline(slope = 1, color = "red")

## Model explanation ----
#--------------------------------------------------------------------------#
# Generate an explaner
bart_explainer <- explain_tidymodels(
  final_res,
  data = df_train %>% select(-c(poc)),
  y = df_train$poc,
  label = "BART"
)

# Variable importance
vip <- model_parts(
  explainer = bart_explainer,
  loss_function = loss_root_mean_square,
  B = 50,
  type = "difference"
)

plot(vip) + ggtitle("Mean variable-importance over 50 permutations", "")


# Partial dependence plots
n_plot <- 3 # number of variables to plot
# Get names of most important variables
vars <- vip %>%
  as_tibble() %>%
  filter(!str_starts(variable, "_")) %>%
  group_by(variable) %>%
  summarise(dropout_loss = mean(dropout_loss)) %>%
  ungroup() %>%
  arrange(desc(dropout_loss)) %>%
  slice(1:n_plot) %>%
  pull(variable)

pdp <- model_profile(
  explainer = bart_explainer,
  type = "partial",
  variables = vars
)
plot(pdp , variables = vars) + ggtitle("Partial dependance plots", "")



# tweedie
#https://www.simoncoulombe.com/2023/08/index.en-us/#tweedie-regression
#https://www.kaggle.com/code/olehmezhenskyi/tweedie-xgboost

# var importance and partial plots
#https://advanced-ds-in-r.netlify.app/posts/2021-03-24-imlglobal/
