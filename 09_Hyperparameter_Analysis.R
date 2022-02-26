#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## HYPERPARAMETER ANALYSIS ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this file the best parameter combination for both the full and food model
# is determined for each algorithm by using the smallest RMSE.
# The results of this analysis are illustrated in the R Shiny App.
# Note that all variables in the data frame are stored as character variables.
# This makes displaying the hyperparameters value in the table more easily and
# nicer as the number of digits do not need to be specified. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### LASSO ####
#%%%%%%%%%%%%%#

# full model
params_lasso_full <- 
  readRDS("models/LASSO/perf_results/LASSO_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, penalty) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(
    penalty = as.character(sprintf('%1.10f', penalty))
  ) %>%
  dplyr::rename(Lambda = penalty)

saveRDS(params_lasso_full,
        "models/LASSO/hyperparameters/hyperparameters_full_model.rds")


# food model
params_lasso_food <-
  readRDS("models/LASSO/perf_results/LASSO_perf_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, penalty) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(
    penalty = as.character(sprintf('%1.10f', penalty))
  ) %>%
  dplyr::rename(Lambda = penalty)

saveRDS(params_lasso_food,
        "models/LASSO/hyperparameters/hyperparameters_food_model.rds")




#### RIDGE ####
#%%%%%%%%%%%%%#

# full model
params_ridge_full <- 
  readRDS("models/RIDGE/perf_results/RIDGE_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, penalty) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(
    penalty = as.character(sprintf('%1.10f', penalty))
  ) %>%
  dplyr::rename(Lambda = penalty)

saveRDS(params_ridge_full,
        "models/RIDGE/hyperparameters/hyperparameters_full_model.rds")

# food model
params_ridge_food <-
  readRDS("models/RIDGE/perf_results/RIDGE_perf_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, penalty) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(
    penalty = as.character(sprintf('%1.10f', penalty))
  ) %>%
  dplyr::rename(Lambda = penalty)

saveRDS(params_ridge_food,
        "models/RIDGE/hyperparameters/hyperparameters_food_model.rds")



#### ENET ####

# full model
params_enet_full <- 
  readRDS("models/ENET/perf_results/ENET_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, penalty, mixture) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(
    penalty = as.character(sprintf('%1.10f', penalty)), 
    mixture = as.character(sprintf('%1.10f', mixture))
  ) %>%
  dplyr::rename(Lambda = penalty, Alpha = mixture)

saveRDS(params_enet_full,
        "models/ENET/hyperparameters/hyperparameters_full_model.rds")

# food model
params_enet_food <-
  readRDS("models/ENET/perf_results/ENET_perf_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, penalty, mixture) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(
    penalty = as.character(sprintf('%1.10f', penalty)),
    mixture = as.character(sprintf('%1.10f', mixture))
  ) %>%
  dplyr::rename(Lambda = penalty, Alpha = mixture)

saveRDS(params_enet_food,
        "models/ENET/hyperparameters/hyperparameters_food_model.rds")


#### XGBoost ####

# full model
params_xgb_full <-
  readRDS("models/XGBoost/perf_results/XGBoost_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, tree_depth, trees, learn_rate, mtry, min_n) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character)) %>%
  dplyr::rename(
    `Tree Depth` = tree_depth,
    `Learning Rate` = learn_rate,
    `Min_n` = min_n,
    `Trees` = trees,
    `Mtry` = mtry
  )

saveRDS(params_xgb_full,
        "models/XGBoost/hyperparameters/hyperparameters_full_model.rds")

# food model
params_xgb_food <-
  readRDS("models/XGBoost/perf_results/XGBoost_perf_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, tree_depth, trees, learn_rate, mtry, min_n) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character)) %>%
  dplyr::rename(
    `Tree Depth` = tree_depth,
    `Learning Rate` = learn_rate,
    `Min_n` = min_n,
    `Trees` = trees,
    `Mtry` = mtry
  )

saveRDS(params_xgb_food,
        "models/XGBoost/hyperparameters/hyperparameters_food_model.rds")


#### Random Forests ####

# full model
params_rf_full <-
  readRDS("models/Random_Forest/perf_results/Random_Forest_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, trees, mtry, min_n) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character)) %>%
  dplyr::rename(
    `Min_n` = min_n,
    `Trees` = trees,
    `Mtry` = mtry
  )

saveRDS(params_rf_full,
        "models/Random_Forest/hyperparameters/hyperparameters_full_model.rds")


# food model
params_rf_food <-
  readRDS("models/Random_Forest/perf_results/Random_Forest_perf_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, trees, mtry, min_n) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character)) %>%
  dplyr::rename(
    `Min_n` = min_n,
    `Trees` = trees,
    `Mtry` = mtry
  )

saveRDS(params_rf_food,
        "models/Random_Forest/hyperparameters/hyperparameters_food_model.rds")


#### SVM ####

# full model
params_svm_full <- 
  readRDS("models/SVM/perf_results/SVM_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, Sigma, C) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character))

saveRDS(params_svm_full,
        "models/SVM/hyperparameters/hyperparameters_full_model.rds")



# food model
params_svm_food <-
  readRDS("models/SVM/perf_results/SVM_tuning_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, sigma, C) %>%
  ungroup() %>%
  dplyr::rename(market = model, Sigma = sigma) %>%
  mutate(across(everything(), as.character))

saveRDS(params_svm_food,
        "models/SVM/hyperparameters/hyperparameters_food_model.rds")



#### Neural Network ####

# full model
params_nn_full <- 
  readRDS("models/NN/perf_results/NN_perf_full_model.rds") %>%
  group_by(model) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(model, layer1, layer2, layer3) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character)) %>%
  dplyr::rename(
    `First Layer` = layer1,
    `Second Layer` = layer2,
    `Third Layer` = layer3
  )

saveRDS(params_nn_full,
        "models/NN/hyperparameters/hyperparameters_full_model.rds")



# food model
params_nn_food <-
  readRDS("models/NN/perf_results/NN_tuning_food_model.rds") %>%
  group_by(model, food) %>%
  filter(RMSE == min(RMSE)) %>%
  dplyr::select(food, model, layer1, layer2, layer3) %>%
  ungroup() %>%
  dplyr::rename(market = model) %>%
  mutate(across(everything(), as.character)) %>%
  dplyr::rename(
    `First Layer` = layer1,
    `Second Layer` = layer2,
    `Third Layer` = layer3
  )

saveRDS(params_nn_food,
        "models/NN/hyperparameters/hyperparameters_food_model.rds")
