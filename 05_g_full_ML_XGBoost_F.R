#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Machine Learning: eXtreme gradient boosting (xgboost) ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# In this file the eXtreme gradient boosting (xgboost) algorithm is tuned and
# fitted for the full model, i.e., one model for each food item. 
# The parameter tuning is conducted via a 5-fold Cross-Validation. 
# The following hyperparameters are considered::
# tree_depth: depth of a tree
## lower values make the model more robust to overfitting and less complex
# trees: number of trees
## higher values may lead to overfitting
# learn_rate: step size shrinkage, also called learning rate
## ranges from 0 to 1
## lower values make the model more robust to overfitting 
# mtry: randomly selected predictors at each spit
## small number of predictors is typically chosen if many predictors are correlated
# min_n: minimal node size
## larger values lead to smaller trees which are more robust to overfitting
# loss_reduction: minimum loss reduction (also called gamma)
# sample_size: proportion observations sampled.
# stop_iter: early stopping criterion; 
## no tuning parameter in the classical sense, 
## but set to 100 to save computation time. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# tidymodels framework is used for machine learning
if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)

# xgboost is used to fit the xgboost model
if (!require("xgboost")) install.packages("xgboost") 
library(xgboost)

# dplyr for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)

# stringr for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)


#### General Setup ####

# load machine learning data frame
df_ML_raw <- readRDS("output/final_ML.rds")

# shuffle data frame
set.seed(42)
random_rows <- sample(1:nrow(df_ML_raw), nrow(df_ML_raw), replace = FALSE)
df_ML_raw <- df_ML_raw[random_rows, ]

# ensure that no missing values and no character variables are present
sum(is.na(df_ML_raw))
## those four are: food, type, country, size (needed later for prediction analysis)
ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | 
                                   sapply(df_ML_raw, is.integer)])
## all columns integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer,as.numeric)
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)

# drop date column
df_ML_raw <- df_ML_raw %>% dplyr::select(-date)

# cross validation (5-fold CV)
k <- 5

# machine learning data frame
df_ML <- df_ML_raw
## for development
# df_ML <- df_ML_raw %>%
#   filter(fruits == 1) %>%
#   head(500) %>%
#   rbind(
#     df_ML_raw %>%
#       filter(fruits == 0) %>%
#       tail(500)
#   )



##%%%%%%%%%%%%%%%%%%%%%%##
#### Parameter Tuning ####
##%%%%%%%%%%%%%%%%%%%%%%##

# added 3 and round(ncol(df_ML_raw)/3)

# define parameter grid
## parameter values are chosen manually in order to have full control
xgb_grid <- expand.grid(
  # smaller values seem to be better
  tree_depth = c(3, 6), # 3, 6, 9
  trees = c(1500), # 100, 500, 700, 800, 1000, 1500
  learn_rate = c(0.01), # 0.03, 0.01, 0.001
  # higher values seem to be better
  mtry = c(round(ncol(df_ML_raw)/3), ncol(df_ML_raw) - 100, 
           ncol(df_ML_raw) - 50, ncol(df_ML_raw)), # 2, 5, 10, 20, 50, 100, 150, 160, 200, 480
  # smaller values seem to be better
  min_n = c(5, 20, 50) # 1, 5, 10, 10, 20, 30, 40, 50, 60
)

# create data frames which combine the results during the loop
xgb_best_param_final <- data.frame()
xgb_pred_final <- data.frame()
xgb_perf_final <- data.frame()
xgb_coef <- data.frame()

# different prices
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")
prices_2 <- "price_F"

# different distances
distances <- df_ML %>% dplyr::select(starts_with("distance")) %>% colnames()

# for each price, a single machine learning model is trained
for (price_sel in prices_2) {
  
  print(paste("Model for:", all_of(price_sel)))
  
  # to create folds, drop price variables not used in current model
  df_ML_price <- df_ML %>%
    dplyr::select(-c(all_of(c(prices[!prices %in% price_sel], "price_total"))))
  
  # drop all lagged price variables except the ones for the selected price
  prices_drop <- colnames(df_ML_price)[str_detect(
    colnames(df_ML_price), paste(paste0(c(prices[!prices %in% price_sel], 
                                          "price_total"), "_lag"), 
                                 collapse = "|"))]
  df_ML_price <- df_ML_price %>%
    dplyr::select(-c(all_of(prices_drop)))
  
  
  # moreover, only the distance variable for the current model should be
  # included.
  ## extract market letter
  market_letter <- str_to_lower(str_sub(price_sel, -1))
  ## identify distance variables to drop
  distances_drop <- distances[str_sub(distances, -1) != market_letter]
  ## drop distances
  df_ML_price <- df_ML_price %>%
    dplyr::select(-c(all_of(distances_drop)))
  
  
  # select predictors
  predictors <- df_ML_price %>% 
    dplyr::select(-c(all_of(price_sel), food, type, country, size)) %>% 
    colnames()
  
  # create folds
  ## stratify according to food
  ## food items are similarly distributes across train and test set
  fold_splits <- vfold_cv(df_ML_price, v = k, repeats = 1, seed = 42, 
                          strata = food)
  
  # iterate over folds
  for (fold in 1:length(fold_splits$splits)) {
    
    ## Extract Training and Test Data Set ##
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
    
    # extract training data
    df_train <- analysis(fold_splits$splits[[fold]]) %>%
      dplyr::select(-c(food, type, country, size))
    
    # extract testing data
    ## store food, type, country, size in extra test data
    df_test_food <- assessment(fold_splits$splits[[fold]])
    df_test <- assessment(fold_splits$splits[[fold]]) %>%
      dplyr::select(-c(food, type, country, size))
    
    
    ## Model Setup ##
    ##%%%%%%%%%%%%%##
    
    # define recipe
    xgb_recipe <- 
      # select training data set
      df_train %>%  
      recipe(.) %>%
      # price variable is outcome
      update_role({{price_sel}}, new_role = "outcome") %>%
      # all other variables are predictors
      update_role({{predictors}},
                  new_role = "predictor")
    
    
    ## Parameter Tuning ##
    ##%%%%%%%%%%%%%%%%%%##
    
    # create data frame to store results
    xgb_perf_tuning <- data.frame()
    
    # iterate over every penalty parameter
    for (tree_depth_sel in unique(xgb_grid$tree_depth)) {
      for (trees_sel in unique(xgb_grid$trees)) {
        for (learn_rate_sel in unique(xgb_grid$learn_rate)) {
          for (mtry_sel in unique(xgb_grid$mtry)) {
            for (min_n_sel in unique(xgb_grid$min_n)) {
              
              # specify model
              xgb_spec_tuning <- 
                boost_tree(tree_depth = tree_depth_sel, trees = trees_sel,
                           learn_rate = learn_rate_sel, mtry = mtry_sel,
                           min_n = min_n_sel, stop_iter = 100) %>%
                set_engine("xgboost") 
              
              # create final workflow
              xgb_workflow_tuning <- 
                workflow() %>%
                add_model(xgb_spec_tuning) %>%
                add_recipe(xgb_recipe)
              
              # fit the model on the training date
              xgb_fit_tuning <- 
                xgb_workflow_tuning %>%
                fit(df_train)
              
              # create data frame with predictions and true value
              xgb_pred_tuning <- data.frame(
                price_pred = predict(xgb_fit_tuning, df_test) %>% 
                  dplyr::select(.pred) %>% pull(),
                price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull()
              )
              
              # create data frame with RMSE and penalty
              xgb_perf_tuning_round <- data.frame(
                RMSE = rmse(xgb_pred_tuning, 
                            truth = price_true, estimate = price_pred) %>%
                  dplyr::select(.estimate) %>% pull(),
                MAE = mae(xgb_pred_tuning,
                          truth = price_true, estimate = price_pred) %>%
                  dplyr::select(.estimate) %>% pull(),
                MAPE = mape(xgb_pred_tuning,
                            truth = price_true, estimate = price_pred) %>%
                  dplyr::select(.estimate) %>% pull(),
                tree_depth = tree_depth_sel,
                trees = trees_sel,
                learn_rate = learn_rate_sel,
                mtry = mtry_sel,
                min_n = min_n_sel
              )
              
              xgb_perf_tuning <- rbind(xgb_perf_tuning, xgb_perf_tuning_round)
            }
          }
        }
        
      }
      
    }
    
    
    # select parameter combination with smallest RMSE
    xgb_best_param <- xgb_perf_tuning %>%
      filter(RMSE == min(RMSE)) %>% 
      head(1)
    xgb_best_param$fold <- fold
    xgb_best_param$model <- price_sel
    
    # fit model again 
    xgb_spec_best <- 
      boost_tree(tree_depth = xgb_best_param$tree_depth, 
                 trees = xgb_best_param$trees,
                 learn_rate = xgb_best_param$learn_rate, 
                 mtry = xgb_best_param$mtry,
                 min_n = xgb_best_param$min_n,
                 stop_iter = 100) %>%
      set_engine("xgboost") 
    
    xgb_workflow_best <- 
      workflow() %>%
      add_model(xgb_spec_best) %>%
      add_recipe(xgb_recipe)
    
    xgb_fit_best <- 
      xgb_workflow_best %>%
      fit(df_train)
    
    # make predictions
    xgb_pred_best <- data.frame(
      price = price_sel, 
      price_pred = predict(xgb_fit_best, df_test) %>% 
        dplyr::select(.pred) %>% pull(),
      price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull(),
      food = df_test_food$food,
      type = df_test_food$type,
      size = df_test_food$size,
      country = df_test_food$country
    )
    
    # evaluate performance
    xgb_perf_best <- data.frame(
      RMSE = rmse(xgb_pred_best, 
                  truth = price_true, estimate = price_pred) %>%
        dplyr::select(.estimate) %>% pull(),
      MAE = mae(xgb_pred_best,
                truth = price_true, estimate = price_pred) %>%
        dplyr::select(.estimate) %>% pull(),
      MAPE = mape(xgb_pred_best,
                  truth = price_true, estimate = price_pred) %>%
        dplyr::select(.estimate) %>% pull(),
      tree_depth = xgb_best_param$tree_depth,
      trees = xgb_best_param$trees,
      learn_rate = xgb_best_param$learn_rate,
      mtry = xgb_best_param$mtry,
      min_n = xgb_best_param$min_n
    )
    
    # rbind
    xgb_best_param_final <- rbind(xgb_best_param_final, xgb_best_param)
    xgb_perf_final <- rbind(xgb_perf_final, xgb_perf_best)
    xgb_pred_final <- rbind(xgb_pred_final, xgb_pred_best)
    
    # track progress
    print(paste("Fold:", fold))
  }
}


saveRDS(xgb_best_param_final, 
        "models/XGBoost/perf_results/XGBoost_perf_full_model_F.rds")
#saveRDS(xgb_perf_final, "xgb_perf_result.rds")
saveRDS(xgb_pred_final, 
        "models/XGBoost/pred_results/XGBoost_pred_full_model_F.rds")