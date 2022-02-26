
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Machine Learning: Random Forest ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# In this file, the machine learning algorithm "Random Forest" is applied to predict prices.
# Random Forests is a very powerful ensemble method as they use bagged ensembles of decision trees with random
# feature selection to add diversity to the model.

# There only weaknesses are their lack of interpretability as well as long computation times and the relative complexity
# to tune them. However, in our task, interpretability of the model is off second-concern, and computation complexity is
# reduced by performing all the predictions on the bwUniCluster 2.0.

# There are mainly three parameters to tune:

# mtry: the optimal parameter specifying the number of features to randomly select at each split. For regression problems,
# the rule of thumb here is the number of predictors divided by 3. However, because of the big data computational restrictions
# we will iteratively and manually tune the parameter by first giving a small grid search for the training function and get the results
# from the cluster and then again change the grid search. With each iteration, we come closer to the optimal parameter
# while bounding with the computational restrictions.

# ntree: This parameter specifies the number of trees to grow. Some authors and studies mentioned that this is not a "real hyperparamater",
# however, in this prediction task, we will vary it to see whether this can further boost performance and lower the RMSE.
# Some authors suggest to start with 1000 trees as a "rule of thumb" and even higher values.

# min_n: This parameter stands for the minimum number of data points in a node that are required for the node to be split further.
# For the "first iteration", we start with 1 and 5 and again depending on the results that we get from the cluster, adjust the 
# optimal value of min_n.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# tidymodels framework is used for machine learning
if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)

# RandomForest package is used to fit the Random Forest model
if (!require("randomForest")) install.packages("randomForest") 
library(randomForest)

# Dplyr package is used for data transformations
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)

if (!require("stringr")) install.packages("stringr") 
library(stringr)


#### General Setup ####

# load machine learning data frame
df_ML_raw <- readRDS("output/final_ML.rds")

# shuffle data frame
set.seed(42)
random_rows <- sample(1:nrow(df_ML_raw), nrow(df_ML_raw), replace = FALSE)

# ensure that no missing values and no character variables are present
sum(is.na(df_ML_raw))
## those four are: food, type, country, size (needed later for prediction analysis)
ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | sapply(df_ML_raw, is.integer)])
## all columns integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer,as.numeric)
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)

# drop date column
df_ML_raw <- df_ML_raw %>% dplyr::select(-date)

# cross validation (5-fold CV)
k <- 2  # 5 
# Change to 2-fold cross validation for faster coomputation

# machine learning data frame

# Drop all non-related prices before ML prediction

df_ML_price <- df_ML_raw %>%
  select(!contains("price_F") & !contains("price_H") & !contains("price_M") &
           !contains("price_B") & !contains("price_total") & !contains('distance_f') &
           !contains('distance_h') & !contains('distance_m') & !contains('distance_b')) # all food

##%%%%%%%%%%%%%%%%%%%%%%##
#### Parameter Tuning ####
##%%%%%%%%%%%%%%%%%%%%%%##

# define parameter grid
## parameter values are chosen manually in order to have full control
rf_grid <- expand.grid(
  trees = c(1000),
  mtry = c(100),  #  round(ncol(df_ML_raw)/3)
  #mtry = c(1:2),
  min_n = c(100)
)

# create data frames which combine the results during the loop
rf_best_param_final <- data.frame()
rf_pred_final <- data.frame()
rf_perf_final <- data.frame()
rf_coef <- data.frame()

# different prices
prices <- c("price_K") 
#"price_F","price_K", "price_M", "price_B")

# for each price, a single machine learning model is trained
for (price_sel in prices) {
  
  print(paste("Model for:", all_of(price_sel)))
  
  # to create folds, drop price variables not used in current model
  #df_ML_price <- df_ML %>%
  #dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
  
  # drop all lagged price variables except the ones for the selected price
  #prices_drop <- colnames(df_ML_price)[str_detect(
  #colnames(df_ML_price), paste(paste0(c(prices[!prices %in% price_sel], 
  # "price_total"), "_lag"), 
  # collapse = "|"))]
  #df_ML_price <- df_ML_price %>%
  #dplyr::select(-c(all_of(prices_drop)))
  
  # select predictors
  predictors <- df_ML_price %>% 
    dplyr::select(-c(price_K, food, type, country, size)) %>% 
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
    rf_recipe <- 
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
    rf_perf_tuning <- data.frame()
    
    # iterate over every penalty parameter
    for (trees_sel in unique(rf_grid$trees)) {
      for (mtry_sel in unique(rf_grid$mtry)) {
        for (min_n_sel in unique(rf_grid$min_n)) {
          
          # specify model
          rf_spec_tuning <- 
            rand_forest(trees = trees_sel, mtry = mtry_sel,
                        min_n = min_n_sel) %>%
            set_engine("randomForest") 
          
          # create final workflow
          rf_workflow_tuning <- 
            workflow() %>%
            add_model(rf_spec_tuning) %>%
            add_recipe(rf_recipe)
          
          # fit the model on the training date
          rf_fit_tuning <- 
            rf_workflow_tuning %>%
            fit(df_train)
          
          # create data frame with predictions and true value
          rf_pred_tuning <- data.frame(
            price_pred = predict(rf_fit_tuning, df_test) %>% 
              dplyr::select(.pred) %>% pull(),
            price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull()
          )
          
          # create data frame with RMSE and penalty
          rf_perf_tuning_round <- data.frame(
            RMSE = rmse(rf_pred_tuning, 
                        truth = price_true, estimate = price_pred) %>%
              dplyr::select(.estimate) %>% pull(),
            MAE = mae(rf_pred_tuning,
                      truth = price_true, estimate = price_pred) %>%
              dplyr::select(.estimate) %>% pull(),
            MAPE = mape(rf_pred_tuning,
                        truth = price_true, estimate = price_pred) %>%
              dplyr::select(.estimate) %>% pull(),
            trees = trees_sel,
            mtry = mtry_sel,
            min_n = min_n_sel
          )
          
          rf_perf_tuning <- rbind(rf_perf_tuning, rf_perf_tuning_round)
        }
      }
    }
    
    
    # select parameter combination with smallest RMSE
    rf_best_param <- rf_perf_tuning %>%
      filter(RMSE == min(RMSE)) %>% 
      head(1)
    rf_best_param$fold <- fold
    rf_best_param$model <- price_sel
    
    # fit model again 
    rf_spec_best <- 
      rand_forest(trees = rf_best_param$trees, mtry = rf_best_param$mtry,
                  min_n = rf_best_param$min_n) %>%
      set_engine("randomForest") 
    
    rf_workflow_best <- 
      workflow() %>%
      add_model(rf_spec_best) %>%
      add_recipe(rf_recipe)
    
    rf_fit_best <- 
      rf_workflow_best %>%
      fit(df_train)
    
    # make predictions
    rf_pred_best <- data.frame(
      price = price_sel, 
      price_pred = predict(rf_fit_best, df_test) %>% 
        dplyr::select(.pred) %>% pull(),
      price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull(),
      food = df_test_food$food,
      type = df_test_food$type,
      size = df_test_food$size,
      country = df_test_food$country
    )
    
    # evaluate performance
    rf_perf_best <- data.frame(
      RMSE = rmse(rf_pred_best, 
                  truth = price_true, estimate = price_pred) %>%
        dplyr::select(.estimate) %>% pull(),
      MAE = mae(rf_pred_best,
                truth = price_true, estimate = price_pred) %>%
        dplyr::select(.estimate) %>% pull(),
      MAPE = mape(rf_pred_best,
                  truth = price_true, estimate = price_pred) %>%
        dplyr::select(.estimate) %>% pull(),
      trees = rf_best_param$trees,
      mtry = rf_best_param$mtry,
      min_n = rf_best_param$min_n
    )
    
    # rbind
    rf_best_param_final <- rbind(rf_best_param_final, rf_best_param)
    rf_perf_final <- rbind(rf_perf_final, rf_perf_best)
    rf_pred_final <- rbind(rf_pred_final, rf_pred_best)
    
    # track progress
    print(paste("Fold:", fold))
  }
}


saveRDS(rf_best_param_final, "rf_tuning_result.rds")
saveRDS(rf_perf_final, "rf_perf_result.rds")
saveRDS(rf_pred_final, "rf_pred_result.rds")



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#### Final Machine Learning Model ####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##


# from parameter tuning prediction result, we choose the parameter combination
# that leads to the smallest RMSE
# we do so for each model
rf_final_param <- rf_best_param_final %>%
  group_by(model) %>%
  summarise(RMSE_min = min(RMSE)) %>%
  inner_join(
    rf_best_param_final, by = c("model", "RMSE_min" = "RMSE")
  )

# train final model using the best parameter combination on full data set # 
# do so for each model #
for (price_sel in prices) {
  
  print(paste("Train final model for", price_sel))
  
  # subset data set - drop prices not used
  #df_ML_price <- df_ML %>%
  #dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
  
  # drop all lagged price variables except the ones for the selected price
  #prices_drop <- colnames(df_ML_price)[str_detect(
  #colnames(df_ML_price), paste(paste0(c(prices[!prices %in% price_sel], 
  #"price_total"), "_lag"), 
  #collapse = "|"))]
  #df_ML_price <- df_ML_price %>%
  #dplyr::select(-c(all_of(prices_drop)))
  
  # select predictors
  predictors <- df_ML_price %>% 
    dplyr::select(-c(price_K, food, type, country, size)) %>% 
    colnames()
  
  # define recipe
  rf_recipe <- 
    # select full data set
    df_ML_price %>%  
    recipe(.) %>%
    # price variable is outcome
    update_role({{price_sel}}, new_role = "outcome") %>%
    # all other variables are predictors
    update_role({{predictors}},
                new_role = "predictor")
  
  # specify final model
  ## select parameters
  rf_params_final <- rf_final_param %>% 
    filter(model == all_of(price_sel)) %>% 
    dplyr::select(trees, mtry, min_n) 
  ## model specification
  rf_spec_final <- 
    rand_forest(trees = rf_params_final$trees, mtry = rf_params_final$mtry,
                min_n = rf_params_final$min_n) %>%
    set_engine("randomForest") 
  
  # create final workflow
  rf_workflow_final <-
    workflow() %>%
    add_model(rf_spec_final) %>%
    add_recipe(rf_recipe)
  
  # fit the model on the full data set
  rf_fit <-
    rf_workflow_final %>%
    fit(df_ML_price)
  
  # save model to make predictions for next week in R Shiny App
  save_path <- paste0("RandomForest_model_", price_sel, ".rds")
  saveRDS(rf_fit, save_path)
}