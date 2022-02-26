#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Machine Learning: Elastic Net ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# In this file the Elastic Net (enet) regression is tuned and fitted for each food type
# separately for the whole sale market Frankfurt.
# For parameter tuning a 5-fold CV is conducted. The enet regression has two tuning parameters:
# penalty (also called lambda): amount of shrinkage
# mixture (also called alpha): importance on the l2 vs l1 norm 
# In this file: 0 < micture() < 1; otherwise LASSO or elastic net is selected
# To tune the lambda/penalty and mixture/alpha parameters a random grid of
# 900 values is selected. 

# Important notes which may be considered for pre-processing the data:
# - Enet can only handle numeric and integer variables, i.e., no character, date etc.
# - Enet cannot handle missing values
# - To ensure that all variables are on the same scale and therefore the model fit
# is independent on the scale on which the features are measured, all features
# are standardized to have zero mean and unit variance.
# Note: According to Tibshirani (1997) also dummy variables should be standardized.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# tidymodels framework is used for machine learning
if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)

# glmnet is used to fit the ENET regression
if (!require("glmnet")) install.packages("glmnet") 
library(glmnet)

# dplyr for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)

# stringr for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)

# for dummy variable creation
if (!require("fastDummies")) install.packages("fastDummies") 
library(fastDummies)

#### General Setup ####

# load machine learning data frame
df_ML_raw <- readRDS("output/final_food_ML.rds")

# ensure that no missing values and no character variables are present
sum(is.na(df_ML_raw))
## those four are: food, type, country, size (needed later for prediction analysis)
ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | sapply(df_ML_raw, is.integer)])
## all columns integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer,as.numeric)
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)

# cross validation (5-fold CV)
k <- 5

# machine learning data frame
df_ML <- df_ML_raw
  ## for development
#df_ML <- df_ML_raw %>% filter(food %in% c("Strawberry", "Kiwi", "Cauliflower"))


##%%%%%%%%%%%%%%%%%%%%%%##
#### Parameter Tuning ####
##%%%%%%%%%%%%%%%%%%%%%%##

# define parameter grid
  ## 30 random values for penalty() and mixture()
  ## this results in a grid of 900 combinations
  ## mixture(range(0.0001, 0.999)
ENET_grid <- grid_regular(penalty(), mixture(range(0.1, 0.9)), 
                          levels = 30)

# create data frames which combine the results during the loop
ENET_best_param_final <- data.frame()
ENET_perf_final <- data.frame()
ENET_pred_final <- data.frame()

# different prices
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")
prices_2 <- "price_F"

# different distances
distances <- df_ML %>% dplyr::select(starts_with("distance")) %>% colnames()

# iterate over food
for (food_sel in unique(df_ML$food)) {
  # print model
  print(paste0("Model for ", food_sel))
  
  # filter food
  df_ML_food <- df_ML_raw %>% filter(food == food_sel)
  
  # shuffle data frame
  set.seed(42)
  random_rows <- sample(1:nrow(df_ML_food), nrow(df_ML_food), replace = FALSE)
  df_ML_food <- df_ML_food[random_rows, ]
  
  # iterate over prices (for each price, a single machine learning model is trained)
  for (price_sel in prices_2) {
    
    # to create folds, drop price variables not used in current model
    df_ML_price <- df_ML_food %>%
      dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
    
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
    
    # create dummy variables using type and country and size
    df_ML_price[, c("type", "country", "size")] <- 
      lapply(df_ML_price[, c("type", "country", "size")], as.factor)
    
    df_ML_price <- dummy_cols(
      df_ML_price,
      remove_first_dummy = FALSE,
      remove_selected_columns = FALSE, 
      select_columns = c("type", "country", "size")
    )
    
    # select predictors
    predictors <- df_ML_price %>% 
      dplyr::select(-c(all_of(price_sel), food, type, country, size, date)) %>% 
      colnames()
    
    # standardize features to mean zero and unit variance
    ## very important for regularization because regularization solutions
    ## are not equivariant under scaling of the inputs
    ## This ensures that all predictors are on the same scale and therefore the 
    ## model fit is independent on the scale on which the features are measured.
    df_ML_price <- df_ML_price %>%
      recipe(.) %>%
      # price variable is outcome
      update_role({{price_sel}}, new_role = "outcome") %>%
      # all other variables are predictors
      update_role({{predictors}},
                  new_role = "predictor") %>%
      # standardize all predictors
      step_normalize(all_predictors()) %>%
      # estimate parameters
      prep() %>%
      # apply standardization to full data set
      bake(new_data = NULL) %>%
      # factors as characters
      mutate_if(is.factor, as.character)
    
    # constant variables have NA after standardization
    # hence, we drop them 
    drop_constant_cols <- colnames(df_ML_price)[colSums(is.na(df_ML_price)) > 0]
    df_ML_price <- df_ML_price %>% dplyr::select(-c(all_of(drop_constant_cols)))
    
    # select predictors again
    predictors <- df_ML_price %>% 
      dplyr::select(-c(all_of(price_sel), food, type, country, size, date)) %>% 
      colnames()
    
    # create folds
    ## stratify according to food
    ## food items are similarly distributes across train and test set
    fold_splits <- vfold_cv(df_ML_price, v = k, repeats = 1, seed = 42, 
                            strata = type)
    
    # iterate over folds
    for (fold in 1:length(fold_splits$splits)) {
      
      ## Extract Training and Test Data Set ##
      ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
      
      # extract training data
      df_train <- analysis(fold_splits$splits[[fold]]) %>%
        dplyr::select(-c(food, type, country, size, date))
      
      # extract testing data
      ## store food, type, country, size in extra test data
      df_test_food <- assessment(fold_splits$splits[[fold]])
      df_test <- assessment(fold_splits$splits[[fold]]) %>%
        dplyr::select(-c(food, type, country, size, date))
      
      
      ## Model Setup ##
      ##%%%%%%%%%%%%%##
    
      # define recipe
      ENET_recipe <- 
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
      ENET_perf_tuning <- data.frame()
      
      # iterate over every penalty parameter
      for (penalty_sel in unique(ENET_grid$penalty)) {
        for (mixture_sel in unique(ENET_grid$mixture)) {
          
          # specify model
          ENET_spec_tuning <- 
            linear_reg(penalty = penalty_sel, mixture = mixture_sel) %>%
            set_engine("glmnet") 
          
          # create final workflow
          ENET_workflow_tuning <- 
            workflow() %>%
            add_model(ENET_spec_tuning) %>%
            add_recipe(ENET_recipe)
          
          # fit the model on the training date
          ENET_fit_tuning <- 
            ENET_workflow_tuning %>%
            fit(df_train)
          
          # create data frame with predictions and true value
          ENET_pred_tuning <- data.frame(
            price_pred = predict(ENET_fit_tuning, df_test) %>% 
              dplyr::select(.pred) %>% pull(),
            price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull()
          )
          
          # create data frame with RMSE and penalty
          ENET_perf_tuning_round <- data.frame(
            RMSE = yardstick::rmse(ENET_pred_tuning, 
                        truth = price_true, estimate = price_pred) %>%
              dplyr::select(.estimate) %>% pull(),
            MAE = yardstick::mae(ENET_pred_tuning,
                      truth = price_true, estimate = price_pred) %>%
              dplyr::select(.estimate) %>% pull(),
            MAPE = yardstick::mape(ENET_pred_tuning,
                        truth = price_true, estimate = price_pred) %>%
              dplyr::select(.estimate) %>% pull(),
            penalty = penalty_sel,
            mixture = mixture_sel
          )
          ENET_perf_tuning <- rbind(ENET_perf_tuning, ENET_perf_tuning_round)
        }
      }
    
      # select parameter combination with smallest RMSE
      ENET_best_param <- ENET_perf_tuning %>%
        filter(RMSE == min(RMSE)) %>% 
        head(1)
      ENET_best_param$fold <- fold
      ENET_best_param$model <- price_sel
      ENET_best_param$food <- food_sel
    
      # fit model again
      ENET_spec_best <- 
        linear_reg(penalty = ENET_best_param$penalty, 
                   mixture = ENET_best_param$mixture) %>%
        set_engine("glmnet") 
      
      # create final workflow
      ENET_workflow_best <- 
        workflow() %>%
        add_model(ENET_spec_best) %>%
        add_recipe(ENET_recipe)
      
      # fit the model on the training date
      ENET_fit_best <- 
        ENET_workflow_best %>%
        fit(df_train)
    
      # create data frame with predictions and true value
      ENET_pred_best <- data.frame(
        date = df_test_food$date,
        price = price_sel, 
        price_pred = predict(ENET_fit_best, df_test) %>% 
          dplyr::select(.pred) %>% pull(),
        price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull(),
        food = df_test_food$food,
        type = df_test_food$type,
        size = df_test_food$size,
        country = df_test_food$country
      )
    
      # create data frame with RMSE and penalty
      ENET_perf_best <- data.frame(
        RMSE = yardstick::rmse(ENET_pred_best, 
                    truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        MAE = yardstick::mae(ENET_pred_best,
                  truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        MAPE = yardstick::mape(ENET_pred_best,
                    truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        penalty = ENET_best_param$penalty,
        mixture = ENET_best_param$mixture
      )
      
      # rbind
      ENET_best_param_final <- rbind(ENET_best_param_final, ENET_best_param)
      ENET_perf_final <- rbind(ENET_perf_final, ENET_perf_best)
      ENET_pred_final <- rbind(ENET_pred_final, ENET_pred_best)
    }
  }
}


saveRDS(ENET_best_param_final, "models/ENET/perf_results/ENET_perf_food_model_price_F.rds")
saveRDS(ENET_pred_final, "models/ENET/perf_results/ENET_pred_food_model_price_F.rds")
#saveRDS(ENET_perf_final, "models/ENET/pred_results/ENET_perf_food_model.rds")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#### Final Machine Learning Model ####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##


# from parameter tuning prediction result, we choose the parameter combination
# that leads to the smallest RMSE
# we do so for each model
ENET_final_param <- ENET_best_param_final %>%
  group_by(model, food) %>%
  dplyr::summarise(RMSE_min = min(RMSE)) %>%
  ungroup() %>%
  inner_join(
    ENET_best_param_final, by = c("food", "model", "RMSE_min" = "RMSE")
  ) %>%
  ungroup()

## train final model using the best parameter combination on full data set ##

# iterate over food items
for (food_sel in unique(df_ML$food)) {
  # print model
  print(paste0("Model for ", food_sel))
  
  # filter food
  df_ML_food <- df_ML_raw %>% filter(food == food_sel)
  
  # shuffle data frame
  set.seed(42)
  random_rows <- sample(1:nrow(df_ML_food), nrow(df_ML_food), replace = FALSE)
  df_ML_food <- df_ML_food[random_rows, ]
  
  # iterate over markets/prices
  for (price_sel in prices_2) {
    
    # subset data set - drop prices not used
    df_ML_price <- df_ML_food %>%
      dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
    
    # drop all lagged price variables except the ones for the selected price
    prices_drop <- colnames(df_ML_price)[str_detect(
      colnames(df_ML_price), paste(paste0(c(prices[!prices %in% price_sel],
                                            "price_total"), "_lag"),
                                   collapse = "|"))]
    df_ML_price <- df_ML_price %>%
      dplyr::select(-c(all_of(prices_drop)))
    
    # drop distances
    market_letter <- str_to_lower(str_sub(price_sel, -1))
    distances_drop <- distances[str_sub(distances, -1) != market_letter]
    df_ML_price <- df_ML_price %>%
      dplyr::select(-c(all_of(distances_drop)))
    
    # create dummy variables using type and country
    df_ML_price[, c("type", "country", "size")] <- 
      lapply(df_ML_price[, c("type", "country", "size")], as.factor)
    
    df_ML_price <- dummy_cols(
      df_ML_price,
      remove_first_dummy = FALSE,
      remove_selected_columns = FALSE, 
      select_columns = c("type", "country", "size")
    )
    
    # select predictors
    predictors <- df_ML_price %>%
      dplyr::select(-c(all_of(price_sel), food, type, country, size, date)) %>%
      colnames()
    
    # standardize features
    ## first create standardizer for future test data sets
    # standardizer <- df_ML_price %>%
    #   recipe(.) %>%
    #   # price variable is outcome
    #   update_role({{price_sel}}, new_role = "outcome") %>%
    #   # all other variables are predictors
    #   update_role({{predictors}},
    #               new_role = "predictor") %>%
    #   # standardize all predictors: substract mean and divide by standard dev.
    #   step_normalize(all_predictors()) %>%
    #   prep()
    
    ## standardize features in data set
    df_ML_final_model <- df_ML_price %>%
      recipe(.) %>%
      # price variable is outcome
      update_role({{price_sel}}, new_role = "outcome") %>%
      # all other variables are predictors
      update_role({{predictors}},
                  new_role = "predictor") %>%
      # standardize all predictors
      step_normalize(all_predictors()) %>%
      # estimate parameters
      prep() %>%
      # apply standardization to full data set
      bake(new_data = NULL)
    
    # constant variables have NA after standardization
    # hence, we drop them 
    drop_constant_cols <- 
      colnames(df_ML_final_model)[colSums(is.na(df_ML_final_model)) > 0]
    df_ML_final_model <- 
      df_ML_final_model %>% dplyr::select(-c(all_of(drop_constant_cols)))
    
    # select predictors again
    predictors <- df_ML_final_model %>%
      dplyr::select(-c(all_of(price_sel), food, type, country, size, date)) %>%
      colnames()
  
    # define recipe
    ENET_recipe <- 
      # select full data set
      df_ML_final_model %>%  
      recipe(.) %>%
      # price variable is outcome
      update_role({{price_sel}}, new_role = "outcome") %>%
      # all other variables are predictors
      update_role({{predictors}},
                  new_role = "predictor")
  
    # extract penalty parameter
    penalty_sel <- ENET_final_param %>% 
      filter(model == price_sel & food == food_sel) %>% 
      dplyr::select(penalty) %>% 
      pull()
    
    mixture_sel <- ENET_final_param %>% 
      filter(model == price_sel & food == food_sel) %>% 
      dplyr::select(mixture) %>% 
      pull()
    
    # specify final model
    ENET_spec_final <-
      linear_reg(penalty = penalty_sel, mixture = mixture_sel) %>%
      set_engine("glmnet")
  
    # create final workflow
    ENET_workflow_final <-
      workflow() %>%
      add_model(ENET_spec_final) %>%
      add_recipe(ENET_recipe)
  
    # fit the model on the full data set
    ENET_fit <-
      ENET_workflow_final %>%
      fit(df_ML_final_model)
  
    # save model to make predictions for next week in R Shiny App
    save_path <- 
      paste0("models/ENET/final_models/ENET_", food_sel, "_", price_sel, ".rds")
    saveRDS(ENET_fit, save_path)
  }
}