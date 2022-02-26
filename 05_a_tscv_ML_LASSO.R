#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Machine Learning: LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# In this file the LASSO regression is tuned and fitted for each food type
# separately. 
# For parameter tuning a 5-fold CV is conducted. The LASSO regression has one 
# tuning parameter, namely penalty (also called lambda). 
# The mixture parameter (also called alpha) is 1 in the LASSO model since the 
# l1 norm as penalty is selected. To tune the lambda/penalty parameter 100 values
# are randomly chosen. 

# Important notes which may be considered for pre-processing the data:
# - LASSO can only handle numeric and integer variables, i.e., no character, date etc.
# - LASSO cannot handle missing values
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

# glmnet is used to fit the LASSO regression
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
## the last one is date and dropped later
ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | sapply(df_ML_raw, is.integer)])
## all columns integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer, as.numeric)
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor, as.character)

# cross validation (5-fold CV)
k <- 5

# machine learning data frame
df_ML <- df_ML_raw
  ## drop all year, month and week columns as they are not useful in time series CV
df_ML <- df_ML %>%
  dplyr::select(!starts_with("year_")) %>%
  dplyr::select(!starts_with("month_")) %>%
  dplyr::select(!starts_with("week_"))
## for development
df_ML <- df_ML %>% filter(food %in% c("Strawberry", "Kiwi", "Cauliflower"))


##%%%%%%%%%%%%%%%%%%%%%%##
#### Parameter Tuning ####
##%%%%%%%%%%%%%%%%%%%%%%##

# define parameter grid
LASSO_grid <- grid_regular(penalty(), levels = 500)

# create data frames which combine the results during the loop
LASSO_best_param_final <- data.frame() # save parameters
LASSO_pred_final <- data.frame() # save predictions
LASSO_perf_final <- data.frame() # save performance
LASSO_coef <- data.frame() # save coefficients

# different prices
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

# different distances
distances <- df_ML %>% dplyr::select(starts_with("distance")) %>% colnames()

# establish train and test counter
train_start_orig <- 2016
train_start <- 2016
train_end <- 2020
test_start <- 2017

# start counting fold 
fold <- 0

# conduct time series cross-validation
# while loop stops as soon as all data has been used for training the model
while (train_start <= train_end) {
  # indicate fold
  fold <- fold + 1
  
  # print model
  print(paste0("Fold ", fold))
  
  # create train and test splits
  ## first training round: training 2016, predict 2017
  ## second training round: training 2016+2017, predict 2018
  ## third training round: training 2016-2018, predict 2019
  ## fourth training round: training 2016-2019, predict 2020
  ## fifth training round: training 2016-2020, predict 2021 + 2022
  df_ML_train <- df_ML %>%
    filter(lubridate::year(date) %in% train_start_orig:train_start)
  
  df_ML_test <- df_ML %>%
    filter(lubridate::year(date) == test_start)
  
  # increase train and test indices
  train_start <- train_start + 1
  test_start <- test_start + 1
  
  if (test_start == 2021) {
    test_start <- 2022
  }
  
  # iterate over food
  for (food_sel in unique(df_ML$food)) {
    
    # filter food and arrange by date
    df_ML_train_food <- df_ML_train %>% 
      filter(food == food_sel) %>%
      arrange(date)
    
    df_ML_test_food <- df_ML_test %>% 
      filter(food == food_sel) %>%
      arrange(date)
      
    # iterate over prices (for each price, a single machine learning model is trained)
    for (price_sel in prices) {
    
      # to create folds, drop price variables not used in current model
      df_ML_price_train <- df_ML_train_food %>%
        dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
      
      df_ML_price_test <- df_ML_test_food %>%
        dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
      
      # drop all lagged price variables except the ones for the selected price
      prices_drop <- colnames(df_ML_price_train)[str_detect(
        colnames(df_ML_price_train), paste(paste0(c(prices[!prices %in% price_sel], 
                                                    "price_total"), "_lag"), 
                                           collapse = "|"))]
      df_ML_price_train <- df_ML_price_train %>%
        dplyr::select(-c(all_of(prices_drop)))
      
      df_ML_price_test <- df_ML_price_test %>%
        dplyr::select(-c(all_of(prices_drop)))
      
      # moreover, only the distance variable for the current model should be
      # included.
      ## extract market letter
      market_letter <- str_to_lower(str_sub(price_sel, -1))
      ## identify distance variables to drop
      distances_drop <- distances[str_sub(distances, -1) != market_letter]
      ## drop distances
      df_ML_price_train <- df_ML_price_train %>%
        dplyr::select(-c(all_of(distances_drop)))
      
      df_ML_price_test <- df_ML_price_test %>%
        dplyr::select(-c(all_of(distances_drop)))
      
      
      # create dummy variables using type and country and size
      df_ML_price_train[, c("type", "country", "size")] <- 
        lapply(df_ML_price_train[, c("type", "country", "size")], as.factor)
      
      df_ML_price_train <- dummy_cols(
        df_ML_price_train,
        remove_first_dummy = FALSE,
        remove_selected_columns = FALSE, 
        select_columns = c("type", "country", "size")
      )
      
      
      df_ML_price_test[, c("type", "country", "size")] <- 
        lapply(df_ML_price_test[, c("type", "country", "size")], as.factor)
      
      df_ML_price_test <- dummy_cols(
        df_ML_price_test,
        remove_first_dummy = FALSE,
        remove_selected_columns = FALSE, 
        select_columns = c("type", "country", "size")
      )
      
      # select predictors
      predictors <- df_ML_price_train %>% 
        dplyr::select(-c(all_of(price_sel), food, type, country, size, date)) %>% 
        colnames()
    
      # standardize features to mean zero and unit variance
      ## very important for regularization because regularization solutions
      ## are not equivariant under scaling of the inputs
      ## This ensures that all predictors are on the same scale and therefore the 
      ## model fit is independent on the scale on which the features are measured.
      df_ML_price_train <- df_ML_price_train %>%
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
      
      
      df_ML_price_test <- df_ML_price_test %>%
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
      drop_constant_cols_train <- colnames(df_ML_price_train)[colSums(is.na(df_ML_price_train)) > 0]
      drop_constant_cols_test <- colnames(df_ML_price_test)[colSums(is.na(df_ML_price_test)) > 0]
      drop_constant_cols <- unique(c(drop_constant_cols_train, drop_constant_cols_test))
      
      df_ML_price_train <- df_ML_price_train %>% dplyr::select(-c(all_of(drop_constant_cols)))
      df_ML_price_test <- df_ML_price_test %>% dplyr::select(-c(all_of(drop_constant_cols)))
    
      # select predictors again
      predictors <- df_ML_price_train %>% 
        dplyr::select(-c(all_of(price_sel), food, type, country, size, date)) %>% 
        colnames()
    
      # create train and test data with correct variables
      df_train <- df_ML_price_train %>%
        dplyr::select(-c(food, type, country, size))
      
      df_test <- df_ML_price_test %>%
        dplyr::select(-c(food, type, country, size))
      
      # define recipe
      LASSO_recipe <- 
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
      LASSO_perf_tuning <- data.frame()
      
      # iterate over every penalty parameter
      for (penalty_sel in unique(LASSO_grid$penalty)) {
        
        # specify model
        LASSO_spec_tuning <- 
          linear_reg(penalty = penalty_sel, mixture = 1) %>%
          set_engine("glmnet") 
        
        # create final workflow
        LASSO_workflow_tuning <- 
          workflow() %>%
          add_model(LASSO_spec_tuning) %>%
          add_recipe(LASSO_recipe)
        
        # fit the model on the training date
        LASSO_fit_tuning <- 
          LASSO_workflow_tuning %>%
          fit(df_train)
        
        # create data frame with predictions and true value
        LASSO_pred_tuning <- data.frame(
          price_pred = predict(LASSO_fit_tuning, df_test) %>% 
            dplyr::select(.pred) %>% pull(), 
          price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull()
        )
        
        # create data frame with RMSE and penalty
        LASSO_perf_tuning_round <- data.frame(
          RMSE = yardstick::rmse(LASSO_pred_tuning, 
                                 truth = price_true, estimate = price_pred) %>%
            dplyr::select(.estimate) %>% pull(),
          MAE = yardstick::mae(LASSO_pred_tuning,
                               truth = price_true, estimate = price_pred) %>%
            dplyr::select(.estimate) %>% pull(),
          MAPE = yardstick::mape(LASSO_pred_tuning,
                                 truth = price_true, estimate = price_pred) %>%
            dplyr::select(.estimate) %>% pull(),
          penalty = penalty_sel
        )
        
        LASSO_perf_tuning <- rbind(LASSO_perf_tuning, LASSO_perf_tuning_round)
      } # close bracket for parameter tuning
      
      # select parameter combination with smallest RMSE
      LASSO_best_param <- LASSO_perf_tuning %>%
        filter(RMSE == min(RMSE)) %>% 
        head(1)
      LASSO_best_param$fold <- fold
      LASSO_best_param$model <- price_sel
      LASSO_best_param$food <- food_sel
      
      # fit model again 
      LASSO_spec_best <- 
        linear_reg(penalty = LASSO_best_param$penalty, mixture = 1) %>%
        set_engine("glmnet") 
      
      LASSO_workflow_best <- 
        workflow() %>%
        add_model(LASSO_spec_best) %>%
        add_recipe(LASSO_recipe)
      
      LASSO_fit_best <- 
        LASSO_workflow_best %>%
        fit(df_train)
      
      # make predictions
      LASSO_pred_best <- data.frame(
        date = df_test$date, 
        price = price_sel, 
        price_pred = predict(LASSO_fit_best, df_test) %>% 
          dplyr::select(.pred) %>% pull(),
        price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull(),
        food = df_ML_price_test$food,
        type = df_ML_price_test$type,
        size = df_ML_price_test$size,
        country = df_ML_price_test$country
      )
      
      # evaluate performance
      LASSO_perf_best <- data.frame(
        RMSE = yardstick::rmse(LASSO_pred_best, 
                               truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        MAE = yardstick::mae(LASSO_pred_best,
                             truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        MAPE = yardstick::mape(LASSO_pred_best,
                               truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        penalty = LASSO_best_param$penalty
      )
      
      # rbind
      LASSO_best_param_final <- rbind(LASSO_best_param_final, LASSO_best_param)
      LASSO_perf_final <- rbind(LASSO_perf_final, LASSO_perf_best)
      LASSO_pred_final <- rbind(LASSO_pred_final, LASSO_pred_best)
    } # close bracket for markets
  } # close food prices loop
} # close while loop for cv 

saveRDS(LASSO_best_param_final, 
        "models/tscv_models/LASSO/LASSO_tuning_TSCV_model.rds")

saveRDS(LASSO_pred_final, 
        "models/tscv_models/LASSO/LASSO_pred_TSCV_model.rds")


