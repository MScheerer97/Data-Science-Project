#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Machine Learning: Ridge ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# In this file the Ridge regression is tuned and fitted. For parameter
# tuning a 5-fold CV is conducted. The Ridge regression has one tuning parameter,
# namely penalty (also called lambda). The mixture parameter (also called alpha)
# is 0 in the Ridge model since the l2 norm as penalty is selected. To tune the
# lambda/penalty parameter 1000 values are randomly chosen.

# Important notes which may be considered for pre-processing the data:
# - Ridge can only handle numeric and integer variables, i.e., no character, date etc.
# - Ridge cannot handle missing values
# - To ensure that all variables are on the same scale and therefore the model fit
# is independent on the scale on which the features are measured, all features
# are standardized to have zero mean and unit variance.
# Note: According to Tibshirani (1997) also dummy variables should be standardized.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# tidymodels framework is used for machine learning
# if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)

# glmnet is used to fit the RIDGE regression
# if (!require("glmnet")) install.packages("glmnet") 
library(glmnet)

# dplyr for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)

# stringr for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)

#### General Setup ####

# load machine learning data frame
df_ML_raw <- readRDS("output/final_ML.rds")

# establish row number column
## needed to later find correct food etc. 
#df_ML_raw$row_number <- row.names(df_ML_raw)

# shuffle data frame
set.seed(42)
random_rows <- sample(1:nrow(df_ML_raw), nrow(df_ML_raw), replace = FALSE)
df_ML_raw <- df_ML_raw[random_rows, ]

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
k <- 5

# machine learning data frame
df_ML <- df_ML_raw


##%%%%%%%%%%%%%%%%%%%%%%##
#### Parameter Tuning ####
##%%%%%%%%%%%%%%%%%%%%%%##

# define parameter grid
## 1000 random values for the amount of regularization are used
## penalty generally ranges from 0 to 1
RIDGE_grid <- grid_regular(penalty(), levels = 500)

# create data frames which combine the results during the loop
RIDGE_best_param_final <- data.frame()
RIDGE_pred_final <- data.frame()
#RIDGE_perf_final <- data.frame()

# different prices
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

# different distances
distances <- df_ML %>% dplyr::select(starts_with("distance")) %>% colnames()

# for each price, a single machine learning model is trained
for (price_sel in prices) {
  
  print(paste("Model for:", all_of(price_sel)))
  
  # to create folds, drop price variables not used in current model
  df_ML_price <- df_ML %>%
    dplyr::select(-c(prices[!prices %in% all_of(price_sel)], "price_total"))
  
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
  
  # create folds
  fold_splits <- vfold_cv(df_ML_price, v = k, repeats = 1, seed = 42, strata = food)
  
  # iterate over folds
  for (fold in 1:length(fold_splits$splits)) {
    
    ## Extract Training and Test Data Set ##
    ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
    
    # extract training data
    df_train <- analysis(fold_splits$splits[[fold]])
    
    ## store food, type, country, size in extra test data
    df_test_food <- assessment(fold_splits$splits[[fold]])
    df_test <- assessment(fold_splits$splits[[fold]]) %>%
      dplyr::select(-c(food, type, country, size))
    
    
    ## Model Setup ##
    ##%%%%%%%%%%%%%##
    
    # select predictors
    #predictors <- df_train %>% dplyr::select(-all_of(price_sel)) %>% colnames()
    
    # define recipe
    RIDGE_recipe <- 
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
    RIDGE_perf_tuning <- data.frame()
    
    # iterate over every penalty parameter
    for (penalty_sel in unique(RIDGE_grid$penalty)) {
      
      # specify model
      RIDGE_spec_tuning <- 
        linear_reg(penalty = penalty_sel, mixture = 0) %>%
        set_engine("glmnet") 
      
      # create final workflow
      RIDGE_workflow_tuning <- 
        workflow() %>%
        add_model(RIDGE_spec_tuning) %>%
        add_recipe(RIDGE_recipe)
      
      # fit the model on the training date
      RIDGE_fit_tuning <- 
        RIDGE_workflow_tuning %>%
        fit(df_train)
      
      # create data frame with predictions and true value
      RIDGE_pred_tuning <- data.frame(
        price_pred = predict(RIDGE_fit_tuning, df_test) %>% 
          dplyr::select(.pred) %>% pull(),
        price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull()
      )
      
      # create data frame with RMSE and penalty
      RIDGE_perf_tuning_round <- data.frame(
        RMSE = rmse(RIDGE_pred_tuning, 
                    truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        MAE = mae(RIDGE_pred_tuning,
                  truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        MAPE = mape(RIDGE_pred_tuning,
                    truth = price_true, estimate = price_pred) %>%
          dplyr::select(.estimate) %>% pull(),
        penalty = penalty_sel
      )
      
      RIDGE_perf_tuning <- rbind(RIDGE_perf_tuning, RIDGE_perf_tuning_round)
    }
    
    # select parameter combination with smallest RMSE
    RIDGE_best_param <- RIDGE_perf_tuning %>%
      filter(RMSE == min(RMSE)) %>% 
      head(1)
    RIDGE_best_param$fold <- fold
    RIDGE_best_param$model <- price_sel
    
    # fit model again
    RIDGE_spec_best <- 
      linear_reg(penalty = RIDGE_best_param$penalty, mixture = 0) %>%
      set_engine("glmnet") 
    
    # create final workflow
    RIDGE_workflow_best <- 
      workflow() %>%
      add_model(RIDGE_spec_best) %>%
      add_recipe(RIDGE_recipe)
    
    # fit the model on the training date
    RIDGE_fit_best <- 
      RIDGE_workflow_best %>%
      fit(df_train)
    
    # create data frame with predictions and true value
    RIDGE_pred_best <- data.frame(
      price = price_sel, 
      price_pred = predict(RIDGE_fit_best, df_test) %>% 
        dplyr::select(.pred) %>% pull(),
      price_true = df_test %>% dplyr::select(all_of(price_sel)) %>% pull(),
      food = df_test_food$food,
      type = df_test_food$type,
      size = df_test_food$size,
      country = df_test_food$country
    )
    
    # create data frame with RMSE and penalty
    # RIDGE_perf_best <- data.frame(
    #   RMSE = rmse(RIDGE_pred_best, 
    #               truth = price_true, estimate = price_pred) %>%
    #     dplyr::select(.estimate) %>% pull(),
    #   MAE = mae(RIDGE_pred_best,
    #             truth = price_true, estimate = price_pred) %>%
    #     dplyr::select(.estimate) %>% pull(),
    #   MAPE = mape(RIDGE_pred_best,
    #               truth = price_true, estimate = price_pred) %>%
    #     dplyr::select(.estimate) %>% pull(),
    #   penalty = RIDGE_best_param$penalty
    # )
    
    # rbind
    RIDGE_best_param_final <- rbind(RIDGE_best_param_final, RIDGE_best_param)
    #RIDGE_perf_final <- rbind(RIDGE_perf_final, RIDGE_perf_best)
    RIDGE_pred_final <- rbind(RIDGE_pred_final, RIDGE_pred_best)
    
    # track progress
    print(paste("Fold:", fold))
  }
}


saveRDS(RIDGE_best_param_final, 
        "models/RIDGE/perf_results/RIDGE_perf_full_model.rds")
#saveRDS(RIDGE_perf_final, "RIDGE_perf_result.rds")
saveRDS(RIDGE_pred_final, 
        "models/RIDGE/pred_results/RIDGE_pred_full_model.rds")



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#### Final Machine Learning Model ####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##


# # from parameter tuning prediction result, we choose the parameter combination
# # that leads to the smallest RMSE
# # we do so for each model
# RIDGE_final_param <- RIDGE_best_param_final %>%
#   group_by(model) %>%
#   summarise(RMSE_min = min(RMSE)) %>%
#   inner_join(
#     RIDGE_best_param_final, by = c("model", "RMSE_min" = "RMSE")
#   )
# 
# # train final model using the best parameter combination on full data set # 
# # do so for each model #
# for (price_sel in prices) {
#   
#   # subset data set - drop prices not used
#   df_ML_price <- df_ML %>%
#     dplyr::select(-c(all_of(prices[!prices %in% price_sel]), "price_total"))
#   
#   # drop all lagged price variables except the ones for the selected price
#   prices_drop <- colnames(df_ML_price)[str_detect(
#     colnames(df_ML_price), paste(paste0(c(prices[!prices %in% price_sel], 
#                                           "price_total"), "_lag"), 
#                                  collapse = "|"))]
#   df_ML_price <- df_ML_price %>%
#     dplyr::select(-c(all_of(prices_drop)))
#   
#   
#   # select predictors
#   predictors <- df_ML_price %>% 
#     dplyr::select(-c(all_of(price_sel), food, type, country, size)) %>% 
#     colnames()
#   
#   # standardize features
#   df_ML_final_model <- df_ML_price %>%
#     recipe(.) %>%
#     # price variable is outcome
#     update_role({{price_sel}}, new_role = "outcome") %>%
#     # all other variables are predictors
#     update_role({{predictors}},
#                 new_role = "predictor") %>%
#     # standardize all predictors
#     step_normalize(all_predictors()) %>%
#     # estimate parameters
#     prep() %>%
#     # apply standardization to full data set
#     bake(new_data = NULL)
#   
#   # define recipe
#   RIDGE_recipe <- 
#     # select full data set
#     df_ML_final_model %>%  
#     recipe(.) %>%
#     # price variable is outcome
#     update_role({{price_sel}}, new_role = "outcome") %>%
#     # all other variables are predictors
#     update_role({{predictors}},
#                 new_role = "predictor")
#   
#   # specify final model
#   penalty_sel <- RIDGE_final_param %>% 
#     filter(model == price_sel) %>% 
#     dplyr::select(penalty) %>% 
#     pull()
#   RIDGE_spec_final <-
#     linear_reg(penalty = penalty_sel, mixture = 0) %>%
#     set_engine("glmnet")
#   
#   # create final workflow
#   RIDGE_workflow_final <-
#     workflow() %>%
#     add_model(RIDGE_spec_final) %>%
#     add_recipe(RIDGE_recipe)
#   
#   # fit the model on the full data set
#   RIDGE_fit <-
#     RIDGE_workflow_final %>%
#     fit(df_ML_final_model)
#   
#   # save model to make predictions for next week in R Shiny App
#   save_path <- paste0("RIDGE_", price_sel, ".rds")
#   saveRDS(RIDGE_fit, save_path)
# }
