#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Machine Learning: Random Forest for each fruit/vegetable ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# This will be filled with Random Forest Theory shortly --

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# tidymodels framework is used for machine learning
if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)

# xgboost is used to fit the xgboost model
if (!require("randomForest")) install.packages("randomForest") 
library(randomForest)

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
ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | 
                                   sapply(df_ML_raw, is.integer)])
## all columns integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer,as.numeric)
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)

# cross validation (5-fold CV)
k <- 2  # 5 changing to 2-fold cross-validation

# machine learning data frame
df_ML <- df_ML_raw
## for development
#df_ML <- df_ML_raw %>%
  #filter(food %in% c("Strawberry", "Banana"))



##%%%%%%%%%%%%%%%%%%%%%%##
#### Parameter Tuning ####
##%%%%%%%%%%%%%%%%%%%%%%##

# define parameter grid
## parameter values are chosen manually in order to have full control
rf_grid <- expand.grid(
  trees = c(1000),
  mtry = c(100),  # Limit the mtry==100 in first iteration, for faster big data computing
  #mtry = c(1:2),  # for small data testing
  min_n = c(100)
)

# create data frames which combine the results during the loop
rf_best_param_final <- data.frame()
rf_pred_final <- data.frame()
rf_perf_final <- data.frame()
rf_coef <- data.frame()

# different prices
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

# different distances
distances <- df_ML %>% select(starts_with("distance")) %>% colnames()

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
  for (price_sel in prices) {
    
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
      rf_best_param$food <- food_sel
      
      # fit model again 
      rf_spec_best <- 
        rand_forest(trees = rf_best_param$trees,
                    mtry = rf_best_param$mtry,
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
        date = df_test_food$date, 
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
    }
  }
}

saveRDS(rf_best_param_final, "rf_tuning_food_model.rds")
saveRDS(rf_perf_final, "rf_perf_food_model.rds")
saveRDS(rf_pred_final, "rf_pred_food_model.rds")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#### Final Machine Learning Model ####
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

# from parameter tuning prediction result, we choose the parameter combination
# that leads to the smallest RMSE
# we do so for each model
rf_final_param <- rf_best_param_final %>%
  group_by(model, food) %>%
  dplyr::summarise(RMSE_min = min(RMSE)) %>%
  ungroup() %>%
  inner_join(
    rf_best_param_final, by = c("food", "model", "RMSE_min" = "RMSE")
  ) %>%
  ungroup()


## train final model using the best parameter combination on full data set ##

# iterate over food items
for (food_sel in unique(df_ML$food)) {
  # print model
  print(paste0("Model for ", food_sel, ":"))
  
  # filter food
  df_ML_food <- df_ML_raw %>% filter(food == food_sel)
  
  # shuffle data frame
  set.seed(42)
  random_rows <- sample(1:nrow(df_ML_food), nrow(df_ML_food), replace = FALSE)
  df_ML_food <- df_ML_food[random_rows, ]
  
  # iterate over markets/prices
  for (price_sel in prices) {
    
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
      filter(model == all_of(price_sel) & food == food_sel) %>% 
      dplyr::select(trees, mtry, min_n) 
    ## model specification
    rf_spec_final <- 
      rand_forest(trees = rf_params_final$trees,
                  mtry = rf_params_final$mtry,
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
    save_path <- paste0("Random_Forest_", food_sel, "_", price_sel, ".rds")
    saveRDS(rf_fit, save_path)
  }
}