##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany

#### Data Project - Machine Learning - SVM

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file


if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("e1071")) install.packages("e1071")
if (!require("caret")) install.packages("caret")
if (!require("modelr")) install.packages("modelr")
if (!require("tidymodels")) install.packages("tidymodels") 
if (!require("MLmetrics")) install.packages("MLmetrics") 
if (!require("fastDummies")) install.packages("fastDummies")
if (!require("stringr")) install.packages("stringr")

library(dplyr)
library(tidyverse)
library(e1071)
library(caret)
library(modelr)
library(tidymodels)
library(MLmetrics)
library(fastDummies)
library(stringr)


#### Loading data and prepare cross-validation
#### General Setup ####

# load machine learning data frame
df_ML_raw <- readRDS("output/final_food_ML.rds")

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
df_ML_raw <- df_ML_raw

# drop all total price variables 
totals <- colnames(df_ML_raw)[str_detect(colnames(df_ML_raw), "price_total")]
df_ML_raw <- df_ML_raw %>% select(-all_of(totals))

# cross validation (5-fold CV)
k <- 5

# machine learning data frame
df_ML <- df_ML_raw

## vfold_cv function splits the data into v/k folds
fold_splits <- vfold_cv(df_ML, v = k, repeats = 1, seed = 42,
                        strata = food)

## Specify Tuning Grid incl. cross-validation for radial kernel
# set gamma, cost and epsilon parameters to be checked
# Computation on Cluster

train_control <- trainControl(method = "cv", number = 2)
svm_grid <- expand.grid(C = seq(0.1, 1, 0.3), sigma = c(0.0001, seq(0.001, 0.01, 0.002)))

## build model on each train fold and predict on test set
## for each price: 

prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

## create a model for each food item
items <- unique(df_ML$food)

distances <- df_ML %>% dplyr::select(starts_with("distance")) %>% colnames()

### Prepare Data Frame for Output
# metrics
results <- data.frame()

# predictions
food_preds <- data.frame()

for(item in items){
  for(price in prices){
    
    item_folds <- data.frame()

    # remove prices of other markets
    
    rem <- prices[!prices %in% price]
    
    ## extract market letter
    market_letter <- str_to_lower(str_sub(price, -1))
    
    ## identify distance variables to drop
    distances_drop <- distances[str_sub(distances, -1) != market_letter]
    
    for (fold in 1:length(fold_splits$splits)) {
      
      
      ## Extract Training and Test Data Set
      
      # extract training data
      df_train <- analysis(fold_splits$splits[[fold]]) %>%
        select(-c(all_of(rem), all_of(distances_drop), date))
      
      # remove price lags of other markets
      
      rem_lags <- colnames(df_train)[str_detect(colnames(df_train), paste(rem, collapse = "|"))]
      
      df_train <- df_train %>%
        select(-all_of(rem_lags)) %>%
        filter(food == item) %>%
        select(-food)
      
      # create dummy variables for country, size and type!
      
      df_train[, c("type", "country", "size")] <- lapply(df_train[, c("type", "country", "size")], as.factor)
      
      df_train <- dummy_cols(
        df_train,
        remove_first_dummy = FALSE,
        remove_selected_columns = TRUE, 
        select_columns = c("type", "country", "size")
      )
      
      # obtain col number of price
      col_num <- which(colnames(df_train) == price)
      
      df_train_x <- df_train[, -col_num]
      
      # use scaling values from scaler folder: min max 
      # remove variables with NA: those variables, not in the scaler are dummies 
      # which do not vary (like: greenhouse)
      
      scaler <- readRDS(paste0("models/scaler/data_scaler_", item, "_food.rds")) 
      
      scaler_x <- scaler %>%
        filter(variable %in% colnames(df_train_x))
      
      norm_rem <- colnames(df_train_x)[!colnames(df_train_x) %in% scaler_x$variable]
      
      df_train_x <- df_train_x %>%
        select(-all_of(norm_rem))
      
      for(col in colnames(df_train_x)){
        
        # manually scale variables
        df_train_x[, col] <- (df_train_x[, col] - (scaler_x[scaler_x$variable == col, "mean"]))/
          (scaler_x[scaler_x$variable == col, "sd"])
        
      }
      
      # here no need to transform target
      df_train_y <- pull(df_train[, col_num])
      
      # rename cols because error occurs if / is part of names
      colnames(df_train_x) <- str_replace_all(colnames(df_train_x), " ", "_")
      colnames(df_train_x) <- str_replace_all(colnames(df_train_x), "[[:punct:]]", "_")
      colnames(df_train_x) <- str_replace_all(colnames(df_train_x), ">", "_")
      
      # extract testing data
      df_test <- assessment(fold_splits$splits[[fold]]) %>%
        select(-c(all_of(rem), all_of(rem_lags), all_of(distances_drop), date)) %>%  
        filter(food == item) %>%
        select(-food)
      
      # create dummy variables for country, size and type also for test data!
      
      df_test[, c("type", "country", "size")] <- lapply(df_test[, c("type", "country", "size")], as.factor)
      
      df_test <- dummy_cols(
        df_test,
        remove_first_dummy = FALSE,
        remove_selected_columns = TRUE, 
        select_columns = c("type", "country", "size")
      )
      
      df_test <- df_test %>%
        select(-all_of(norm_rem))
      
      df_test_x <- df_test[, -col_num]
      
      # use scaling values from output
      # remove variables with NA: those variables, not in the scaler are dummies 
      # which do not vary (like: greenhouse)
      
      for(col in colnames(df_test_x)){
        
        # manually scale variables: we do this because for new predictions we need 
        # the max and min values to scale new input values
        
        df_test_x[, col] <- (df_test_x[, col] - (scaler_x[scaler_x$variable == col, "mean"]))/
          (scaler_x[scaler_x$variable == col, "sd"])
        
      }
      
      # rename cols because error occurs if / is part of names
      colnames(df_test_x) <- str_replace_all(colnames(df_test_x), " ", "_")
      colnames(df_test_x) <- str_replace_all(colnames(df_test_x), "[[:punct:]]", "_")
      colnames(df_test_x) <- str_replace_all(colnames(df_test_x), ">", "_")
      
      # no need to transform test target
      df_test_y <- pull(df_test[, col_num])
      
      ##### Support Vector Regression
      
      svm_tune <- train(
        x = df_train_x,
        y = df_train_y,
        method = "svmRadial",
        tuneGrid = svm_grid, 
        trControl = train_control, 
        metric = "RMSE"
      )

      
      # predictions
      preds <- predict(svm_tune, df_test_x)

      # extract test data to save predictions
      df_test_food <- assessment(fold_splits$splits[[fold]]) %>%
        select(date, food, type, country, size, all_of(price)) %>%
        filter(food == item) %>%
        mutate(price_pred = preds)
      
      df_test_food$price <- price
      rename_price_col <- which(str_detect(colnames(df_test_food), price))
      colnames(df_test_food)[rename_price_col] <- "price_true"
      
      df_test_food <- df_test_food[, c("date", "price", "price_pred", "price_true", 
                                       "food", "type", "size", "country")]
      
      
      food_preds <- rbind(food_preds, df_test_food)
      
      # obtain metrics 
      
      MAPE <- MAPE(preds, df_test_y)
      RMSE <- RMSE(preds, df_test_y)
      MAE <- MAE(preds, df_test_y)
      
      svm_best <- svm_tune$bestTune
      
      # save parameters
      
      fold_results <- as.data.frame(cbind(RMSE, MAE, MAPE, svm_best[1], svm_best[2], 
                                          fold, price, item))
      
      # for model selection 
      item_folds <- rbind(item_folds, fold_results)
      
      # for metrics
      results <- rbind(results, fold_results)
      
      
    }
    
    best_results <- item_folds %>%
      group_by(price) %>%
      arrange(RMSE) %>%
      filter(row_number() == 1)
    
    
    perf_grid <- expand.grid(C = best_results$C, sigma = best_results$sigma)
    
    # save model 
    
    svm_tune <- train(
      x = df_train_x,
      y = df_train_y,
      method = "svmRadial",
      tuneGrid = svm_grid, 
      trControl = train_control, 
      metric = "RMSE"
    )
    
    saveRDS(svm_tune, paste0("models/SVM/final_models/SVM_", item, "_", price, ".rds"))
    
    
  }
}

# save metrics
# reorder columns 

results <- results %>%
  dplyr::rename(model = price, food = item)

saveRDS(results, "models/SVM/perf_results/SVM_tuning_food_model.rds")

# finally save predictions
# arrange

food_preds <- food_preds %>%
  arrange(food, date, price) 

saveRDS(food_preds, "models/SVM/pred_results/SVM_pred_food_model.rds")



















