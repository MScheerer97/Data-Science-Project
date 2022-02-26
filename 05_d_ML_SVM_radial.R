##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany

#### Data Project - Machine Learning - Support Vector Machine

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

library(dplyr)
library(tidyverse)
library(e1071)
library(caret)
library(modelr)
library(tidymodels)
library(MLmetrics)


#### Loading data and prepare cross-validation
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
df_ML_raw <- df_ML_raw %>% select(-price_total)

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

distances <- df_ML %>% dplyr::select(starts_with("distance")) %>% colnames()

train_control <- trainControl(method = "cv", number = 2)
svm_grid <- expand.grid(C = seq(0.1, 1, 0.3), sigma = c(0.0001, seq(0.001, 0.01, 0.002)))


## build model on each train fold and predict on test set
## for each price: 

prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

for(price in prices){
  
  rem <- prices[!prices %in% price]
  
  ## extract market letter
  market_letter <- str_to_lower(str_sub(price, -1))
  
  ## identify distance variables to drop
  distances_drop <- distances[str_sub(distances, -1) != market_letter]
  
  ### Prepare Data Frame for Output
  results <- data.frame()
  food_preds <- data.frame()
  
    for (fold in 1:length(fold_splits$splits)) {
      
    ## Extract Training and Test Data Set
    
    # extract training data
      
    df_train <- analysis(fold_splits$splits[[fold]]) %>%
      select(-c(all_of(rem), all_of(distances_drop))) %>%
      select(-c(food, type, country, size, date))
    
    rem_lags <- colnames(df_train)[str_detect(colnames(df_train), paste(rem, collapse = "|"))]
    
    df_train <- df_train %>%
      select(-all_of(rem_lags))
    
    
    # col number of target variable
    
    col_num <- which(colnames(df_train) == price)
    
    
    df_train_x <- as.data.frame(scale(df_train[, -col_num]))
    df_train_y <- pull(df_train[, col_num])


# extract testing data
    
    df_test <- assessment(fold_splits$splits[[fold]]) %>%
      select(-c(all_of(rem), all_of(distances_drop))) %>%
      select(-c(food, type, country, size, all_of(rem_lags), date))
    
    df_test_x <- as.data.frame(scale(df_test[, -col_num]))
    df_test_y <- pull(df_test[, col_num])
    
# the radial kernel is chosen as previous computations showed better results 
# compared to the polynomial kernel (tested using d = 2, 3, 4)
    
    svm_tune <- train(
      x = df_train_x,
      y = df_train_y,
      method = "svmRadial",
      tuneGrid = svm_grid, 
      trControl = train_control, 
      metric = "RMSE"
      )

    preds <- predict(svm_tune, df_test_x)
    
    # extract test data to save predictions
    df_test_food <- assessment(fold_splits$splits[[fold]]) %>%
      select(food, type, country, size, all_of(price), date) %>%
      mutate(price_pred = preds)
    
    df_test_food$price <- price
    rename_price_col <- which(str_detect(colnames(df_test_food), price))
    colnames(df_test_food)[rename_price_col] <- "price_true"
    
    df_test_food <- df_test_food[, c("date", "price", "price_pred", "price_true", 
                                     "food", "type", "size", "country")]
    
    # save prediction data
    
    food_preds <- rbind(food_preds, df_test_food)
    
    MAPE <- MAPE(preds, df_test_y)
    RMSE <- RMSE(preds, df_test_y)
    MAE <- MAE(preds, df_test_y)
    
    Sigma <- svm_tune$bestTune$sigma
    C <- svm_tune$bestTune$C
    
    fold_results <- as.data.frame(cbind(RMSE, MAE, MAPE, Sigma, C, 
                                        fold, price))
    
    results <- rbind(results, fold_results)

    }
  
  best_results <- results %>%
    group_by(price) %>%
    arrange(RMSE) %>%
    filter(row_number() == 1)
  
  perf_grid <- expand.grid(C = as.numeric(best_results$C), sigma = as.numeric(best_results$Sigma))
  
  # save model 

  svm_tune_perf <- train(
    x = df_train_x,
    y = df_train_y,
    method = "svmRadial",
    tuneGrid = perf_grid, 
    metric = "RMSE", 
    trControl = trainControl(method = "none")
  )
  
  saveRDS(svm_tune_perf, paste0("models/SVM/final_models/SVM_full_", price, ".rds"))
  
  
}

results <- results %>%
  dplyr::rename(model = price)

saveRDS(results, paste0("models/SVM/perf_results/SVM_tuning_full_", price, ".rds"))

# finally save predictions
# arrange

food_preds <- food_preds %>%
  arrange(food, date, price) 

saveRDS(food_preds, paste0("models/SVM/pred_results/SVM_full_", price, ".rds"))


