##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany

#### Data Project - Machine Learning - naive mean prediction

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("modelr")) install.packages("modelr")
if (!require("tidymodels")) install.packages("tidymodels") 
if (!require("MLmetrics")) install.packages("MLmetrics")
if (!require("stringr")) install.packages("stringr")


library(dplyr)
library(tidyverse)
library(modelr)
library(tidymodels)
library(MLmetrics)
library(stringr)

# load machine learning data frame
df_ML_raw <- readRDS("output/final_ML.rds")

# select necessary columns for naive prediction
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

df_ML <- df_ML_raw %>% 
  select(all_of(prices), food, type, size, country, date)

# obtain market means and medians for all unique items
item_means <- df_ML %>%
  select(-date) %>%
  group_by(food, type, size, country) %>%
  mutate(mean_B = mean(price_B), median_B = median(price_B), 
         mean_F = mean(price_F), median_F = median(price_F), 
         mean_H = mean(price_H), median_H = median(price_H),
         mean_K = mean(price_K), median_K = median(price_K),
         mean_M = mean(price_M), median_M = median(price_M)
         ) %>% 
  select(-all_of(prices)) %>%
  distinct()

# complete data frame with predictions and true prices

naive_pred_data <- inner_join(df_ML, item_means, by = c("food", "type", "size", "country")) 
  
### Prepare Data Frame for Output

items <- unique(naive_pred_data$food)
results_mean <- data.frame()
results_median <- data.frame()

# obtain performance metrics for all the different items

for(item in items){
  for(price in prices){
    
    # filter for each item price
  rem <- prices[!prices %in% price]
  data <- naive_pred_data %>%
    select(-all_of(rem)) 
  
  # keep only true price and median/mean prediction
  
  col_num <- which(colnames(data) == price)
  market_letter <- str_sub(colnames(data), -1)[col_num]
  
  pred_cols <- colnames(data)[str_detect(colnames(data), market_letter)]
  
  # new data
  pred_data <- data %>%
    select(all_of(pred_cols))
  
  mean_name <- colnames(pred_data)[str_detect(colnames(pred_data), "mean")]
  
  median_name <- colnames(pred_data)[str_detect(colnames(pred_data), "median")]
  
  # mean prediction metrics 
  
  mape <- MAPE(pull(pred_data[, mean_name]), pull(pred_data[, col_num]))
  rmse <- RMSE(pull(pred_data[, mean_name]), pull(pred_data[, col_num]))
  mae <- MAE(pull(pred_data[, mean_name]), pull(pred_data[, col_num]))
  
  price_results <- as.data.frame(cbind(price, item, rmse, mae, mape))
  results_mean <- rbind(results_mean, price_results)
  
  
  # median prediction metrics
  
  mape <- MAPE(pull(pred_data[, median_name]), pull(pred_data[, col_num]))
  rmse <- RMSE(pull(pred_data[, median_name]), pull(pred_data[, col_num]))
  mae <- MAE(pull(pred_data[, median_name]), pull(pred_data[, col_num]))
  
  price_results <- as.data.frame(cbind(price, item, rmse, mae, mape))
  results_median <- rbind(results_median, price_results)
  
  
  }
}


# save metrics for both mean and median
saveRDS(results_mean, paste0("models/Naive/mean_metrics.rds"))
saveRDS(results_median, paste0("models/Naive/median_metrics.rds"))


# save prediction data
saveRDS(naive_pred_data, paste0("models/Naive/predictions.rds"))




