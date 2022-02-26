#%%%%%%%%%%%%%%%%%%%%%%#
# Prediction Analysis #
#%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this analysis the performance measures (RMSE, MAE, and MAPE) are 
# calculated and/or prepared accordingly for further analysis

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# load (or install if necessary) packages
  ## MLmetrics package is used to calculate the performance measures 
if (!require("MLmetrics")) install.packages("MLmetrics") 
library(MLmetrics)
  ## dplyr is used for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
  ## vip and pdp is used for variable importance
if (!require("vip")) install.packages("vip") 
library(vip)
if (!require("pdp")) install.packages("pdp") 
library(pdp)
  ## for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)
  ## to work with dates
if (!require("lubridate")) install.packages("lubridate") 
library(lubridate)
  ## for pivot_longer function
if (!require("tidyr")) install.packages("tidyr") 
library(tidyr)


#### LASSO, Ridge, Enet, XGBoost, and Random Forests ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# elastic net requires some small pre-preparation as food models were trained
# differently for each market
# elastic net is different as one food model per price is trained
df_enet_food <- data.frame()
df_enet_food_pred <- data.frame()

for (market in c("B", "F", "H", "K", "M")) {
  
  # file paths
  path_enet_food <- 
    paste0("models/ENET/perf_results/ENET_perf_food_model_price_", market, ".rds")
  
  path_enet_food_pred <- 
    paste0("models/ENET/pred_results/ENET_pred_food_model_price_", market, ".rds")
  
  # load
  df_enet_food_sub <- readRDS(path_enet_food)
  df_enet_food_pred_sub <- readRDS(path_enet_food_pred)
  
  # combine 
  df_enet_food <- rbind(df_enet_food, df_enet_food_sub)
  df_enet_food_pred <- rbind(df_enet_food_pred, df_enet_food_pred_sub)
}

saveRDS(df_enet_food, "models/ENET/perf_results/ENET_perf_food_model.rds")
saveRDS(df_enet_food_pred, "models/ENET/pred_results/ENET_pred_food_model.rds")


# define models and data frames to fill during loop
models <- c("LASSO", "RIDGE", "ENET", "XGBoost", "Random_Forest")
models_naming <- c("LASSO Regression", "Ridge Regression", "Elastic Net Regression",
                   "XGBoost", "Random Forests")

df_perf_overall <- data.frame()
df_perf_food <- data.frame()

# iterate over models to calculate performance measures
for (i in 1:length(models)) {
  
  ## overall performance measures ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # load data
  load_path_overall <- paste0("models/", models[i], "/perf_results/",
                           models[i], "_perf_food_model.rds")
  
  df_perf_overall_sub <- readRDS(load_path_overall)
  
  
  # calculate performance measures
  df_perf_overall_sub <-
    df_perf_overall_sub %>% 
    group_by(model) %>%
    dplyr::summarise(
      RMSE = mean(RMSE),
      MAE = mean(MAE),
      MAPE = mean(MAPE)
    ) %>%
    ungroup() 
  
  # may rename column and add model name
  if ("model" %in% colnames(df_perf_overall_sub)) {
    df_perf_overall_sub <- df_perf_overall_sub %>%
      dplyr::rename(price = model) %>%
      mutate(model = models_naming[i])
  } else {
    df_perf_overall_sub <- df_perf_overall_sub %>%
      mutate(model = models_naming[i])
  }
  
  # append to final data frame
  df_perf_overall <- rbind(df_perf_overall, df_perf_overall_sub)
  
  
  
  
  ## food-type-size-country performance measures ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # load data
  load_path_food <- paste0("models/", models[i], "/pred_results/", models[i],
                           "_pred_food_model.rds")
  
  df_perf_food_sub <- readRDS(load_path_food)
  
  # calculate performance measures across groups
  df_perf_food_sub <-
    df_perf_food_sub %>%
    group_by(price, food, type, size, country) %>%
    dplyr::summarise(
      RMSE = MLmetrics::RMSE(price_pred, price_true),
      MAE = MLmetrics::MAE(price_pred, price_true),
      MAPE = MLmetrics::MAPE(price_pred, price_true)
    ) %>%
    ungroup() 
  
  # may rename column and add model name
  if ("model" %in% colnames(df_perf_food_sub)) {
    df_perf_food_sub <- df_perf_food_sub %>%
      dplyr::rename(price = model) %>%
      mutate(model = models_naming[i])
  } else {
    df_perf_food_sub <- df_perf_food_sub %>%
      mutate(model = models_naming[i])
  }
  
  # append to final data frame
  df_perf_food <- rbind(df_perf_food, df_perf_food_sub)  
}



#### SVM and Neural Network ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

models_2 <- c("SVM", "NN")
models_naming_2 <- c("Support Vector Machines", "Neural Network")

df_perf_overall_2 <- data.frame()
df_perf_food_2 <- data.frame()

for (i in 1:length(models_2)) {
  
  ## overall performance measures ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # load data
  load_path_overall <- paste0("models/", models_2[i], "/perf_results/",
                              models_2[i], "_tuning_food_model.rds")
  
  df_perf_overall_sub <- readRDS(load_path_overall)
  
  
  # calculate performance measures
  df_perf_overall_sub <-
    df_perf_overall_sub %>% 
    group_by(model) %>%
    dplyr::summarise(
      RMSE = mean(RMSE),
      MAE = mean(MAE),
      MAPE = mean(MAPE) * 100
    ) %>%
    ungroup() 
  
  # may rename column and add model name
  if ("model" %in% colnames(df_perf_overall_sub)) {
    df_perf_overall_sub <- df_perf_overall_sub %>%
      dplyr::rename(price = model) %>%
      mutate(model = models_naming_2[i])
  } else {
    df_perf_overall_sub <- df_perf_overall_sub %>%
      mutate(model = models_naming_2[i])
  }
  
  # append to final data frame
  df_perf_overall_2 <- rbind(df_perf_overall_2, df_perf_overall_sub)
  
  
  
  
  ## food-type-size-country performance measures ##
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  # load data
  load_path_food <- paste0("models/", models_2[i], "/pred_results/", models_2[i],
                           "_pred_food_model.rds")
  
  df_perf_food_sub <- readRDS(load_path_food)
  
  # calculate performance measures across groups
  df_perf_food_sub <-
    df_perf_food_sub %>%
    group_by(price, food, type, size, country) %>%
    dplyr::summarise(
      RMSE = MLmetrics::RMSE(price_pred, price_true),
      MAE = MLmetrics::MAE(price_pred, price_true),
      MAPE = MLmetrics::MAPE(price_pred, price_true)
    ) %>%
    ungroup() 
  
  # may rename column and add model name
  if ("model" %in% colnames(df_perf_food_sub)) {
    df_perf_food_sub <- df_perf_food_sub %>%
      dplyr::rename(price = model) %>%
      mutate(model = models_naming_2[i])
  } else {
    df_perf_food_sub <- df_perf_food_sub %>%
      mutate(model = models_naming_2[i])
  }
  
  # append to final data frame
  df_perf_food_2 <- rbind(df_perf_food_2, df_perf_food_sub)  
}



#### Time Series Model ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

## yearly cv ##

# # bsts baseline
# bsts_perf_baseline_yearly <- 
#   readRDS("models/BSTS/perf_results/bsts_perf_baseline_yearly.rds")
# 
# for (num in 2:4) {
#   load_path_baseline_yearly <- paste0("models/BSTS/perf_results/bsts_perf_baseline_yearly_",
#                                  num, ".rds")
#   bsts_perf_baseline_yearly_add <- readRDS(load_path_baseline_yearly)
#   bsts_perf_baseline_yearly <- rbind(bsts_perf_baseline_yearly, bsts_perf_baseline_yearly_add)
# } 
# 
# bsts_perf_baseline_yearly_food <- 
#   bsts_perf_baseline_yearly %>% 
#   distinct() %>%
#   group_by(price, food, type, size, country) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS Baseline") %>%
#   dplyr::rename(market = price)
# 
# 
# bsts_perf_baseline_yearly_overall <-
#   bsts_perf_baseline_yearly %>%
#   group_by(price) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS Baseline") %>%
#   dplyr::rename(market = price)
# 
# 
# 
# # bsts with regressors
# bsts_perf_yearly <- 
#   readRDS("models/BSTS/perf_results/time_series_regr_perf_yearly.rds")
# 
# for (num in 2:4) {
#   load_path_reg_yearly <- paste0("models/BSTS/perf_results/time_series_regr_perf_yearly_",
#                                  num, ".rds")
#   bsts_perf_yearly_add <- readRDS(load_path_reg_yearly)
#   bsts_perf_yearly <- rbind(bsts_perf_yearly, bsts_perf_yearly_add)
# }
# 
# 
# bsts_perf_yearly_food <- 
#   bsts_perf_yearly %>% 
#   distinct() %>%
#   group_by(price, food, type, size, country) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS") 
# 
# bsts_perf_yearly_overall <-
#   bsts_perf_yearly %>%
#   group_by(price) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS Baseline") 


## monthly cv ##

# BSTS baseline
# bsts_perf_baseline_monthly <- 
#   readRDS("models/BSTS/perf_results/bsts_perf_baseline.rds")
# 
# for (num in 2:5) {
#   load_path_baseline_monthly <- paste0("models/BSTS/perf_results/bsts_perf_baseline_",
#                                   num, ".rds")
#   bsts_perf_baseline_monthly_add <- readRDS(load_path_baseline_monthly)
#   bsts_perf_baseline_monthly <- rbind(bsts_perf_baseline_monthly, bsts_perf_baseline_monthly_add)
# }
# 
# bsts_perf_baseline_monthly_food <- 
#   bsts_perf_baseline_monthly %>% 
#   distinct() %>%
#   group_by(price, food, type, size, country) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS Baseline")
# 
# bsts_perf_baseline_monthly_overall <- 
#   bsts_perf_baseline_monthly %>% 
#   distinct() %>%
#   group_by(price) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS Baseline")
# 
# 
# # BSTS with regressors
# bsts_perf_monthly <- 
#   readRDS("models/BSTS/perf_results/time_series_regr_perf.rds")
# 
# for (num in 2:5) {
#   load_path_reg_monthly <- paste0("models/BSTS/perf_results/time_series_regr_perf_",
#                                  num, ".rds")
#   bsts_perf_monthly_add <- readRDS(load_path_reg_monthly)
#   bsts_perf_monthly <- rbind(bsts_perf_monthly, bsts_perf_monthly_add)
# }
# 
# bsts_perf_monthly_food <- 
#   bsts_perf_monthly %>% 
#   distinct() %>%
#   group_by(price, food, type, size, country) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS")
# 
# bsts_perf_monthly_overall <-
#   bsts_perf_monthly %>%
#   group_by(price) %>%
#   dplyr::summarise(
#     RMSE = mean(RMSE), MAE = mean(MAE), MAPE = mean(MAPE)
#   ) %>%
#   mutate(model = "BSTS") 


## handle predictions ##

# baseline
df_pred_old <- readRDS("models/BSTS/pred_results/bsts_pred_baseline.rds")
for (num in 2:5) {
  path_old <- paste0("models/BSTS/pred_results/bsts_pred_baseline_", num, ".rds")
  df_perf_old_add <- readRDS(path_old)
  df_pred_old <- rbind(df_pred_old, df_perf_old_add)
}

df_pred_old_yearly <- readRDS("models/BSTS/pred_results/bsts_pred_baseline_yearly.rds")
for (num in 2:4) {
  path_old <- paste0("models/BSTS/pred_results/bsts_pred_baseline_yearly_", num, ".rds")
  df_perf_old_add <- readRDS(path_old)
  df_pred_old_yearly <- rbind(df_pred_old_yearly, df_perf_old_add)
}
df_pred_old_yearly <- df_pred_old_yearly %>%
  filter(year(date) == "2022")

df_pred_old <- rbind(df_pred_old, df_pred_old_yearly)

# adjust wrong true prices (don't know what happenend there and no time
# to train the models again)
df_pred_LASSO <- readRDS("models/LASSO/pred_results/LASSO_pred_food_model.rds") 

df_pred_old <- left_join(df_pred_old, 
                         df_pred_LASSO %>% dplyr::select(date, price_true, price, 
                                                         food, type, size, country))
df_pred_old <- df_pred_old %>%
  mutate(actual = price_true) %>%
  dplyr::select(-price_true)

saveRDS(df_pred_old, "models/BSTS/pred_results/bsts_baseline_pred_food_model.rds")

# regressors
df_pred_old <- readRDS("models/BSTS/pred_results/time_series_regr_pred.rds")
for (num in 2:5) {
  path_old <- paste0("models/BSTS/pred_results/time_series_regr_pred_", num, ".rds")
  df_perf_old_add <- readRDS(path_old)
  df_pred_old <- rbind(df_pred_old, df_perf_old_add)
}

df_pred_old_yearly <- readRDS("models/BSTS/pred_results/time_series_regr_pred_yearly.rds")
for (num in 2:4) {
  path_old <- paste0("models/BSTS/pred_results/time_series_regr_pred_yearly_", num, ".rds")
  df_perf_old_add <- readRDS(path_old)
  df_pred_old_yearly <- rbind(df_pred_old_yearly, df_perf_old_add)
}
df_pred_old_yearly <- df_pred_old_yearly %>%
  filter(year(date) == "2022")

df_pred_old <- rbind(df_pred_old, df_pred_old_yearly)

# adjust wrong true prices (don't know what happenend there and no time
# to train the models again)
df_pred_LASSO <- readRDS("models/LASSO/pred_results/LASSO_pred_food_model.rds") 

df_pred_old <- left_join(df_pred_old, 
                         df_pred_LASSO %>% dplyr::select(date, price_true, price, 
                                                         food, type, size, country))
df_pred_old <- df_pred_old %>%
  mutate(actual = price_true) %>%
  dplyr::select(-price_true)

saveRDS(df_pred_old, "models/BSTS/pred_results/bsts_regr_pred_food_model.rds")


# calculate performance measures
  ## baseline
df_pred_bsts_baseline <- 
  readRDS("models/BSTS/pred_results/bsts_baseline_pred_food_model.rds")
df_perf_bsts_baseline_overall <- 
  df_pred_bsts_baseline %>%
    group_by(price) %>%
    dplyr::summarise(
      RMSE = MLmetrics::RMSE(pred, actual),
      MAE = MLmetrics::MAE(pred, actual),
      MAPE = MLmetrics::MAPE(pred, actual) * 100
    ) %>%
  mutate(model = "BSTS Baseline")
df_perf_bsts_baseline_food <- 
  df_pred_bsts_baseline %>%
  group_by(price, food, type, size, country) %>%
  dplyr::summarise(
    RMSE = MLmetrics::RMSE(pred, actual),
    MAE = MLmetrics::MAE(pred, actual),
    MAPE = MLmetrics::MAPE(pred, actual) 
  ) %>%
  mutate(model = "BSTS Baseline")


  ## regression
df_pred_bsts <- 
  readRDS("models/BSTS/pred_results/bsts_regr_pred_food_model.rds")
df_perf_bsts_overall <- 
  df_pred_bsts %>%
  group_by(price) %>%
  dplyr::summarise(
    RMSE = MLmetrics::RMSE(pred, actual),
    MAE = MLmetrics::MAE(pred, actual),
    MAPE = MLmetrics::MAPE(pred, actual) * 100
  ) %>%
  mutate(model = "BSTS")
df_perf_bsts_food <- 
  df_pred_bsts %>%
  group_by(price, food, type, size, country) %>%
  dplyr::summarise(
    RMSE = MLmetrics::RMSE(pred, actual),
    MAE = MLmetrics::MAE(pred, actual),
    MAPE = MLmetrics::MAPE(pred, actual) 
  ) %>%
  mutate(model = "BSTS")



#### Naive Prediction ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# load data and keep only necessary information
df_naive <- readRDS("models/Naive/predictions.rds")

# restructure data set
df_naive_true <- df_naive %>%
  dplyr::select(date, food, type, size, country, starts_with("price")) %>%
  pivot_longer(c(price_B, price_F, price_M, price_H, price_K), 
               names_to = "price", values_to = "price_true")

df_naive_mean <- df_naive %>%
  dplyr::select(date, food, type, size, country, starts_with("mean")) %>%
  dplyr::rename(
    price_B = mean_B, price_F = mean_F, price_M = mean_M,
    price_H = mean_H, price_K = mean_K
  ) %>%
  pivot_longer(c(price_B, price_F, price_M, price_H, price_K), 
               names_to = "price", values_to = "price_pred_mean")


df_naive_median <- df_naive %>%
  dplyr::select(date, food, type, size, country, starts_with("median")) %>%
  dplyr::rename(
    price_B = median_B, price_F = median_F, price_M = median_M,
    price_H = median_H, price_K = median_K
  ) %>%
  pivot_longer(c(price_B, price_F, price_M, price_H, price_K), 
               names_to = "price", values_to = "price_pred_median")


df_naive_mean <- left_join(df_naive_true, df_naive_mean,
                           by = c("date", "food", "type", "size", 
                                  "country", "price"))

df_naive_median <- left_join(df_naive_true, df_naive_median,
                             by = c("date", "food", "type", "size", 
                                    "country", "price"))


# calculate overall error metrics for naive prediction
df_naive_error_mean_overall <- 
  left_join(
    df_naive_mean %>%
      group_by(price) %>%
      yardstick::rmse(truth = price_true, estimate = price_pred_mean) %>%
      dplyr::select(price, .estimate) %>%
      rename(RMSE = .estimate),
    df_naive_mean %>%
      group_by(price) %>%
      yardstick::mae(truth = price_true, estimate = price_pred_mean) %>%
      dplyr::select(price, .estimate) %>%
      rename(MAE = .estimate),
    by = "price"
  ) %>%
  left_join(
    df_naive_mean %>%
      group_by(price) %>%
      yardstick::mape(truth = price_true, estimate = price_pred_mean) %>%
      dplyr::select(price, .estimate) %>%
      rename(MAPE = .estimate),
    by = "price"
  ) %>%
  mutate(model = "Naive Mean Prediction") %>% 
  dplyr::select(price, RMSE, MAE, MAPE, model)


df_naive_error_median_overall <- 
  left_join(
    df_naive_median %>%
      group_by(price) %>%
      yardstick::rmse(truth = price_true, 
                      estimate = price_pred_median) %>%
      dplyr::select(price, .estimate) %>%
      rename(RMSE = .estimate),
    df_naive_median %>%
      group_by(price) %>%
      yardstick::mae(truth = price_true, 
                     estimate = price_pred_median) %>%
      dplyr::select(price, .estimate) %>%
      rename(MAE = .estimate),
    by = "price"
  ) %>%
  left_join(
    df_naive_median %>%
      group_by(price) %>%
      yardstick::mape(truth = price_true, 
                      estimate = price_pred_median) %>%
      dplyr::select(price, .estimate) %>%
      rename(MAPE = .estimate),
    by = "price"
  ) %>% 
  mutate(model = "Naive Median Prediction") %>% 
  dplyr::select(price, RMSE, MAE, MAPE, model)



# calculate detailed error metrics for naive prediction
df_naive_error_mean_detail <- 
  left_join(
    df_naive_mean %>%
      group_by(price, food, type, size, country) %>%
      yardstick::rmse(truth = price_true, estimate = price_pred_mean) %>%
      dplyr::select(price, food, type, size, country, .estimate) %>%
      dplyr::rename(RMSE = .estimate),
    df_naive_mean %>%
      group_by(price, food, type, size, country) %>%
      yardstick::mae(truth = price_true, estimate = price_pred_mean) %>%
      dplyr::select(price, food, type, size, country, .estimate) %>%
      dplyr::rename(MAE = .estimate),
    by = c("price", "food", "type", "size", "country")
  ) %>%
  left_join(
    df_naive_mean %>%
      group_by(price, food, type, size, country) %>%
      yardstick::mape(truth = price_true, estimate = price_pred_mean) %>%
      dplyr::select(price, food, type, size, country, .estimate) %>%
      dplyr::rename(MAPE = .estimate),
    by = c("price", "food", "type", "size", "country")
  ) %>%
  mutate(model = "Naive Mean Prediction") %>% 
  mutate(MAPE = MAPE / 100) %>%
  dplyr::select(price, food, type, size, country, RMSE, MAE, MAPE, model)



df_naive_error_median_detail <- 
  left_join(
    df_naive_median %>%
      group_by(price, food, type, size, country) %>%
      yardstick::rmse(truth = price_true, estimate = price_pred_median) %>%
      dplyr::select(price, food, type, size, country, .estimate) %>%
      dplyr::rename(RMSE = .estimate),
    df_naive_median %>%
      group_by(price, food, type, size, country) %>%
      yardstick::mae(truth = price_true, estimate = price_pred_median) %>%
      dplyr::select(price, food, type, size, country, .estimate) %>%
      dplyr::rename(MAE = .estimate),
    by = c("price", "food", "type", "size", "country")
  ) %>%
  left_join(
    df_naive_median %>%
      group_by(price, food, type, size, country) %>%
      yardstick::mape(truth = price_true, estimate = price_pred_median) %>%
      dplyr::select(price, food, type, size, country, .estimate) %>%
      dplyr::rename(MAPE = .estimate),
    by = c("price", "food", "type", "size", "country")
  ) %>%
  mutate(model = "Naive Median Prediction") %>%
  mutate(MAPE = MAPE / 100) %>%
  dplyr::select(price, food, type, size, country, RMSE, MAE, MAPE, model)


#### Combine and Save ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# combine
  ## overall
df_perf_overall <- rbind(df_perf_overall, df_perf_overall_2)
df_perf_overall <- rbind(df_perf_overall, df_naive_error_mean_overall)
df_perf_overall <- rbind(df_perf_overall, df_naive_error_median_overall)
df_perf_overall <- rbind(df_perf_overall, df_naive_error_median_overall)
df_perf_overall <- rbind(df_perf_overall, df_perf_bsts_baseline_overall)
df_perf_overall <- rbind(df_perf_overall, df_perf_bsts_overall)
  ## food-type-size-country
df_perf_food <- rbind(df_perf_food, df_perf_food_2)
df_perf_food <- rbind(df_perf_food, df_naive_error_mean_detail)
df_perf_food <- rbind(df_perf_food, df_naive_error_median_detail)
df_perf_food <- rbind(df_perf_food, df_perf_bsts_baseline_food)
df_perf_food <- rbind(df_perf_food, df_perf_bsts_food)

# some more adjustments
df_perf_overall <- df_perf_overall %>%
  ## show results for 1kg instead 100kg
  ## MAPE is only divided for conversion later with function label_percent
  mutate_at(vars("RMSE", "MAE", "MAPE"), ~./100) %>%
  ## rename price to market
  dplyr::rename(market = price) %>%
  ## replace markets
  mutate(
    market = ifelse(
      market == "price_F", "Frankfurt",
      ifelse(
        market == "price_B", "Berlin", 
        ifelse(
          market == "price_M", "Munich",
          ifelse(market == "price_K", "Cologne",
                 ifelse(market == "price_H", "Hamburg", market))
        )
      )
    )
  )

saveRDS(df_perf_overall, "output/perf_metric_overall.rds")


# modifications
df_perf_food <- df_perf_food %>%
  ## convert in 1kg instead 100kg prices; MAPE is not converted since percentage change remains the same
  mutate_at(vars("RMSE", "MAE"), ~./100) %>%
  ## rename price to market
  dplyr::rename(market = price) %>%
  ## replace markets
  mutate(
    market = ifelse(
      market == "price_F", "Frankfurt",
      ifelse(
        market == "price_B", "Berlin", 
        ifelse(
          market == "price_M", "Munich",
          ifelse(market == "price_K", "Cologne",
                 ifelse(market == "price_H", "Hamburg", market))
        )
      )
    )
  )


saveRDS(df_perf_food, "output/perf_metric_final.rds")


#### Preparation for Hyperparameter Analysis ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# for the following models, the full models were trained for each market
# separately in order to save computation time. 

## XGBoost ##
XGB_tuning_full_model <- data.frame()
for (market in c("B", "F", "H", "K", "M")) {
  
  # generate file path
  xgb_full_path <- 
    paste0("models/XGBoost/perf_results/XGBoost_perf_full_model_", market, ".rds")
  
  # generate data frame name
  # svm_df_name <- paste0("df_full_", market)
  
  # read in file path and append this to data frame
  # assign(svm_df_name, readRDS(svm_full_path))
  XGB_tuning_full_model <- rbind(XGB_tuning_full_model, readRDS(xgb_full_path))
}

saveRDS(XGB_tuning_full_model, 
        "models/XGBoost/perf_results/XGBoost_perf_full_model.rds")


## Random Forests ##
RF_tuning_full_model <- data.frame()
for (market in c("B", "F", "H", "K", "M")) {
  
  # generate file path
  rf_full_path <- 
    paste0("models/Random_Forest/tuning_results/rf_tuning_result_", market, ".rds")
  
  # generate data frame name
  # svm_df_name <- paste0("df_full_", market)
  
  # read in file path and append this to data frame
  # assign(svm_df_name, readRDS(svm_full_path))
  RF_tuning_full_model <- rbind(RF_tuning_full_model, readRDS(rf_full_path))
}

saveRDS(RF_tuning_full_model, 
        "models/Random_Forest/perf_results/Random_Forest_perf_full_model.rds")



## SVM ##
SVM_tuning_full_model <- data.frame()
for (market in c("B", "F", "H", "K", "M")) {
  
  # generate file path
  svm_full_path <- 
    paste0("models/SVM/perf_results/SVM_tuning_full_price_", market, ".rds")
  
  # generate data frame name
  # svm_df_name <- paste0("df_full_", market)
  
  # read in file path and append this to data frame
  # assign(svm_df_name, readRDS(svm_full_path))
  SVM_tuning_full_model <- rbind(SVM_tuning_full_model, readRDS(svm_full_path))
}

SVM_tuning_full_model <- SVM_tuning_full_model %>%
  mutate(
    RMSE = as.numeric(RMSE),
    MAE = as.numeric(MAE),
    MAPE = as.numeric(MAPE)
  )

saveRDS(SVM_tuning_full_model, 
        "models/SVM/perf_results/SVM_perf_full_model.rds")



## Neural Network ##
NN_tuning_full_model <- data.frame()
for (market in c("B", "F", "H", "K", "M")) {
  
  # generate file path
  NN_full_path <- 
    paste0("models/NN/perf_results/NN_tuning_full_price_", market, ".rds")
  
  # generate data frame name
  # svm_df_name <- paste0("df_full_", market)
  
  # read in file path and append this to data frame
  # assign(svm_df_name, readRDS(svm_full_path))
  NN_tuning_full_model <- rbind(NN_tuning_full_model, readRDS(NN_full_path))
}

saveRDS(NN_tuning_full_model, 
        "models/NN/perf_results/NN_perf_full_model.rds")



#### Error Metrics Full Model ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# for the full model only overall error metrics are calculated
models_full <- c("LASSO", "RIDGE", "ENET", "XGBoost", "SVM",
                 "NN", "Random_Forest")
models_full_names <- c("LASSO Regression", "Ridge Regression", "Elastic Net Regression",
                       "XGBoost", "Support Vector Machines", "Neural Network",
                       "Random Forests")

df_overall_full <- data.frame()
for (i in 1:length(models_full)) {
  # generate file path
  load_path_full <- paste0("models/", models_full[i], "/perf_results/",
                           models_full[i], "_perf_full_model.rds")
  
  # if file path exists, load data and calculate overall error metrics
  if (file.exists(load_path_full)) {
    # for SVM and NN MAPE needs not be divided by 100
    if (any(c("SVM", "NN") %in% models_full[i])) {
      df_full <- readRDS(load_path_full) %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 2) / 100, 
          MAE = round(mean(MAE), 2) / 100,
          MAPE = round(mean(MAPE), 2) 
        ) %>%
        dplyr::rename(market = model) %>%
        mutate(model = models_full_names[i])
    } else {
      df_full <- readRDS(load_path_full) %>%
        group_by(model) %>%
        dplyr::summarise(
          RMSE = round(mean(RMSE), 2) / 100, 
          MAE = round(mean(MAE), 2) / 100,
          MAPE = round(mean(MAPE), 2) / 100
        ) %>%
        dplyr::rename(market = model) %>%
        mutate(model = models_full_names[i])
    }

  } else {
    df_full <- data.frame()
  }
  
  # append to final data frame
  df_overall_full <- rbind(df_overall_full, df_full)
}

# replace market names
df_overall_full <- df_overall_full %>%
  mutate(
    market = ifelse(
      market == "price_F", "Frankfurt",
      ifelse(
        market == "price_B", "Berlin", 
        ifelse(
          market == "price_M", "Munich",
          ifelse(market == "price_K", "Cologne",
                 ifelse(market == "price_H", "Hamburg", market))
        )
      )
    )
  )

# save 
saveRDS(df_overall_full, "output/perf_metric_overall_full.rds")

















