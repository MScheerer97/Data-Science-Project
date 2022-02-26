#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Prediction Price Recommendation #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this file, the price predictions for the upcoming four week sare made.
# Those predictions are used to generate the price recommendation.
# The function to make predictions is stored in the file 
# "func_price_predictions_shiny.R"
# It takes the following arguments in the following order:
# food, type, size, country, model, market, horizon
# To make predictions for each food-type-size-country combination, we iterate
# over all possible combinations. Moreover, we iterate over all markets.
# For each food item, the algorithm is chosen which produces the best 
# predictions.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### General Setup ####
#%%%%%%%%%%%%%%%%%%%%%#

# clear workspace
rm(list = ls())

# load packages
  ## tidymodels framework is used for machine learning
if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)
  ## glmnet is used to fit the regularized regressions
if (!require("glmnet")) install.packages("glmnet") 
library(glmnet)
  ## xgboost is used to fit the regularized regressions
if (!require("xgboost")) install.packages("xgboost") 
library(xgboost)
  ## randomForest is used to fit the regularized regressions
if (!require("randomForest")) install.packages("randomForest") 
library(randomForest)
  ## RSNNS is used to fit the neural network
if (!require("RSNNS")) install.packages("RSNNS") 
library(RSNNS)
  ## for BSTS models
if (!require("bsts")) install.packages("bsts") 
library(bsts)
if (!require("tsbox")) install.packages("tsbox") 
library(tsbox)
  ## dplyr for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
  ## stringr for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)
  ## for dummy variable creation
if (!require("fastDummies")) install.packages("fastDummies") 
library(fastDummies)
  ## data.table to get best model
if (!require("data.table")) install.packages("data.table") 
library(data.table)

# track time
start_time <- Sys.time()

# load functions
source("func_price_predictions_ml.R")
source("func_price_predictions_bsts.R")

# load data for combinations
df_descr <- readRDS("output/df_descriptives.rds") 
## extract combinations
df_comb <- df_descr %>%
  dplyr::select(food, type, size, country) %>%
  distinct()

# predictions is only done for next week
week_sel <- 4

# markets
markets <- c("price_F", "price_H", "price_K", "price_M", "price_B")

# define models: best model for each food combination is used
## best model for each market for each food-type-size-country combination
## however to do so, we drop Naive Predictions
## we also drop BSTS as they sometimes produce unrealistic results
df_perf_food <- readRDS("output/perf_metric_final.rds") %>%
  filter(!model %in% c("Naive Mean Prediction", "Naive Median Prediction")) %>%
  filter(!model %in% c("BSTS", "BSTS Baseline"))
## extract model with smallest RMSE
df_comb_best_model <- 
  unique(setDT(df_perf_food)[order(RMSE)], 
         by = c("market", "food", "type", "size", "country")) %>%
  as.data.frame()



#### Make predictions ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

# data frame where all prediction results are stored
df_pred_all <- data.frame() 

# iterate over food-type-size-country combinations
for (i in 1:nrow(df_comb)) {
  
  # extract combination for this iteration
  df_comb_sub <- df_comb[i, ]
  
  # select models for this combination across markets
  df_model_food <- df_comb_best_model %>%
    filter(food == df_comb_sub$food, type == df_comb_sub$type, size ==  df_comb_sub$size,
           country == df_comb_sub$country)
  
  # iterate over markets
  df_pred_markets <- data.frame()
  for (markets_sel in markets) {
    # replace markets_sel
    markets_sel_2 <- 
      ifelse(
        markets_sel == "price_F", "Frankfurt",
        ifelse(markets_sel == "price_H", "Hamburg",
               ifelse(markets_sel == "price_K", "Cologne",
                      ifelse(markets_sel == "price_B", "Berlin",
                             ifelse(markets_sel == "price_M", "Munich", 
                                    markets_sel
                             )))))
    
    # extract best model for specific market
    model_sel <- df_model_food %>%
      filter(market == markets_sel_2) %>%
      dplyr::select(model) %>%
      pull()
    
    # replace names so that they work within function
    model_sel <- 
      ifelse(model_sel == "LASSO Regression", "LASSO",
             ifelse(model_sel == "Ridge Regression", "RIDGE",
                    ifelse(model_sel == "Elastic Net Regression", "ENET",
                      ifelse(model_sel == "Random Forests", "Random_Forest",
                           ifelse(model_sel == "Neural Network", "NN",
                                  ifelse(model_sel == "Support Vector Machines", 
                                         "SVM", model_sel))))))
    
    
    if (any(c("BSTS Baseline", "BSTS") %in% model_sel)) {
      # iterate over selected markets and models
      df_pred_sub <-
        # use function to make predictions for selected food-type-size-country combination
        # and selected market
        func_price_predictions_bsts(df_comb_sub$food, df_comb_sub$type, df_comb_sub$size,
                       df_comb_sub$country, model_sel, markets_sel, week_sel) %>%
        # arrange by date and keep last row as this is the results for the predicted week
        # the function actually returns the whole time series (from Jan. 2016 until
        # next weeks predictions)
        arrange(date) %>% tail(week_sel) %>% 
        # add model
        mutate(model = model_sel)
    } else {
      # make predictions for this combination
      df_pred_sub <- 
        # use function to make predictions for selected food-type-size-country combination
        # and selected market
        func_price_predictions_ml(df_comb_sub$food, df_comb_sub$type, df_comb_sub$size,
                           df_comb_sub$country, model_sel, markets_sel, week_sel) %>%
        # arrange by date and keep last row as this is the results for the predicted week
        # the function actually returns the whole time series (from Jan. 2016 until
        # next weeks predictions)
        arrange(date) %>% tail(week_sel) %>% 
        # add model
        mutate(model = model_sel)
    }
    
    # append 
    df_pred_markets <- rbind(df_pred_markets, df_pred_sub)
  }
  
  # append to total data frame
  df_pred_all <- rbind(df_pred_all, df_pred_markets)
  
  # track progress
  print(paste("Prediction finished for combination", i))
}


#### Save Data Frame ####
#%%%%%%%%%%%%%%%%%%%%%%%#

# keep only variables of interest
df_pred_all <- df_pred_all %>%
  dplyr::select(date, market, food, type, size, country, `predicted price`, model)

# save data frame
saveRDS(df_pred_all, "output/food_basket_price_recommendation.rds")

# end time
Sys.time() - start_time
