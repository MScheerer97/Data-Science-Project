#%%%%%%%%%%%%%%%%%%%%#
# Update Predictions #
#%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this file the price predictions are updated from the 17th January 2022
# onwards, or rather from calender week 3 in 2022.
# This is done as the last price predictions from cross validation are obtained
# in calender week 03 in 2022. As the models are not updated in the near
# feature the price predictions are made here for every model.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### General Setup ####

# load packages
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
if (!require("lubridate")) install.packages("lubridate") 
library(lubridate)
if (!require("stringr")) install.packages("stringr") 
library(stringr)
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
if (!require("bsts")) install.packages("bsts") 
library(bsts)
if (!require("tsbox")) install.packages("tsbox") 
library(tsbox)
if (!require("fastDummies")) install.packages("fastDummies") 
library(fastDummies)

# track time
start_time <- Sys.time()

# load function
source("func_price_predictions_ml.R")
source("func_price_predictions_bsts.R")
source("func_dummy_variables.R")

# load data for combinations
df_descr <- readRDS("output/df_descriptives.rds") 
## extract combinations
df_comb <- df_descr %>%
  dplyr::select(food, type, size, country) %>%
  distinct()

# predictions are only done for next week (so this file is updated weekly)
week_pred <- 1

# markets
markets <- c("price_F", "price_H", "price_K", "price_M", "price_B")



#### BSTS ####
#%%%%%%%%%%%%#


bsts_model_sel <- c("BSTS", "BSTS Baseline")


# iterate over markets, models, and, combinations
for (model_pred in bsts_model_sel) {
  
  # establish new data frame for predictions
  df_pred_update <- data.frame()
  
  for (market_pred in markets) {
    for (i in 1:nrow(df_comb)) {
      
      # extract combination
      df_comb_sub <- df_comb[i, ]
      
      # make predictions
      df_pred_sub <- 
        func_price_predictions_bsts(
          df_comb_sub$food, df_comb_sub$type, df_comb_sub$size,
          df_comb_sub$country, model_pred, market_pred, week_pred) %>%
        # arrange by date 
        arrange(date) %>%
        # add model
        mutate(model = model_pred)
      
      
      # append
      df_pred_update <- rbind(df_pred_update, df_pred_sub)
    }
  }
  
  ## save ##
  
  # save predictions
  if (model_sel == "BSTS") {
    load_path <- paste0("models/BSTS/pred_results/bsts_regr_pred_food_model.rds")
  } else {
    load_path <- paste0("models/BSTS/pred_results/bsts_pred_baseline.rds")
  }

  
  print(unique(df_pred_update$model))
  #saveRDS(df_pred_sub, save_path)
}


#### Machine Learning and Neural Network ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

model_sel <- c("SVM", "XGBoost", "Random_Forest", "NN", "LASSO", "RIDGE", "ENET")

# iterate over markets, models, and, combinations
for (model_pred in model_sel) {
  
  # establish new data frame for predictions
  df_pred_update <- data.frame()
  
  for (market_pred in markets) {
    for (i in 1:nrow(df_comb)) {
      
      # extract combination
      df_comb_sub <- df_comb[i, ]
      
      # make predictions
      df_pred_sub <- 
        func_price_predictions_ml(df_comb_sub$food, df_comb_sub$type, df_comb_sub$size,
                                  df_comb_sub$country, model_pred, 
                                  market_pred, week_pred) %>%
          # arrange by date 
          arrange(date) %>%
          # add model
          mutate(model = model_pred)
      

      # append
      df_pred_update <- rbind(df_pred_update, df_pred_sub)
    }
  }
  
  ## save ##
  
  # save predictions
  load_path <- paste0("models/", model_sel, 
                      "/pred_results/", model_sel, "_pred_food_model.rds")
  
  print(unique(df_pred_update$model))
  #saveRDS(df_pred_sub, save_path)
}