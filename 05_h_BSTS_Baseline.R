#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Bayesian Structural Time Series Model ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# This file trains a bayesian structural time series (bsts) model for each
# food-type-size-country combination. This is feasible as the run time of the
# model is relatively small with around 3.5hours on the local computer. 
# Since otherwise for some food items many dummy variables are included
# this version seems to be appropriate. 
# In contrast to the machine learning models, no hyperparameters are tuned.
# Moreover, the cross validation works differently: train and test data cannot
# be chosen by taking random samples as it does not make sense at all to use
# values from the future to forecast values from the past. Thus, the temporal
# dependency between observations must be preserved. Precisely, cross-validation
# on a rolling basis is applied. I use all observations from 2016 & 2017
# as training data. Then, I make predictions for the first month of 2018.
# Afterwards, the training data is extended by this month. I move on until 
# today. 


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Track Time ####

start_time <- Sys.time()

#### Load Packages ####

# clear workspace
rm(list = ls())

# load packages
  ## bsts: to estimate the bayesian structural time series model
if (!require("bsts")) install.packages("bsts") 
library(bsts)
  ## for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
if (!require("tidyr")) install.packages("tidyr") 
library(tidyr)
  ## to work with time series
if (!require("tsbox")) install.packages("tsbox") 
library(tsbox)
  ## for performance measures
if (!require("tidymodels")) install.packages("tidymodels") 
library(tidymodels)
  ## to work with dates 
if (!require("lubridate")) install.packages("lubridate") 
library(lubridate)


#### Train all models ####


#### Baseline Model: No Regression Component ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# load machine learning data frame
df_ML_raw <- readRDS("output/final_ML.rds")

# keep only price variables and food, type, country, size information
df_ML_raw <- df_ML_raw %>%
  dplyr::select(date, food, type, country, size, matches("^price_[A-Z]$"))

# create data frame with all possible combinations
df_comb <- df_ML_raw %>%
  dplyr::select(food, type, country, size) %>%
  distinct()

# price vector
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

# data frame to store performance measures and predictions
df_perf_final <- data.frame()
df_pred_final <- data.frame()

# training data start
year_start <- c("2016", "2017")
year_end <- c("2020")
fold <- 0

# iterate over all combinations
for (i in 1:nrow(df_comb)) {
  
  # do so until data is available
  while ((as.numeric(year_start[length(year_start)]) + 1) <= year_end) {
    # count fold
    fold <- fold + 1
    
    # extract combination
    df_comb_sel <- df_comb[i, ]
    
    # select data
    df_ML <- df_ML_raw %>%
      filter(food == df_comb_sel$food & type == df_comb_sel$type & 
               country == df_comb_sel$country & size == df_comb_sel$size)
    
    # select indices for 2016 and 2017
    ## the years 2016 and 2017 are always used as training data
    row_names_training <- which(year(df_ML$date) %in% year_start)
    
    # test data are always one year more
    row_names_testing <- which(year(df_ML$date) %in% 
                                 (as.numeric(year_start[length(year_start)]) + 1))
    
    # iterate over all prices and store results in df_perf_prices
    df_perf_prices <- data.frame()
    
    for (price_sel in prices) {
      # select data: keep only selected price and date
      df_ML_price <- df_ML %>% 
        dplyr::select(date, all_of(price_sel))
      
      # convert data as time series
      series <- df_ML_price %>%
        ts_long() %>%
        ts_ts()
      series <- series[!is.na(series)]
      
      # use a log transformation to make the model multiplicative
      y_b <- log10(series)
      
      # extract training and testing data
      y_b_train <- y_b[row_names_training]
      y_b_test <- y_b[row_names_testing]
      
      # Run the bsts model
      ## add a local linear trend state component to an empty state specification
      ss <- AddLocalLinearTrend(list(), y_b_train)
      ## add a seasonal state component with 52 seasons to the state specification 
      ## created on the previous line
      ss <- AddSeasonal(ss, y_b_train, nseasons = 52)
      ## niter: number of markov chain monte carlo iterations
      ## seed to ensure reproducibility
      bsts.model <- bsts(y_b_train, state.specification = ss, niter = 500, ping = 0, seed = 42)
      
      # Get a suggested number of the size of the MCMC burn in sample as a proportion of the total run
      burn <- SuggestBurn(0.1, bsts.model)
      
      # predict
      ## horizon: time points to be predicted
      p <- predict.bsts(bsts.model, horizon = 52, burn = burn, quantiles = c(.025, .975))
      
      # Actual versus predicted
      df_pred <- data.frame(
        date = df_ML_price[row_names_testing, "date"],
        actual = df_ML_price[row_names_testing, ] %>% 
          dplyr::select(starts_with("price")) %>% pull(),
        pred = 10^as.numeric(p$mean),
        price = price_sel,
        food = df_comb_sel$food,
        type = df_comb_sel$type,
        size = df_comb_sel$size,
        country = df_comb_sel$country
      )
      # df_pred <- data.frame(
      #   date = df_ML_price$date,
      #   actual = df_ML_price %>% dplyr::select(starts_with("price")) %>% pull(),
      #   pred = c(10^as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),]) + y_b),  
      #            10^as.numeric(p$mean))
      # )
      
      # calculate performance measures
      df_perf <- data.frame(
        price = price_sel,
        food = df_comb_sel$food,
        type = df_comb_sel$type,
        size = df_comb_sel$size,
        country = df_comb_sel$country, 
        RMSE = yardstick::rmse(df_pred, truth = actual, estimate = pred) %>% 
          dplyr::select(.estimate) %>% pull(),
        MAPE = yardstick::mape(df_pred, truth = actual, estimate = pred) %>% 
          dplyr::select(.estimate) %>% pull(),
        MAE = yardstick::mae(df_pred, truth = actual, estimate = pred) %>% 
          dplyr::select(.estimate) %>% pull(),
        fold = fold
      )
      
      # add to data frame
      df_perf_prices <- rbind(df_perf_prices, df_perf)
      
      # append to final data frame
      df_perf_final <- rbind(df_perf_final, df_perf_prices)
      df_pred_final <- rbind(df_pred_final, df_pred)
      
    }
    # new training and testing data
    year_start <- c(year_start, as.numeric(year_start[length(year_start)]) + 1)
  }
  
  # print combination finished
  print(paste("Iteration", i))
}

# save data frame
saveRDS(df_perf_final, "output/time_series_simple_perf.rds")
saveRDS(df_pred_final, "output/time_series_simple_pred.rds")
  


# Final Model #
#%%

# Train final model using full data sequence
# this model is then used to make predictions



#### Bayesian Structural Time Series Model with Regression Component ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#




# end time tracking
Sys.time() - start_time
