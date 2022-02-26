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
# In this file only the baseline time series model is trained. This model
# does not include any explanatory variables. Simply the prices are used.

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
  ## to create dummy variables
if (!require("fastDummies")) install.packages("fastDummies") 
library(fastDummies)
  ## for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)


#### Bayesian Structural Time Series Model with Regression Component ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# load machine learning data frame
df_ML_raw <- readRDS("output/final_food_ML.rds")

# keep only price variables and food, type, country, size information
# as regressors keep only temperature 
df_ML_raw <- df_ML_raw %>%
  # dplyr::select(date, food, type, country, size, 
  #               matches("^price_[A-Z]$"), matches("s_temp$"))
  dplyr::select(-starts_with("week"), -starts_with("month"), -starts_with("year"),
                -starts_with("food_"))

# food, type, size, and country as factor variables to remove class information
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)

# create data frame with all possible combinations
df_comb <- df_ML_raw %>%
  dplyr::select(food, type, country, size) %>%
  distinct()

# price vector
prices <- c("price_F", "price_H", "price_K", "price_M", "price_B")

# distance vectors
distances <- df_ML_raw %>% dplyr::select(starts_with("distance")) %>% colnames()

# data frame to store performance measures and predictions
df_perf_final <- data.frame()
df_pred_final <- data.frame()

# training data start
year_start <- c("2016")
pred_horizon <- 4

# iterate over all food-type-size-country combinationss
for (i in 1:nrow(df_comb)) {
  
  # extract combination
  df_comb_sel <- df_comb[i, ]
  
  # select data
  df_ML <- df_ML_raw %>%
    dplyr::filter(food == df_comb_sel$food, type == df_comb_sel$type,
                  country == df_comb_sel$country, size == df_comb_sel$size)
  
  # create dummy variables for type, size, and country
  # create dummy variables using type and country and size
  df_ML[, c("type", "country", "size")] <- 
    lapply(df_ML[, c("type", "country", "size")], as.factor)
  
  df_ML <- dummy_cols(
    df_ML,
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE, 
    select_columns = c("type", "country", "size")
  )
  
  # start of training indices
  row_names_training_start <- which(year(df_ML$date) %in% year_start)
  
  # start fold counting
  fold <- 0
  
  # do so until data is available
  # that is until at least 4 rows which have not been used as training so far
  # are included in the sample
  while (length(row_names_training_start) < (nrow(df_ML) - pred_horizon)) {
    
    # count fold
    fold <- fold + 1
    # select indices for training and test set
      ## training set
    row_names_training <- row_names_training_start
      ## test set: four weeks after training
    row_names_testing <- seq(row_names_training[length(row_names_training)] + 1, 
                             row_names_training[length(row_names_training)] + pred_horizon, 
                             1)
    
    # iterate over all prices and store results in df_perf_prices
    df_perf_prices <- data.frame()
    
    for (price_sel in prices) {
      # select data: drop food, type, country and size
      # drop other prices
      df_ML_price <- df_ML %>% 
        dplyr::select(-c("food", "type", "country", "size", 
                         all_of(prices[!prices %in% price_sel])))
      
      # drop lags
      # drop all lagged price variables except the ones for the selected price
      prices_drop <- colnames(df_ML_price)[str_detect(
        colnames(df_ML_price), paste(paste0(c(prices[!prices %in% price_sel], 
                                              "price_total"), "_lag"), 
                                     collapse = "|"))]
      df_ML_price <- df_ML_price %>%
        dplyr::select(-c(all_of(prices_drop)))
      
      # drop distances
        ## extract market letter
      market_letter <- str_to_lower(str_sub(price_sel, -1))
        ## identify distance variables to drop
      distances_drop <- distances[str_sub(distances, -1) != market_letter]
        ## drop distances
      df_ML_price <- df_ML_price %>%
        dplyr::select(-c(all_of(distances_drop)))
      
      # drop constant variables
      col_constant_drop <-
        names(df_ML_price[, sapply(df_ML_price, function(v) var(v, na.rm = TRUE) == 0)])
        ## but not selected price in case it is constant
      col_constant_drop <- col_constant_drop[!str_detect(price_sel, col_constant_drop)]
      df_ML_price <- df_ML_price %>%
        dplyr::select(-all_of(col_constant_drop))
      
      # convert data as time series
      suppressMessages({
        series <- df_ML_price %>%
          ts_long() %>%
          ts_ts()
      })
        ## remove missing values: only possible via data frame; then
        ## converted back as time series. 
        ## moreover price is log transformed
      series <- data.frame(series) %>% 
        mutate_at(vars(all_of(price_sel)), ~log10(.)) %>%
        na.omit() %>% ts()
      
      # extract training and testing data
      y_b_train <- series[row_names_training, ]
      y_b_test <- series[row_names_testing, ]
      
      # if standard deviation is zero, i.e., prices are constant add
      # random deviation
      if (sd(y_b_train[, price_sel]) == 0) {
        y_b_train[, price_sel] <- round(
          y_b_train[, price_sel] + runif(nrow(y_b_train))
        )
      } else {
        y_b_train <- y_b_train
      }
      
      # Run the bsts model
        ## set up priors
      # sdy <- sd(y_b_train)
      # sigma.prior <- SdPrior(sigma.guess = 0.01*sdy,
      #                        upper.limit = sdy,
      #                        sample.size = 16)
      # slope.sigma.prior <- SdPrior(sigma.guess = 0.01*sdy,
      #                              upper.limit = sdy,
      #                              sample.size = 16)
      # prior.spikes <- rep(0.1, ncol(y_b_train))
      # prior.mean <- rep(0, ncol(y_b_train))
      # prior <- SpikeSlabPrior(x = model.matrix(price_B ~ ., data = data.frame(y_b_train)), 
      #                         y = y_b_train[, price_sel], 
      #                         prior.information.weight = 200,
      #                         prior.inclusion.probabilities = prior.spikes,
      #                         optional.coefficient.estimate = prior.mean)
      
        ## add a local linear trend state component to an empty state specification
      ss <- AddLocalLinearTrend(state.specification = list(), 
                                y = y_b_train[, price_sel]#, sdy = sdy,
                                #level.sigma.prior = sigma.prior,
                                #slope.sigma.prior = slope.sigma.prior, 
                                #initial.y = y_b_train[1]
                                )
        ## add a seasonal state component with 52 seasons to the state specification created on the previous line
      ss <- AddSeasonal(ss, y_b_train[, price_sel], nseasons = 52#, sdy = sdy,
                        #sigma.prior = 0.01
                        )
        ## niter: number of markov chain monte carlo iterations
        ## seed to ensure reproducibility
      bsts.model <- bsts(paste(price_sel, "~ ."), state.specification = ss, 
                         niter = 500, ping = 0, seed = 42, 
                         data = data.frame(y_b_train))#, prior = prior)
      
      # Get a suggested number of the size of the MCMC burn in sample as a proportion of the total run
      burn <- SuggestBurn(0.1, bsts.model)
      
      # predict
        ## horizon: time points to be predicted
      p <- predict.bsts(bsts.model, horizon = pred_horizon, 
                        burn = burn, quantiles = c(.025, .975), 
                        newdata = data.frame(y_b_test))
      
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
    # new training indices
    row_names_training_start <- c(row_names_training_start, row_names_testing)
  }
  
  # print combination finished
  print(paste("Iteration", i))
}

# aggregate performance measures
df_perf_final <- df_perf_final %>%
  group_by(price, food, type, size, country) %>%
  dplyr::summarise(
    RMSE = mean(RMSE),
    MAPE = mean(MAPE),
    MAE = mean(MAE)
  )

# save data frame
saveRDS(df_perf_final, "models/BSTS/perf_results/time_series_regr_perf.rds")
saveRDS(df_pred_final, "models/BSTS/pred_results/time_series_regr_pred.rds")

