#%%%%%%%%%%%%%%%%%%%%%%%%#
# MAKING NEW PREDICTIONS #
#%%%%%%%%%%%%%%%%%%%%%%%%#


## by Lana Kern ##


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# This file includes a function to make predictions for the upcoming
# weeks for each food-type-size-country combination using the two BSTS models.
# Inputs:
  ## food_sel: selected fruit/vegetable
  ## type_sel: selected fruit/vegetable type
  ## size_sel: selected size of the selected fruit/vegetable
  ## country_sel: selected country where the fruit/vegetable originates from
  ## model_sel: model which should be used to make the predictions
    # Naming of models used within the functions:
    # BSTS
    # BSTS Baseline
  ## price_sel: market for which the prediction should be made:
    # choices are: price_B, price_F, price_H, price_M, price_K
    # (for Berlin, Frankfurt, Hamburg, Munich, and Köln)
  ## pred_horizon: number of weeks for which you want to make the predictions

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### FUNCTION: TIME SERIES MODEL ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# As the training time is relatively short, the model is directly trained
# within this function.
# In our analysis, we consider two different time series model
# 1.) Bayesian Structural Time Series Model without predictors (baseline)
# 2.) Bayesian Structural Time Series Model with predictors

# func_pred_bsts
func_price_predictions_bsts <- function(food_sel, type_sel, size_sel, country_sel,
                                        model_sel, price_sel, pred_horizon) {
  
  # load packages
  # library(dplyr)
  # library(stringr)
  # library(fastDummies)
  # library(bsts)
  # library(tsbox)
  
  ## Prepare Training Data ##
  
  # load machine learning data frame
  df_ML_raw <- readRDS("output/final_food_ML.rds")
  
  # subset on selected
  df_ML <- df_ML_raw %>%
    filter(food == food_sel, type == type_sel, size == size_sel,
           country_sel == country) 
  
  # define prices
  prices <- c("price_F", "price_H", "price_B", "price_M", 
              "price_K", "price_total")
  
  
  ## BSTS Baseline ##
  #%%%%%%%%%%%%%%%%%#
  
  if (model_sel == "BSTS Baseline") {
    
    # keep only date and selected price variable
    df_ML_price <- df_ML %>%
      dplyr::select(date, all_of(price_sel))
    
    # convert data as time series
    suppressMessages({
      series <- df_ML_price %>%
        ts_long() %>%
        ts_ts()
    })
    series <- series[!is.na(series)]
    
    # use a log transformation to make the model multiplicative
    y_b <- log10(series)
    
    # Run the bsts model
    ## add a local linear trend state component to an empty state specification
    ss <- AddLocalLinearTrend(list(), y_b)
    ## add a seasonal state component with 52 seasons to the state specification 
    ## created on the previous line
    ss <- AddSeasonal(ss, y_b, nseasons = 52)
    ## niter: number of markov chain monte carlo iterations
    ## seed to ensure reproducibility
    bsts.model <- bsts(y_b, state.specification = ss, niter = 500, 
                       ping = 0, seed = 42)
    
    # Get a suggested number of the size of the MCMC burn in sample as a proportion of the total run
    burn <- SuggestBurn(0.1, bsts.model)
    
    # predict for the next n weeks
    ## horizon: time points to be predicted
    p <- predict.bsts(bsts.model, horizon = pred_horizon, 
                      burn = burn, quantiles = c(.025, .975))
    
    ## extract predicted prices
    pred_prices <- 10^as.numeric(p$mean)
    
    # load new price data
    df_new_prices_final <- readRDS("output/price_data_updated_app.rds")
    df_new_prices_final <- df_new_prices_final %>% 
      filter(food == food_sel, type == type_sel, size == size_sel,
             country == country_sel) %>%
      dplyr::select(date, food, type, country, size, all_of(price_sel)) %>%
      dplyr::rename(price_true = all_of(price_sel)) %>%
      arrange(date) %>%
      head(pred_horizon)
    
    # create data frame with predictions
    df_pred <- data.frame(
      date = df_new_prices_final$date,
      price_pred = pred_prices,
      price = price_sel
    )
    
    # combine true and predicted prices
    df_pred_final <- inner_join(
      df_new_prices_final, df_pred, by = "date"
    )
    
    # add old predictions
    ## load all predictions
    # df_pred_old <- readRDS("models/BSTS/pred_results/bsts_pred_baseline.rds")
    # for (num in 2:5) {
    #   path_old <- paste0("models/BSTS/pred_results/bsts_pred_baseline_", num, ".rds")
    #   df_perf_old_add <- readRDS(path_old)
    #   df_pred_old <- rbind(df_pred_old, df_perf_old_add)
    # }
    df_pred_old <- readRDS("models/BSTS/pred_results/bsts_baseline_pred_food_model.rds")
    ## subset on selection
    df_pred_old <- df_pred_old %>% 
      filter(food == food_sel, type == type_sel, size == size_sel,
             country == country_sel, price == price_sel) %>%
      distinct() %>%
      dplyr::rename(
        price_true = actual,
        price_pred = pred
      )
    
    # combine
    df_pred_final <- rbind(df_pred_final, df_pred_old)
    
    # arrange by date
    df_pred_final <- rbind(df_pred_old, df_pred_final) %>%
      arrange(date)
    
    # round predictions
    df_pred_final$price_pred <- round(df_pred_final$price_pred)
    
    # prices per 100kg
    df_pred_final <- df_pred_final %>%
      mutate(
        price_true = price_true / 100,
        price_pred = price_pred / 100
      )
    
    # rename and sort columns
    df_pred_final <- df_pred_final %>%
      mutate(`price deviation` = round(price_true - price_pred, 2)) %>%
      dplyr::select(date, price, food, type, size, country, 
                    price_true, price_pred, everything()) %>%
      dplyr::rename(market = price, `true price` = price_true, 
                    `predicted price` = price_pred) 
    
    # return predictions
    return(df_pred_final)
    
    
    ## BSTS ##
    #%%%%%%%%#
  } else if (model_sel == "BSTS") {
    # create dummy variables
    # to do so, we first make the dummy variables as character variables
    # this deletes the factor levels
    # then we convert them back as factor in order to create the dummmies
    df_ML_reg <- df_ML
    df_ML_reg <- df_ML_reg %>% mutate_if(is.factor,as.character) # convert as character
    df_ML_reg[, c("type", "country", "size")] <- 
      lapply(df_ML_reg[, c("type", "country", "size")], as.factor) # convert back as factor
    # create dummies
    df_ML_reg <- dummy_cols(
      df_ML_reg,
      remove_first_dummy = FALSE,
      remove_selected_columns = FALSE, 
      select_columns = c("type", "country", "size")
    )
    
    # drop variables
    df_ML_reg <- df_ML_reg %>%
      # drop dummy variables which should not be included in the model
      dplyr::select(-starts_with("week"), -starts_with("month"), -starts_with("year")) %>%
      # drop price variables not selected
      dplyr::select(-c("food", "type", "country", "size", "price_total", 
                       all_of(prices[!prices %in% price_sel])))
    # drop lagged variables
    prices_drop <- colnames(df_ML_reg)[str_detect(
      colnames(df_ML_reg), paste(paste0(c(prices[!prices %in% price_sel], 
                                          "price_total"), "_lag"), 
                                 collapse = "|"))]
    df_ML_reg <- df_ML_reg %>%
      dplyr::select(-c(all_of(prices_drop)))
    # drop distances
    distances <- c("distance_f", "distance_h", "distance_m", "distance_b", "distance_k")
    market_letter <- str_to_lower(str_sub(price_sel, -1))
    distances_drop <- distances[str_sub(distances, -1) != market_letter]
    df_ML_reg <- df_ML_reg %>%
      dplyr::select(-c(all_of(distances_drop)))
    # drop constant variables
    col_constant_drop <-
      names(df_ML_reg[, sapply(df_ML_reg, function(v) var(v, na.rm = TRUE) == 0)])
    df_ML_reg <- df_ML_reg %>%
      dplyr::select(-all_of(col_constant_drop))
    
    # convert data as time series
    suppressMessages({
      series <- df_ML_reg %>%
        ts_long() %>%
        ts_ts()
    })
    # remove missing values 
    series <- data.frame(series) %>% 
      mutate_at(vars(all_of(price_sel)), ~log10(.)) %>%
      na.omit() %>% ts()
    
    # train the model
    ss <- AddLocalLinearTrend(list(), series[, price_sel])
    ss <- AddSeasonal(ss, series[, price_sel], nseasons = 52)
    bsts.model <- bsts(paste(price_sel, "~ ."), state.specification = ss, 
                       niter = 500, ping = 0, seed = 42, data = series)
    burn <- SuggestBurn(0.1, bsts.model)
    
    # make predictions using the new training data
    ## load new training data
    df_test_data <- readRDS("output/final_test_data.rds")
    ## rename date_pred in date
    df_test_data <- df_test_data %>% dplyr::rename(date = date_pred)
    ## keep only selected food-type-size-country combination
    df_test_data <- df_test_data %>%
      filter(food == food_sel, type == type_sel, size == size_sel,
             country_sel == country) 
    ## all columns as numeric
    df_test_data <- df_test_data %>% 
      as.data.frame() %>%
      mutate_at(vars(df_test_data %>% 
                       dplyr::select(-c(food, type, size, country, date)) %>% 
                       colnames()),
                as.numeric)
    ## keep only columns also used for model
    df_test_data <- df_test_data %>%
      dplyr::select(all_of(colnames(df_ML_reg)))
    ## arrange by date; extract test data for first week
    df_test_data_first_week <- df_test_data %>% arrange(date) %>% head(1)
    ## make predictions
    price_pred <- predict.bsts(bsts.model, horizon = 1, 
                               burn = burn, quantiles = c(.025, .975),
                               newdata = df_test_data_first_week)
    ## add predictions to data frame
    df_pred <- data.frame(week = 1, price_pred = 10^(price_pred$mean))
    
    ## if prediction_horizon is above one, iterate 
    ## in the iteration price_lag_one_week is replaced by prediction
    week_sel <- 1 # counter for week
    if (pred_horizon > 1) {
      # make predictions until selected pred_horizon is met
      while (week_sel < pred_horizon) {
        week_sel <- week_sel + 1
        df_test_data_week_next <- df_test_data %>% arrange(date) %>% dplyr::slice(week_sel)
        missing_cols <- 
          colnames(df_test_data_week_next)[colSums(is.na(df_test_data_week_next)) > 0]
        # lag from last week is replaced by prediction from last week
        df_test_data_week_next[, missing_cols[str_detect(missing_cols, "lag_one_week")]] <- 
          df_pred %>% filter(week == (week_sel - 1)) %>% 
          dplyr::select(price_pred) %>% pull()
        # make prediction
        price_pred <- predict.bsts(bsts.model, horizon = 1, 
                                   burn = burn, quantiles = c(.025, .975),
                                   newdata = df_test_data_week_next)
        # append to data frame
        df_pred_next <- data.frame(week = week_sel, 
                                   price_pred = 10^(price_pred$mean))
        df_pred <- rbind(df_pred, df_pred_next)
        
      }
    }
    
    
    # load previous predictions
    path_old_pred <- "models/BSTS/pred_results/bsts_regr_pred_food_model.rds"
    df_pred_old <- readRDS(path_old_pred)
    ## select
    df_pred_old <- df_pred_old %>%
      filter(food == food_sel, type == type_sel, size == size_sel,
             country == country_sel, price == price_sel) %>%
      arrange(date)
    
    # replace week variable by date
    df_pred$date <- df_test_data$date[1:pred_horizon]
    #df_pred <- df_pred %>% dplyr::select(-week)
    
    # append true prices
    df_new_prices_final <- readRDS("output/price_data_updated_app.rds")
    df_new_prices_final <- df_new_prices_final %>% 
      filter(food == food_sel, type == type_sel, size == size_sel,
             country == country_sel) %>%
      dplyr::select(date, food, type, country, size, all_of(price_sel)) %>%
      dplyr::rename(price_true = all_of(price_sel))
    
    df_pred_final <- inner_join(
      df_new_prices_final, df_pred, by = "date"
    )
    
    # extend prediction data frame for merge with previous predictions
    df_pred_final$price <- price_sel
    
    df_pred_final <- df_pred_final %>%
      dplyr::select(date, food, type, country, size, price, price_true, price_pred)
    
    df_pred_old <- df_pred_old %>%
      rename(price_true = actual, price_pred = pred) %>%
      dplyr::select(date, food, type, country, size, price, price_true, price_pred)
    
    df_pred_final <- rbind(df_pred_old, df_pred_final) %>%
      arrange(date)
    
    # round predictions
    df_pred_final$price_pred <- round(df_pred_final$price_pred)
    
    # prices per 100kg
    df_pred_final <- df_pred_final %>%
      mutate(
        price_true = price_true / 100,
        price_pred = price_pred / 100
      )
    
    # rename and sort columns
    df_pred_final <- df_pred_final %>%
      mutate(`price deviation` = round(price_true - price_pred, 2)) %>%
      dplyr::select(date, price, food, type, size, country, 
                    price_true, price_pred, everything()) %>%
      dplyr::rename(market = price, `true price` = price_true, 
                    `predicted price` = price_pred) 
    
    # return predictions
    return(df_pred_final)
    
  }
  
}
