#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# MAKING NEW PREDICTIONS USING MACHINE LEARNING #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## by Lana Kern ##


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# This file includes a function to make predictions for the upcoming
# weeks for each food-type-size-country combination using all machine 
# learning models.  
# Inputs:
  ## food_sel: selected fruit/vegetable
  ## type_sel: selected fruit/vegetable type
  ## size_sel: selected size of the selected fruit/vegetable
  ## country_sel: selected country where the fruit/vegetable originates from
  ## model_sel: model which should be used to make the predictions
    # Naming of models used within the functions:
    # XGBoost
    # LASSO
    # RIDGE
    # ENET
    # SVM
    # NN
    # Random_Forest
  ## price_sel: market for which the prediction should be made:
    # choices are: price_B, price_F, price_H, price_M, price_K
    # (for Berlin, Frankfurt, Hamburg, Munich, and Köln)
  ## pred_horizon: number of weeks for which you want to make the predictions

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### Source: Dummy Variable Function ####

source("func_dummy_variables.R")


#### FUNCTION: MAKE PREDICTIONS FOR FOOD MODELS ####

# From the general test data set above, the test data set used to make the
# prediction is generated according to the user input in the R Shiny App.
# The user can select between:
## food type, type, size, country
## model
## market or rather price
## prediction horizon: 1, 2, 3, or 4 (weeks)

# func_pred_new_data
func_price_predictions_ml <- 
  function(food_sel, type_sel, size_sel, country_sel,
           model_sel, price_sel, pred_horizon) {
    
    # needed packages
    # library(dplyr)
    # library(stringr)
    # library(lubridate)
    # library(tidymodels)
    # library(glmnet)
    # library(randomForest)
    # library(xgboost)
    # library(RSNNS)
    # library(kernlab)
    # library(fastDummies)
    
    # load data 
    df_test_data <- readRDS("output/final_test_data.rds")
    
    # subset on selected food, type, size, and country 
    df_test_data_sub <- df_test_data %>%
      filter(food == food_sel, type == type_sel, size == size_sel,
             country == country_sel)
    
    # drop all current price variables
    prices <-
      c("price_total", "price_F", "price_M", "price_H", "price_B", "price_K")
    df_test_data_sub <- df_test_data_sub %>% dplyr::select(-all_of(prices))
    
    # drop lagged price variables of not select markets
    prices_drop <- colnames(df_test_data_sub)[str_detect(
      colnames(df_test_data_sub), paste(paste0(c(prices[!prices %in% price_sel],
                                                 "price_total"), "_lag"),
                                        collapse = "|"))]
    df_test_data_sub <- df_test_data_sub %>%
      dplyr::select(-c(all_of(prices_drop)))
    
    # drop distances not belonging to selected market
    distances <-
      df_test_data_sub %>% dplyr::select(starts_with("distance")) %>% colnames()
    market_letter <- str_to_lower(str_sub(price_sel, -1))
    distances_drop <- distances[str_sub(distances, -1) != market_letter]
    df_test_data_sub <- df_test_data_sub %>%
      dplyr::select(-c(all_of(distances_drop)))
    
    # # subset on selected prediction horizon (number of predicted weeks; 1-4)
    df_test_data_sub <- df_test_data_sub %>%
      arrange(date_pred) %>%
      head(pred_horizon)
    
    # drop date
    date_pred <- df_test_data_sub$date_pred
    df_test_data_sub <- df_test_data_sub %>% dplyr::select(-date_pred)
    
    # generate dummy variables for type, size and country 
    df_dummies <- 
      func_dummy_variables(food_sel, type_sel, size_sel, country_sel)
    ## dummies are merged
    df_test_data_sub <- left_join(
      df_test_data_sub, df_dummies, by = "food"
    )
    ## drop variables that are dummies now
    ## drop food variable as it is not used for prediction
    df_test_data_sub <- df_test_data_sub %>%
      dplyr::select(-c(food, type, country, size))
    
    # for the NN and SVM model the whitespaced need to be replaced by a "_". 
    # Moreover, apostrophs need to be replaced by an underscore too.
    if (model_sel %in% c("NN", "SVM")) {
      colnames(df_test_data_sub) <- sub(" ", "_", colnames(df_test_data_sub))
      colnames(df_test_data_sub) <- sub("'", "_", colnames(df_test_data_sub))
    } else {
      df_test_data_sub <- df_test_data_sub
    }
    # load selected model; depends on model_sel, price_sel and food_sel
    # if model_sel bsts, depends also on type_sel, size_sel and country_sel
    path_model <- paste0("models/", model_sel, "/final_models/", model_sel, "_", 
                         food_sel, "_", price_sel, ".rds")
    
    # only move on if file path exists
    if (file.exists(path_model)) {
      model_trained <- readRDS(path_model)
      
      # extract model predictors; keep only model predictors in test data
      ## this is done differently for tidymodels and train
      ## all models except SVM and NN are tidymodels objects
      if (model_sel %in% c("SVM", "NN")) {
        model_predictors_outcome <- model_trained$trainingData %>% colnames()
        model_predictors <- model_predictors_outcome[model_predictors_outcome != ".outcome"]
      } else {
        model_predictors <- model_trained$pre$mold$predictors %>% colnames()
      }
      
      df_test_data_sub <- df_test_data_sub %>%
        dplyr::select(all_of(model_predictors))
      
      # check if loaded model and test data contains correct predictors
      # model_trained$pre$mold$predictors %>% colnames() %>% length()
      # df_test_data_sub %>% colnames() %>% length()
      # length(intersect(model_trained$pre$mold$predictors %>% colnames(), 
      #                  df_test_data_sub %>% colnames()))
      
      # ensure that all predictors are numeric
      df_test_data_sub <- df_test_data_sub %>%
        mutate_if(is.character, as.numeric) %>%
        mutate_if(is.factor, as.numeric)
      
      ## Make Prediction for next week ##
      #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
      
      df_test_data_week1 <- df_test_data_sub %>% head(1)
      
      # standardize all predictors if we have LASSO, Ridge, Enet or SVM
      # min-max normalisation for Neural Network
      # For other models no feature scaling necessary
      if (model_sel %in% c("LASSO", "RIDGE", "ENET")) {
        scaler_path <- paste0("models/scaler/data_scaler_", food_sel, "_food.rds")
        scaler <- readRDS(scaler_path)
        ## drop all price variables
        scaler <- scaler[!scaler$variable %in% prices,]
        ## drop all lags not needed
        scaler <- scaler[!scaler$variable %in% prices_drop,]
        ## distances dropped
        scaler <- scaler[!scaler$variable %in% distances_drop,]
        ## do transformation
        for (i in colnames(df_test_data_week1)) {
          scaler_mean <- scaler[scaler$variable == i, "mean"]
          scaler_std <- scaler[scaler$variable == i, "sd"]
          df_test_data_week1[, i] <- (df_test_data_week1[, i] - scaler_mean)/scaler_std
        }
        # standardization also for SVM but different column names
      } else if (model_sel %in% c("SVM")) {
        scaler_path <- paste0("models/scaler/data_scaler_", food_sel, "_food.rds")
        scaler <- readRDS(scaler_path)
        ## drop all price variables
        scaler <- scaler[!scaler$variable %in% prices,]
        ## drop all lags not needed
        scaler <- scaler[!scaler$variable %in% prices_drop,]
        ## distances dropped
        scaler <- scaler[!scaler$variable %in% distances_drop,]
        ## replace variables name by underscore
        scaler$variable <- gsub(" ", "_", scaler$variable )
        scaler$variable <- gsub("'", "_", scaler$variable)
        ## do transformation
        for (i in colnames(df_test_data_week1)) {
          scaler_mean <- scaler[scaler$variable == i, "mean"]
          scaler_std <- scaler[scaler$variable == i, "sd"]
          df_test_data_week1[, i] <- (df_test_data_week1[, i] - scaler_mean)/scaler_std
        }
        # min-max-normalization for NN
      } else if (model_sel %in% c("NN")) {
        min_max_path <- paste0("models/scaler/data_scaler_min_max_", food_sel, "_food.rds")
        scaler <- readRDS(min_max_path)
        ## drop all price variables
        scaler <- scaler[!scaler$variable %in% prices,]
        ## drop all lags not needed
        scaler <- scaler[!scaler$variable %in% prices_drop,]
        ## distances dropped
        scaler <- scaler[!scaler$variable %in% distances_drop,]
        ## replace variables name by underscore
        scaler$variable <- gsub(" ", "_", scaler$variable)
        scaler$variable <- gsub("'", "_", scaler$variable)
        ## do transformation
        for (i in colnames(df_test_data_week1)) {
          scaler_max <- scaler[scaler$variable == i, "max"]
          scaler_min <- scaler[scaler$variable == i, "min"]
          df_test_data_week1[, i] <- 
            (df_test_data_week1[, i] - scaler_min)/(scaler_max - scaler_min)
        }
      }
      
      # make predictions
      price_pred <- predict(model_trained, df_test_data_week1)
      
      # data frame with price predictions
      ## return from predict() function differs across tidymodels and caret
      if (model_sel %in% c("SVM", "NN")) {
        df_pred <- data.frame(week = 1, price_pred = price_pred)
      } else {
        df_pred <- data.frame(week = 1, price_pred = price_pred %>% pull())
      }
      
      # if pred_horizon is bigger than 1, lagged prices needs to be adjusted by
      # predicted prices
      week_sel <- 1 # counter for week
      if (pred_horizon > 1) {
        # make predictions until selected pred_horizon is met
        while (week_sel < pred_horizon) {
          week_sel <- week_sel + 1
          df_test_data_week_next <- df_test_data_sub %>% dplyr::slice(week_sel)
          missing_cols <- 
            colnames(df_test_data_week_next)[colSums(is.na(df_test_data_week_next)) > 0]
          # lag from last week is replaced by prediction from last week
          df_test_data_week_next[, missing_cols[str_detect(missing_cols, "lag_one_week")]] <- 
            df_pred %>% filter(week == (week_sel - 1)) %>% 
            dplyr::select(price_pred) %>% pull()
          # standardize
          if (model_sel %in% c("LASSO", "RIDGE", "ENET")) {
            scaler_path <- 
              paste0("models/scaler/data_scaler_", food_sel, "_food.rds")
            scaler <- readRDS(scaler_path)
            ## drop all price variables
            scaler <- scaler[!scaler$variable %in% prices,]
            ## drop all lags not needed
            scaler <- scaler[!scaler$variable %in% prices_drop,]
            ## distances dropped
            scaler <- scaler[!scaler$variable %in% distances_drop,]
            ## do transformation
            for (i in colnames(df_test_data_week_next)) {
              scaler_mean <- scaler[scaler$variable == i, "mean"]
              scaler_std <- scaler[scaler$variable == i, "sd"]
              df_test_data_week_next[, i] <- 
                (df_test_data_week_next[, i] - scaler_mean)/scaler_std
            }
            # standardization also for SVM but different column names
          } else if (model_sel %in% c("SVM")) {
            scaler_path <- paste0("models/scaler/data_scaler_", food_sel, "_food.rds")
            scaler <- readRDS(scaler_path)
            ## drop all price variables
            scaler <- scaler[!scaler$variable %in% prices,]
            ## drop all lags not needed
            scaler <- scaler[!scaler$variable %in% prices_drop,]
            ## distances dropped
            scaler <- scaler[!scaler$variable %in% distances_drop,]
            ## replace variables name by underscore
            scaler$variable <- gsub(" ", "_", scaler$variable)
            scaler$variable <- gsub("'", "_", scaler$variable)
            ## do transformation
            for (i in colnames(df_test_data_week1)) {
              scaler_mean <- scaler[scaler$variable == i, "mean"]
              scaler_std <- scaler[scaler$variable == i, "sd"]
              df_test_data_week1[, i] <- (df_test_data_week1[, i] - scaler_mean)/scaler_std
            }
            # min-max-normalization for NN
          } else if (model_sel %in% c("NN")) {
            min_max_path <- paste0("models/scaler/data_scaler_min_max_", food_sel, "_food.rds")
            scaler <- readRDS(min_max_path)
            ## drop all price variables
            scaler <- scaler[!scaler$variable %in% prices,]
            ## drop all lags not needed
            scaler <- scaler[!scaler$variable %in% prices_drop,]
            ## distances dropped
            scaler <- scaler[!scaler$variable %in% distances_drop,]
            ## replace variables name by underscore
            scaler$variable <- gsub(" ", "_", scaler$variable)
            scaler$variable <- gsub("'", "_", scaler$variable)
            ## do transformation
            for (i in colnames(df_test_data_week1)) {
              scaler_max <- scaler[scaler$variable == i, "max"]
              scaler_min <- scaler[scaler$variable == i, "min"]
              df_test_data_week1[, i] <- 
                (df_test_data_week1[, i] - scaler_min)/(scaler_max - scaler_min)
            }
          }
          
          # make prediction
          price_pred <- predict(model_trained, df_test_data_week_next)
          
          # append to data frame
          ## return from predict() function differs across tidymodels and caret
          if (model_sel %in% c("SVM", "NN")) {
            df_pred_next <- data.frame(week = week_sel, price_pred = price_pred)
          } else {
            df_pred_next <- data.frame(week = week_sel, 
                                       price_pred = price_pred %>% pull())
          }
          
          df_pred <- rbind(df_pred, df_pred_next)
        }
      }
      
      
      # load previous predictions
      path_old_pred <- 
        paste0("models/", model_sel, "/pred_results/", model_sel, "_pred_food_model.rds")
      df_pred_old <- readRDS(path_old_pred)
      ## select
      df_pred_old <- df_pred_old %>%
        filter(food == food_sel, type == type_sel, size == size_sel,
               country == country_sel, price == price_sel) %>%
        arrange(date)
      
      # replace week variable by date
      df_pred$date <- date_pred
      #df_pred <- df_pred %>% dplyr::select(-week)
      
      # if we have NN, we need to revert min-max-normalization
      if (model_sel %in% "NN") {
        # load scaler again as we need price
        min_max_path <- paste0("models/scaler/data_scaler_min_max_", food_sel, "_food.rds")
        scaler <- readRDS(min_max_path)
        # extract correct values
        scaler_price <- scaler[scaler$variable == price_sel, ]
        
        # rescale predictions
        df_pred <- df_pred %>%
          mutate(
            price_pred = price_pred * (scaler_price$max - scaler_price$min) + scaler_price$min
          )
      } else {
        df_pred <- df_pred
      }
      
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
      
    } else {
      #opt <- options(show.error.messages = FALSE)
      #on.exit(options(opt))
      #stop("Test")
      df_pred_final <- data.frame(error = "Model not available")
      return(df_pred_final)
    }
  }
