#%%%%%%%%%%%%%%%%%%%%%%#
## FEATURE IMPORTANCE ##
#%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this file, a function is created which produces feature importance plots
# using the XGBoost model.
# The feature importance plot can be produced for each food model separately.
# Models for which features important plots can be created are:

# The function akes four two arguments:
  ## "food" indicates the food item, e.g. "Apple" or "Strawberry".
  ## "market" indicates the market on which the food item is sold, e.g. "Berlin".
  ## "n_features" indicates the number of features displayed in the feature
  ## importance plots.
  ## "type" indicates the importance method. Choices are "Gain" and "Permutation"


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# load packages (comment out as this is done in the load_file.R)
# if (!require("tidymodels")) install.packages("tidymodels")
# library(tidymodels)
# 
# if (!require("dplyr")) install.packages("dplyr")
# library(dplyr)
# 
# if (!require("vip")) install.packages("vip")
# library(vip)



#### Feature Importance XGBoost ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


func_xgb_importance <- function(food, market, n_features, type) {
  
  # for all types #
  #%%%%%%%%%%%%%%%#
  
  # load trained model
  load_path <- paste0("models/XGBoost/final_models/XGBoost_", food, "_",
                      market, ".rds")
  xgb_model <- readRDS(load_path)
  
  # map market
  market_sel <- ifelse(
    market == "price_F", "Frankfurt",
    ifelse(market == "price_H", "Hamburg",
           ifelse(market == "price_K", "Cologne",
                  ifelse(market == "price_B", "Berlin",
                         ifelse(market == "price_M", "Munich", market
                         )))))
  
  
  # feature importance with type "gain"
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  if (type == "Gain") {
    
    # create feature importance plot using the vip() function and the selected
    # number of features
    # xgb_model %>%
    # extract_fit_parsnip() %>%
    # vip(num_features = n_features, geom = "point", horizontal = TRUE,
    #     aesthetics = list(color = "#00FFFF", size = 4),
    #     method = "model") +
    
    # calculate importance scores
    df_imp_xgboost <- 
      vi_model(xgb_model %>% extract_fit_parsnip(), type = "gain") %>%
      arrange(-Importance) %>%
      head(n_features) 
    
    # create plot
    xgb_feature_importance <-
      df_imp_xgboost %>% 
      ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
      geom_point(color = "#00FFFF", size = 4) +
      xlab("Importance Score") +
      scale_x_continuous(limits = c(0, max(df_imp_xgboost$Importance) + 0.1)) +
      labs(title = paste0("XGBoost feature importance plot for ", food,
                          " from wholesale market ", market_sel),
           subtitle = "using 'gain' to calculate the importance scores \n",
           y = " ",
           x = "\n Importance Score"
      ) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(size = 20, colour = "white", hjust = 0.5),
        plot.subtitle = element_text(size = 16, colour = "white", hjust = 0.5),
        # axis labels
        axis.text = element_text(size = 14, colour = "white"),
        axis.title.x = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white", size = 14)
      )
    
    return(xgb_feature_importance)
    
    
    # feature importance with type "permutation"
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
    
  } else if (type == "Permutation") {
    # load machine learning data set
    df_ML_raw <- readRDS("output/final_ML.rds")
    ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | 
                                       sapply(df_ML_raw, is.integer)])
    df_ML_raw <- df_ML_raw %>% mutate_if(is.integer,as.numeric)
    df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)
    df_ML_raw <- df_ML_raw %>% dplyr::select(-date)
    
    # subset on selected food
    ## extract vector (otherwise not working in filter with same name)
    food_sel <- food 
    ## filter on selected food
    df_ML <- df_ML_raw %>%
      filter(food %in% food_sel)
    
    # keep only column names used in model
    colnames_predictors <- xgb_model$pre$mold$predictors %>% colnames()
    df_ML <- df_ML %>%
      dplyr::select(all_of(market), all_of(colnames_predictors))
    
    # calculate permutation importance
    ## to save computation time only two simulations are used
    vip(xgb_model %>% extract_fit_parsnip(), train = as.matrix(df_ML), 
        method = "permute", target = market, nsim = 2,
        metric = "rmse", pred_wrapper = predict, 
        num_features = n_features, 
        geom = "point", aesthetics = list(color = "#00FFFF", size = 4)) +
      labs(title = paste0("XGBoost Feature Importance plot for ", food_sel,
                          " from wholesale market ", market_sel),
           subtitle = "using 'permutation' to calculate the importance scores \n",
           x = " ",
           y = "\n Importance Score"
      ) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(size = 20, colour = "white", hjust = 0.5),
        plot.subtitle = element_text(size = 16, colour = "white", hjust = 0.5),
        # axis labels
        axis.text = element_text(size = 14, colour = "white"),
        axis.title.x = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white", size = 14)
      )
    
  }
  
}

