#%%%%%%%%%%%%%%%%%%%%%%#
## FEATURE IMPORTANCE ##
#%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this file, a function is created which produces feature importance plots
# using the LASSO Regression as well as a table including the coefficients
# set to zero.
# The function takes four two arguments:
  ## "food" indicates the food item, e.g. "Apple" or "Strawberry".
  ## "market" indicates the market on which the food item is sold, e.g. "Berlin".
  ## "n_features" indicates the number of features displayed in the feature
  ## importance plots.
  ## "type" indicates the importance method. Choices are "zero coefficients" and 
  ## "non-zero coefficients"

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


#### Feature Importance LASSO ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



func_lasso_importance <- function(food, market, n_features, type) {
  # load trained model
  load_path <- paste0("models/LASSO/coef_results/LASSO_coef_", food, "_",
                      market, ".rds")
  lasso_model <- readRDS(load_path)
  
  # map market
  market_sel <- ifelse(
    market == "price_F", "Frankfurt",
    ifelse(market == "price_H", "Hamburg",
           ifelse(market == "price_K", "Cologne",
                  ifelse(market == "price_B", "Berlin",
                         ifelse(market == "price_M", "Munich", market
                         )))))
  
  # create table with zero coefficients
  if (type == "Zero Coefficients") {
    coef_0 <-
      lasso_model %>%
      filter(estimate == 0) %>%
      dplyr::select(term) %>%
      pull()
    
    weather_vars <- c("temp", "humidity", "cloudcover", "precip", "windspeed")
    weather_vars <- sort(coef_0[str_detect(coef_0, 
                                           paste(weather_vars, collapse = "|"))])
    
    transport_vars <- c("Kerosene", "fuel", "oil", "wci")
    transport_vars <- sort(coef_0[str_detect(coef_0, 
                                             paste(transport_vars, collapse = "|"))])
    
    fertilizer_vars <- c("DAP", "TSP", "Urea", "Phosphate")
    fertilizer_vars <- sort(coef_0[str_detect(coef_0, 
                                              paste(fertilizer_vars, collapse = "|"))])
    
    time_vars <- c("^year_", "^month_", "^week_")
    time_vars <- sort(coef_0[str_detect(coef_0, 
                                        paste(time_vars, collapse = "|"))])
    
    economic_vars <- c("exchange", "inflation")
    economic_vars <- sort(coef_0[str_detect(coef_0, 
                                            paste(economic_vars, collapse = "|"))])
    
    other_vars <- coef_0[!coef_0 %in% c(economic_vars, time_vars, fertilizer_vars, 
                                        transport_vars, weather_vars)]
    
    
    df_zero_coef <- 
      data.frame(
        Economic = c(sort(economic_vars), rep(NA, 100 - length(economic_vars))),
        Fertilizer = c(sort(fertilizer_vars), rep(NA, 100 - length(fertilizer_vars))),
        Time = c(sort(time_vars), rep(NA, 100 - length(time_vars))),
        Transport = c(sort(transport_vars), rep(NA, 100 - length(transport_vars))),
        Weather = c(sort(weather_vars), rep(NA, 100 - length(weather_vars))), 
        Other = c(sort(other_vars), rep(NA, 100 - length(other_vars)))
      ) %>%
      filter(., rowSums(is.na(.)) != ncol(.))
    
    return(df_zero_coef)
    
  } else {
    lasso_df <- lasso_model %>%
      filter(estimate > 0, term != ("(Intercept)")) %>%
      dplyr::arrange(desc(abs(estimate))) %>%
      dplyr::select(term, estimate) %>%
      head(n_features) %>%
      dplyr::rename(
        Variable = term,
        Importance = estimate
      ) 
    
    lasso_plot <- lasso_df %>%
      ggplot(aes(y = reorder(Variable, Importance), x = Importance)) +
      geom_point(color = "#00FFFF", size = 4) +
      xlab("Importance Score") +
      scale_x_continuous(limits = c(0, max(lasso_df$Importance) + 2)) +
      ylab("") +
      labs(title = paste0("LASSO Feature importance plot for \n", food,
                          " from wholesale market ", market_sel)) +
      theme(
        # black background of panel and plot
        panel.background = element_rect(fill = "black", color = "black"),
        plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(size = 20, colour = "white", hjust = 0.5),
        # axis labels
        axis.text = element_text(size = 14, colour = "white"),
        axis.title.x = element_text(colour = "white", size = 14),
        axis.title.y = element_text(colour = "white")
      )
    
    
    return(lasso_plot)
    
  }
  
}



