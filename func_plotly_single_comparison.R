#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# PLOTLY PLOT: TRUE VS. PREDICTED VALUES #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## by Lana Kern ##


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# This file includes a function to generate a nice plotly graphic which
# illustrates the true vs. predicted values for a model, market, and
# food-type-size-country-combination of choice.
# This file requires the function; func_dummy_variables, func_price_predictions_ml
# and func_price_predictions_bsts

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


source("func_price_predictions_ml.R") # already includes source("func_dummy_variables.R")

source("func_price_predictions_bsts.R")




#### FUNCTION: GENERATE PLOT FOR TRUE VALUES VS. PREDICTIONS ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# to do so the prediction food model function (func_pred_new_data) needs to be
# executed.
# then a plot is created which illustrates for each selection the true and
# predicted values over time.
# Moreover error metrics are displayed.

# func_pred_true_plot
func_plotly_single_comparison <- 
  function(food_sel, type_sel, size_sel, country_sel,
           model_sel, price_sel, pred_horizon) {
    
    # generate title of plot
    ## depends on food item
    if (food_sel %in% c("Iceberg Lettuce", "Cauliflower", "Endive")) {
      plot_year_2016_title <- "price in  \u20ac for 1 piece"
    } else {
      plot_year_2016_title <- "price in  \u20ac for 1kg"
    }
    
    
    # replace model name
    model_sel_full <- ifelse(
      model_sel == "Random_Forest", "Random Forests",
      ifelse(model_sel == "SVM", "Support Vector Machines",
      ifelse(model_sel == "NN", "Neural Network", 
      ifelse(model_sel == "ENET", "Elastic Net", 
      ifelse(model_sel == "RIDGE", "Ridge", model_sel))))
    )
    
    # generate test data
    # depends on the model which function is used
    if (any(c("BSTS Baseline", "BSTS") %in% model_sel)) {
      test_data <-
        func_price_predictions_bsts(
          food_sel, type_sel, size_sel, country_sel,
          model_sel, price_sel, pred_horizon)
    } else {
      test_data <- 
        func_price_predictions_ml(
          food_sel, type_sel, size_sel, country_sel,
          model_sel, price_sel, pred_horizon)
    }
    
    # identify market
    market_full_name <- test_data %>%
      mutate(
        market = if_else(market == "price_F", "Frankfurt", market),
        market = if_else(market == "price_B", "Berlin", market), 
        market = if_else(market == "price_M", "Munich", market),
        market = if_else(market == "price_K", "Cologne", market),
        market = if_else(market == "price_H", "Hamburg", market)
      ) %>%
      dplyr::select(market) %>% pull() %>% unique()
    
    # subset test data on prediction horizon
    test_data_sub <- test_data %>% 
      arrange(date) %>% tail(pred_horizon) 
    
    # generate plot
    ## create update button which only shows current predictions
    updatebutton <- list(
      list(
        active = -1,
        showactive = TRUE, # clicked button is visible
        type = "buttons",
        xanchor = "center",
        yanchor = "below",
        direction = "right", # buttons side by side 
        pad = list('r' = 0, 't' = 10, 'b' = 10),
        bordercolor = "00FF00",
        backgroundcolor = "black",
        font = list(color = "#00FF00"),
        x = 0.5, # x-axis position
        y = -0.7, # y-axis position # -0.4
        buttons = list(
          list(
            label = "Zoom into predictions",
            method = "update",
            args = list(
              list(visible = c(F, F, T, T))
            )
          ),
          list(
            label = "Zoom out",
            method = "update",
            args = list(list(visible = c(T, T, F, F)))
          )
        )
      )
    )
    
    # define margin for distance between plot and title
    mrg <- list(l = 50, r = 50, b = 100, t = 100, pad = 4)
    
    # generate plot
    plot_ly() %>% #(height = 600, width = 1000) %>%
      # add lines for zoom out: only lines
      add_lines(data = test_data, x = ~ date, y = ~ `true price`, 
                name = "true price", 
                line = list(color = "#FFFF00"),
                visible = TRUE,
                hoverinfo = "text",
                hovertext = paste0(
                    "<b>Date</b>: ", test_data$date %>% format("%B %d, %Y"),  "<br>",
                    "<b>True Price</b>: ", sprintf(test_data$`true price`, fmt = '%#.2f'), 
                    " \u20ac"
                  )
                ) %>%
      add_lines(data = test_data, x = ~ date, y = ~ `predicted price`, 
                name = "predicted price", 
                line = list(color = "#00FFFF"),
                visible = TRUE,
                hoverinfo = "text",
                hovertext = paste0(
                  "<b>Predicted Price</b>: ", 
                  sprintf(test_data$`predicted price`, fmt = '%#.2f'), 
                  " \u20ac"
                )
                ) %>%
      ## add lines for zoom in: lines + markers
      add_trace(data = test_data_sub, x = ~ date, y = ~ `true price`, 
                name = "true price", type = "scatter", mode = "lines + markers",
                line = list(color = "#FFFF00"), marker = list(color = "#F2EA02"),
                hovertemplate = paste('%{y:.2f}\u20ac'),
                visible = FALSE)  %>%
      add_trace(data = test_data_sub, x = ~ date, y = ~ `predicted price`, 
                name = "predicted price", type = "scatter", mode = "lines + markers",
                line = list(color = "#00FFFF"), marker = list(color = "#099FFF"),
                hovertemplate = paste('%{y:.2f}\u20ac'),
                visible = FALSE) %>%
      ## style
      plotly::layout(
        title = paste0("True vs. predicted prices for ", food_sel, ", ", type_sel, 
                       ", ", size_sel, " from ", country_sel, "<br>",  "<br>", "<sup>",
                       "Predictions origin from ", model_sel_full, " algorithm for wholesale market ",
                       market_full_name, "<br>", "<br>", "<br>", "<br>"),
        font = list(color = "white"),
        xaxis = list(title = "year", rangeslider = list(), type = "date"),
        yaxis = list(title = plot_year_2016_title),
        plot_bgcolor = "black",
        paper_bgcolor = "black",
        updatemenus = updatebutton,
        hovermode = "x unified", 
        margin = mrg # for space between title and plot
      )
    
  }