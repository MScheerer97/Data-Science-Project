# General Setup #
#%%%%%%%%%%%%%%%#

# The global.R file runs before ui.R and server.R. Any variables, data etc.
# included in global.R is accessible from either file.

# packages
packages <- c("shiny", "shinydashboard", "shinycssloaders", "shinyjs", "shinyWidgets",
              "graphics", "corrplot", "plotly", "ggplot2", "scales", "dplyr", "DT",
              "stringi", "stringr", "tidyverse", "knitr", "stargazer", "RColorBrewer", 
              "lubridate", "rmarkdown", "fastDummies", "tidymodels", "glmnet",
              "randomForest", "xgboost", "reshape2", "rlang", "data.table",
              "factoextra", "cluster", "vip", "ggpubr", "fastDummies", 
              "RSNNS", "bsts", "tsbox", "corrplot", "knitr", "tableHTML", 
              "kableExtra", "viridis", "tidyr", "gtools", "forcats",
              "MLmetrics", "kernlab")

# load packages
invisible(lapply(packages, library, character.only = TRUE))



# Load Data #
#%%%%%%%%%%%#

# data for descriptive statistics
df_descr <- readRDS("output/df_descriptives.rds") %>% ungroup()

# data for food basket
df_basket <- readRDS("output/final_basket.rds") %>% ungroup()

# machine learning data set
df_pred <- readRDS("output/final_ML.rds") %>% ungroup()
df_pred$month <- month(df_pred$date)
df_pred$year <- year(df_pred$date)

# data for making predictions
df_test_data <- readRDS("output/final_test_data.rds")
df_new_prices_final <- readRDS("output/price_data_updated_app.rds")

# data for cluster
df_cluster <- readRDS("output/final_cluster.rds") %>% ungroup()

# data for performance measures
df_perf <- readRDS("output/perf_metric_final.rds")
df_perf_overall <- readRDS("output/perf_metric_overall.rds")
df_perf_overall_comb <- readRDS("output/perf_metric_overall_full.rds")


# Define vectors #
#%%%%%%%%%%%%%%%%#

# define fruits included in the app
fruits <- c("Apple", "Pear", "Grapes", "Strawberry", "Peach", "Nectarine",
            "Kiwi", "Orange", "Lemon","Banana")


# define replacement list for all markets
market_replacement <- list(price_F = 'Frankfurt', price_H = 'Hamburg',
                           price_K = 'Cologne', price_M = 'Munich',
                           price_B = 'Berlin')


# define replacement list for all models
## this is necessary even if name is not changed
model_replacement <- list(Random_Forest = 'Random Forests', 
                          NN = 'Neural Network',
                          SVM = 'Support Vector Machines',
                          ENET = 'Elastic Net',
                          LASSO = 'LASSO', 
                          RIDGE = 'Ridge',
                          XGBoost = 'XGBoost',
                          BSTS = 'BSTS', 
                          `BSTS Baseline` = 'BSTS Baseline')



# Load Files #
#%%%%%%%%%%%%#

# source data preparation file
source("04_Data_Preparation_Shiny.R")


# Load Functions #
#%%%%%%%%%%%%%%%%#

# load function to perform clustering
source("func_kmeans.R") 

# load function to perform predictions
source("func_price_predictions_ml.R")
source("func_price_predictions_bsts.R")

# load functions for feature importance
source("func_xgb_importance.R")
source("func_lasso_importance.R")

# load function to generate plot for single comparison
source("func_plotly_single_comparison.R")

# generate function for ordering with mixed types
## this functions is used to order our size column which includes number and 
## characters, e.g. 270g, 400g, etc. 
func_order_mixed_types <- function(x) {
  order(gtools::mixedorder(x))
}
