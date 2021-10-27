##### Data Science Project - Price Prediction of Fruits and Vegtables in Germany



#### Data Project - Food Nutrition Values Data Acquisition 
## FoodData Central API

# Libraries  --------------------------------------------------------------
rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")


library(httr)
library(jsonlite)


# Data Acquisition --------------------------------------------------------

## Food Nutrition Data - Using the FoodData Central API Guide by the 
## U.S. Department of Agriculture

# 3600 requests per hour

source("API Key.R")

nutrition_url <- "https://api.nal.usda.gov/fdc/v1/foods/list?"
nutrition_data <- GET(url = nutrition_url, query = list(api_Key = .key, pageNumber = 2))

cont <- content(nutrition_data, as = "text")
new2 <- fromJSON(cont)




















