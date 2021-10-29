##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany



#### Data Project - Food Nutrition Values Data Acquisition 
## FoodData Central API

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")


library(httr)
library(jsonlite)
library(dplyr)

# Data Acquisition --------------------------------------------------------

### Food Nutrition Data - Using the FoodData Central API Guide by the 
### U.S. Department of Agriculture

source("API Key.R")

# A maximum of 200 results per page is possible
# We start with page one and successively append the following pages

page <- 1
food_url <- "https://api.nal.usda.gov/fdc/v1/foods/search?"

food_data <- GET(url = food_url, query = list(api_Key = .key, 
                                              pageNumber = page, 
                                              pageSize = 200, 
                                              dataType = "SR Legacy"
                                              ))
food_data$url
cont <- content(food_data, as = "text")
food <- fromJSON(cont); length(food$foods)
foods <- food$foods[, c("description", "lowercaseDescription", "foodCategory", 
                         "foodNutrients")]

repeat { 
  
  page <- page + 1
  food_data <- GET(url = food_url, query = list(api_Key = .key, 
                                                pageNumber = page, 
                                                pageSize = 200,
                                                dataType = "SR Legacy"
                                                ))  
  
  if(food_data$status_code != 200) {
    
    print(page); print(food_data$status_code)
    break
    
  }
  
  cont <- content(food_data, as = "text")
  food <- fromJSON(cont)
  
  if(length(food$foods) == 0){
    
    print(paste("Page number", page - 1, "is the last page."))
    break
    
  }
  
  else {
    
    data <- food$foods[, c("description", "lowercaseDescription", "foodCategory", 
                           "foodNutrients")]
    foods <- rbind(foods, data)
    
  }
  
  print(page)
  
  # 3600 requests per hour
  Sys.sleep(1)
}

# Filtering the data for Vegetables and Fruits

foods <- foods %>%
  filter(foodCategory %in% c("Vegetables and Vegetable Products", "Fruits and Fruit Juices", 
                             "Apples", "Other vegetables and combinations", 
                             "Other fruits and fruit salads", "Dried fruits"))



