##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany

## by Lana Kern and Martin Scheerer ##

#### Data Project - Food Nutrition Values Data Acquisition 
## FoodData Central API

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plyr")) install.packages("plyr") 

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyverse)
library(plyr)

# Data Acquisition --------------------------------------------------------

### Food Nutrition Data - Using the FoodData Central API Guide by the 
### U.S. Department of Agriculture


#### Load API Key ####
source("API_Key_Nutrition.R")


#### Define vectors ####

# A maximum of 200 results per page is possible
# We start with page one and successively append the following pages

page <- 1
food_url <- "https://api.nal.usda.gov/fdc/v1/foods/search?"

# define vector of fruits and vegetables
  ## Satsumas gibt es nicht
food_names <- c("Apple", "Pear", "Grapes", "Strawberries", "Kiwi fruit", "Orange", 
                 "Clementine", "Cherry", "Tangerines, (mandarin oranges)", 
                 "Tangerine", "Lemon", "Banana", "Nectarine", "Peach", "Plum",
                 "Cucumber", "Tomatoes", "Onions", "Eggplant", "Cauliflower", 
                 "Carrots", "Asparagus", 
                 "Green beans", "Endive", "Brussels sprouts", "Lettuce",
                 "Lettuce, iceberg", "Artichoke", "Zucchini", "Leek", "Potatoes") 


# define nutrition names, we want to keep
  ## Carbohydrate: Kohlenhydrate
  ## Fiber: Ballaststoffe
  ## Folic acid: Folsäure
  ## Potassium: Kalium
nutrition <- c("Protein", "Energy", "Water", "Sugars, total including NLEA",
               "Calcium, Ca", "Magnesium, Mg", "Phosphorus, P", 
               "Carbohydrate, by difference", "Sodium, Na", "Zinc, Zn", 
               "Fiber, total dietary", "Folic acid", "Iron, Fe",  
               "Total lipid (fat)", "Cholesterol", "Potassium, K", 
               "Vitamin A, RAE",  "Vitamin B-6", "Vitamin B-12", 
               "Vitamin C, total ascorbic acid", 
               "Vitamin D (D2 + D3)", "Vitamin D (D2 + D3), International Units", 
               "Vitamin E (alpha-tocopherol)", "Vitamin K (phylloquinone)")


#### Extract specified fruits and vegetables ####

df_food_final <- data.frame()

for (i in 1:length(food_names)) {
  # GET request
  food_data <- GET(url = food_url, query = list(api_Key = .key, 
                                                #pageNumber = page, 
                                                pageSize = 200, 
                                                query = food_names[i]))
  # extract content
  cont <- content(food_data, as = "text")
  food <- fromJSON(cont); length(food$foods)
  
  # keep only foods data frame and variables of interest
  df_food_sub <- food$foods[, c("description", "lowercaseDescription", "foodCategory", 
                          "foodNutrients")]
  
  # filter on food description 
  df_food_sub <- df_food_sub %>% 
    filter(description %in% paste0(food_names[i], ", raw"))
    ## this may not work in some special cases
  if (nrow(df_food_sub) == 0) {
    df_food_sub <- food$foods[, c("description", "lowercaseDescription", "foodCategory", 
                                  "foodNutrients")]
    df_food_sub <- df_food_sub[str_detect(df_food_sub$description, "raw") & 
                                 str_detect(df_food_sub$description, food_names[i]), ]
    ## if it still not works
    if (nrow(df_food_sub) == 0) {
    df_food_sub <- food$foods[, c("description", "lowercaseDescription", "foodCategory", 
                                  "foodNutrients")]
    df_food_sub <- df_food_sub[df_food_sub$lowercaseDescription == str_to_lower(food_names[i]), ]
    }
  }
  
  # in case of duplicate (e.g. for strawberries keep random entry)
  df_food_sub <- df_food_sub[1, ]
  
  # drop ", raw" from decsription if it is contained
  if (str_detect(df_food_sub$description, ", raw,")) {
    df_food_sub$description <-
      substring(df_food_sub$description, 1, nchar(df_food_sub$description) - 5)
  }
  
  if (str_detect(df_food_sub$description, "raw")) {
    df_food_sub$description <-
      substring(df_food_sub$description, 1, nchar(df_food_sub$description) - 5)
  }
  
  # extract info from nutrition table
  df_food_sub <-
    df_food_sub$foodNutrients[[1]][
      df_food_sub$foodNutrients[[1]]$nutrientName %in% nutrition, ] %>%
    dplyr::select(nutrientName, unitName, value) %>%
    dplyr::rename(
      nutrient = nutrientName, 
      nutrient_unit = unitName,
      nutrient_value = value
    ) %>%
    mutate(
      food_type = df_food_sub$description
    )
    
  # append to final fruits data frame
  df_food_final <- rbind(df_food_final, df_food_sub)
  
  # rate limit
  Sys.sleep(1)
  
}


#### Extract Pepper ####

# we extract pepper manually since, we want to distinguish between red, green and yellow
food_data <- GET(url = food_url, query = list(api_Key = .key, 
                                              pageNumber = 1, 
                                              pageSize = 200, 
                                              query = "Pepper"))
  ## extract content
cont <- content(food_data, as = "text")
food <- fromJSON(cont); length(food$foods)
  ## keep only foods data frame and variables of interest
df_food_sub <- food$foods[, c("description", "lowercaseDescription", "foodCategory", 
                              "foodNutrients")]
  ## keep only peppers of interest
df_food_sub <- df_food_sub %>%
  filter(description %in% c("Peppers, sweet, yellow, raw", "Peppers, sweet, red, raw",
                            "Peppers, sweet, green, raw"))

# for each row
for (i in 1:nrow(df_food_sub)) {
  
  # extract corresponding row
  df_food_sub_2 <- df_food_sub[i, ]
  
  # drop ", raw" from decsription if it is contained
  df_food_sub_2$description <-
    substring(df_food_sub_2$description, 1, nchar(df_food_sub_2$description) - 5)
  
  # extract info from nutrition table
  df_food_sub_2 <-
    df_food_sub_2$foodNutrients[[1]][
      df_food_sub_2$foodNutrients[[1]]$nutrientName %in% nutrition, ] %>%
    dplyr::select(nutrientName, unitName, value) %>%
    dplyr::rename(
      nutrient = nutrientName, 
      nutrient_unit = unitName,
      nutrient_value = value
    ) %>%
    mutate(
      food_type = df_food_sub_2$description
    )
  
  # append to final fruits data frame
  df_food_final <- rbind(df_food_final, df_food_sub_2)
}



#### Satsuma ####

# information on nutrition about satsuma is manually extracted from the
# following web pages:
# https://www.wikifit.de/kalorientabelle/obst/satsumas
# https://www.naehrwertrechner.de/naehrwerte/F608000/Satsuma

# Note: web scraping would have taken longer for only one fruit
df_satsuma <- df_food_final %>% 
  filter(food_type == "Apple") %>% 
  dplyr::select(nutrient, nutrient_unit) %>%
  mutate(
    nutrient_value = c(0.800, 0.000, 0.940, 46.000, 86.900, 0.940, 0.700, NA, 100,
                       6, NA, NA, NA, NA, NA, NA, NA, 69, NA, NA, NA, NA, NA),
    food_type = "Satsuma"
  ) %>%
  na.omit()

df_food_final <- rbind(df_food_final, df_satsuma)


#### Final data frame ####

# remove ","
df_food_final$food_type <- str_replace_all(df_food_final$food_type, ",$", "")

# adjust food names such as first letter is capitel and all other are lower case
df_food_final$food_type <- str_to_sentence(df_food_final$food_type)

# check food names
unique(df_food_final$food_type)

# drop Tangerine, since we have Tangerines, (mandarin oranges)
df_food_final <- df_food_final %>%
  filter(food_type != "Tangerine")

# adjust names according to names in price data
df_food_final$food_type <- str_to_title(df_food_final$food_type)

df_food_final$food_type <- mapvalues(
  df_food_final$food_type,
  from = c("Kiwi Fruit", "Satsuma", "Strawberries","Tomatoes", "Carrots", 
           "Onions","Lettuce, Iceberg (Includes Crisphead Types)", 
           "Green Beans", "Peppers, Sweet, Yellow", "Peppers, Sweet, Green",
           "Peppers, Sweet, Red", "Tangerines, (Mandarin Oranges)"),
  to = c("Kiwi", "Satsumas", "Strawberry", "Tomato", "Carrot", "Onion", 
         "Iceberg Lettuce", "Beans", "Yellow Bell Pepper", "Green Bell Pepper",
         "Red Bell Pepper", "Mandarin")
)


# save data frame
# saveRDS(df_food_final, "output/food_nutrition.Rds")
write.csv(df_food_final, "output/food_nutrition.csv", row.names = FALSE)



