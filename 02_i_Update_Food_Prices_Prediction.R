#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Data Preparation: New Food Prices for Prediction #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern (with price update code from Martin Scheerer)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#### Content of File ####

# In this file, the prices are updated for the future.
# This file is established to not interrupt the usual update.
# The prices updated here are shown in the app as comparison to the
# predicted prices. In reality they would be unknown

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#



## Please adjust "todays's date" here:
year_today <- "2022"
week_today <- "03"


#### Data Project - Table Extraction for Fruit and Vegetables Prices using 
#### the ExtractTable API

rm(list = ls())

#### Data Project - Food Prices

if (!require("magrittr")) install.packages("magrittr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("dplyr")) install.packages("dplyr")
if (!require("gtools")) install.packages("gtools")
if (!require("stringr")) install.packages("stringr")
if (!require("tibble")) install.packages("tibble")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("readr")) install.packages("readr")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plyr")) install.packages("plyr") 
if (!require("RCurl")) install.packages("RCurl") 
if (!require("ISOweek")) install.packages("ISOweek") 
if (!require("missRanger")) install.packages("missRanger") 
if (!require("readxl")) install.packages("readxl") 
if (!require("stringr")) install.packages("stringr") 
if (!require("stringi")) install.packages("stringi")
if (!require("zoo")) install.packages("zoo")

library(missRanger)
library(magrittr)
library(jsonlite)
library(httr)
library(dplyr)
library(gtools)
library(stringr)
library(tibble)
library(tidyverse)
library(readr)
library(lubridate)
library(plyr)
library(RCurl)
library(ISOweek)
library(readxl)
library(stringr)
library(stringi)
library(zoo)


#### Update Food Prices ####

# First get a vector of all files to easily loop through and create list to
# save data frames with corresponding name --> filter 

files <- list.files("Data/prices")
files <- mixedsort(files)

last_file <- files[length(files)]
last_year <- as.numeric(str_extract(last_file, "[:digit:]{4}"))
last_kw <- parse_number(str_extract(last_file, "[:digit:]{2}\\."))

curr_kw <- week(Sys.Date())
curr_year <- year(Sys.Date())

## First check if data from most recent year in the folder (= last year) 
## is missing and obtain missing files

# sometimes last calendar week differs:_ to be sure 54 weeks are tested even though 
# this is impossible
if (curr_year != last_year) {
  
  kws <- (last_kw + 1):54
  kws <- ifelse(kws < 10, paste0("0", kws), as.character(kws))
  
  new_urls <- sprintf("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/%s/Wochenbericht_%s_%s.pdf?__blob=publicationFile&v=2", 
                      last_year, last_year, kws)
  
  for (i in 1:length(new_urls)) {
    
    tryCatch({
      
      download.file(new_urls[i], paste0("Data/prices/Wochenbericht_", last_year, "_", kws[i], 
                                        ".pdf"), mode = 'wb')
    }, error = function(e){})
  }
  
  # Now obtain kw data for current year
  
  curr_kws <- 1:curr_kw
  curr_kws <- ifelse(curr_kws < 10, paste0("0", curr_kws), as.character(curr_kws))
  
  curr_urls <- sprintf("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/%s/Wochenbericht_%s_%s.pdf?__blob=publicationFile&v=2", 
                       curr_year, curr_year, curr_kws)
  
  for (i in 1:length(curr_urls)) {
    
    tryCatch({
      
      download.file(curr_urls[i], paste0("Data/prices/Wochenbericht_", curr_year, "_", curr_kws[i], 
                                         ".pdf"), mode = 'wb')
    }, error = function(e){})
  }
  
  
}

## Now for the case that the year is the same

if (curr_year == last_year) {
  
  kws <- (last_kw + 1):curr_kw
  kws <- ifelse(kws < 10, paste0("0", kws), as.character(kws))
  
  new_urls <- sprintf("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/%s/Wochenbericht_%s_%s.pdf?__blob=publicationFile&v=2", 
                      curr_year, curr_year, kws)
  
  for (i in 1:length(new_urls)) {
    
    tryCatch({
      
      suppressWarnings(download.file(new_urls[i], paste0("Data/prices/Wochenbericht_", curr_year, "_", kws[i], 
                                                         ".pdf"), mode = 'wb'))
    }, error = function(e){})
  }
  
}



## Obtain those files where new data is displayed: in essence, the files that 
## were not in the folder before the code ran

new_files <- list.files("Data/prices") %>%
  mixedsort() 
new_files <- new_files[!(new_files %in% files)]

# These are the functions the API offers to extract tables from a PDF/PNG; 
# Please note: we did not write these functions ourselves

check_credits <- function(api_key) {
  validate_endpoint = 'https://validator.extracttable.com'
  return(content(GET(url = validate_endpoint, add_headers(`x-api-key` = api_key)), 
                 as = 'parsed', type = 'application/json'))
}
retrieve_result <- function(api_key, job_id) {
  retrieve_endpoint = "https://getresult.extracttable.com"
  return(
    GET(
      url = paste0(retrieve_endpoint, "/?JobId=", job_id),
      add_headers(`x-api-key` = api_key)
    )
  )
}
parseResponse <- function(server_resp) {
  return(fromJSON(content(server_resp, "text", encoding = "UTF-8")))
}
proces_file <- function(api_key, filepath) {
  trigger_endpoint = "https://trigger.extracttable.com"
  return(
    POST(
      url = trigger_endpoint,
      add_headers(`Content-Type` = "multipart/form-data", `x-api-key` = api_key),
      body = list(input = upload_file(filepath))
    )
  )
}
ExtractTable <- function(filepath, api_key) {
  server_response <- proces_file(api_key, filepath)
  parsed_resp = parseResponse(server_response)
  
  
  # Wait for a maximum of 5 minutes to finish the trigger job
  # Retries every 20 seconds
  max_wait_time = 5*60
  retry_interval = 20
  while (parsed_resp$JobStatus == 'Processing' & max_wait_time >= 0) {
    max_wait_time = max_wait_time - retry_interval
    print(paste0("Job is still in progress. Let's wait for ", retry_interval, " seconds"))
    Sys.sleep(retry_interval)
    server_response <- retrieve_result(api_key, job_id = parsed_resp$JobId)
    parsed_resp = parseResponse(server_response)
  }
  
  ### Parse the response for tables
  et_tables <- content(server_response, as = 'parsed', type = 'application/json')
  
  all_tables <- list()
  
  if (tolower(parsed_resp$JobStatus) != "success") {
    print(paste0("The processing was NOT SUCCESSFUL Below is the complete response from the server"))
    print(parsed_resp)
    return(all_tables)
  }
  
  ### Convert the extracted tabular JSON data as a dataframe for future use
  ### Each data frame represents one table
  for (i in 1:length(et_tables$Table)) {
    all_tables[[i]] <- sapply(et_tables$Tables[[i]]$TableJson, unlist) %>% t() %>% as.data.frame()
  }
  
  return(all_tables)
} 

api_key <- "ZJogzW14VII6em2hN1xiavTRTSuQh9EewohUYuaa"
credits <- check_credits(api_key = api_key)$usage
input_location <- paste0(getwd(), "/Data/prices/", new_files)

# new data is only read if there exists new data! 

if (!rlang::is_empty(new_files)) {
  
  #### Read new data and append
  n <- length(new_files)
  
  fruits_veggies_new <- readRDS("output/fruits_veggies_new.rds")
  potatoes_new <- readRDS("output/potatoes_new.rds")
  
  new_potatoes_pred <- data.frame()
  new_fruits_pred <- data.frame()
  
  for (i in 1:n) { 
    
    results <- ExtractTable(api_key = api_key, filepath = input_location[i])
    
    year_file <- as.numeric(str_extract(new_files[i], "[:digit:]{4}"))
    kw_file <- parse_number(str_extract(new_files[i], "[:digit:]{2}\\."))
    
    k <- length(results)
    
    for (p in 1:k) { 
      
      data <- results[[p]] %>% 
        mutate(index = as.numeric(row.names(.))) %>% 
        arrange(index) %>% 
        dplyr::select(-c("index"))
      
      data <- data[, mixedsort(colnames(data))]
      n_cols <- ncol(data)
      
      # Now differentiate between potatoes and fruits
      
      if (n_cols == 12) {
        
        data$year <- str_sub(str_remove_all(new_files[i], "[[:alpha:]]"), start = 2, end = 5)
        data$week <- str_sub(str_remove_all(new_files[i], "[[:alpha:]]"), start = 7, end = 8)
        
        # for final data set
        fruits_veggies_new <- rbind(fruits_veggies_new, data)
        
        # for prediction data set
        new_fruits_pred <- rbind(new_fruits_pred, data)
        
      }
      
      else if (n_cols == 7 & sum(str_detect(data, "Alexander")) == 0) { 
        
        data$year <- str_sub(str_remove_all(new_files[i], "[[:alpha:]]"), start = 2, end = 5)
        data$week <- str_sub(str_remove_all(new_files[i], "[[:alpha:]]"), start = 7, end = 8)
        
        # for final data set
        potatoes_new <- rbind(potatoes_new, data)
        
        # for prediction data set
        new_potatoes_pred <- rbind(new_potatoes_pred, data)
        
      }
    }
  }
  

  ## Data Preparation of Updated Fruit/Veggi Prices ##
  
  fruits_veggies_final <- fruits_veggies_new
  
  # drop rows in which first column contains "KW" or "Erzeugnis" or "BLE"
  fruits_veggies_final <- fruits_veggies_final[!str_detect(fruits_veggies_final[, 1], 
                                                           "KW|Erzeugnis|BLE|vom"),]
  
  # replace all empty columns with NA
  fruits_veggies_final <- fruits_veggies_final %>% mutate_all(na_if,"")
  
  # identify fruits and vegetables
  fruits_veggies_final[rowSums(is.na(fruits_veggies_final[, 2:11])) == 10, "food"] <- 
    fruits_veggies_final[rowSums(is.na(fruits_veggies_final[, 2:11])) == 10, 1]
  ## fill missings below
  fruits_veggies_final <- fruits_veggies_final %>% fill(food)
  ## drop fruits and vegetables row
  drop_fruit_rows <- unique(which(rowSums(is.na(fruits_veggies_final[, 2:11])) == 10))
  fruits_veggies_final <- fruits_veggies_final[-drop_fruit_rows, ]
  
  # select necessary columns and rename
  fruits_veggies_final <- fruits_veggies_final %>% dplyr::select(-c(4:6))
  colnames(fruits_veggies_final) <- c("type", "country", "size", "price_total",
                                      "price_F", "price_H", "price_K", "price_M", 
                                      "price_B", "year", "week", "food")
  
  # drop rows with two entries
  fruits_veggies_final <- fruits_veggies_final[-which(lengths(
    str_split(fruits_veggies_final[, 2], " ")) == 2), ]
  
  # replace missing values in type as well as "/" with "unknown"
  fruits_veggies_final[fruits_veggies_final$type == "/" | 
                         is.na(fruits_veggies_final$type), "type"] <- "unknown"
  
  # replace missing values in size with "/
  fruits_veggies_final[is.na(fruits_veggies_final$size), "size"] <- "/"
  
  # correct order of columns
  fruits_veggies_final <- fruits_veggies_final %>%
    dplyr::select(food, type, country, size, price_total, price_F,
                  price_H, price_K, price_M, price_B, year, week)
  
  
  ## Data Preparation of Updated Potatoe Prices ##
  
  potatoes_final <- potatoes_new
  
  # drop rows in which first column contains "KW" or "Erzeugnis" or "BLE"
  potatoes_final <- potatoes_final[!str_detect(potatoes_final[, 1], 
                                               "KW|Erzeugnis|BLE|vom"),]
  
  # replace all empty columns with NA
  potatoes_final <- potatoes_final %>% mutate_all(na_if,"")
  
  # drop rows with many missing values
  potatoes_final <- potatoes_final[rowSums(is.na(potatoes_final[, 1:7])) < 5, ]
  
  # rename variables
  colnames(potatoes_final) <- c("country", "type", "price_B", "price_F", 
                                "price_H", "price_K", "price_M", 
                                "year", "week")
  
  # create food, total price and size variable
  potatoes_final$food <- "Potatoes"
  potatoes_final$price_total <- NA # no total prices for potatoes
  potatoes_final$size <- "/" # no size for potatoes
  
  # drop first row since it contains variable names
  potatoes_final <- potatoes_final[-1, ]
  
  # replace missing values in type as well as "/" with "unknown"
  potatoes_final[potatoes_final$type == "/" | 
                   is.na(potatoes_final$type), "type"] <- "unknown"
  
  # correct order of columns
  potatoes_final <- potatoes_final %>%
    dplyr::select(food, type, country, size, price_total, price_F,
                  price_H, price_K, price_M, price_B, year, week)
  
  ## Save new data frame for predictions of next week
  
  ## create data frame only if it does not exist already
  
  pred_data_new <- rbind(potatoes_final, fruits_veggies_final) %>%
    filter((year %in% new_fruits_pred$year) & (week %in% new_fruits_pred$week))
  
  ## English Translation ##
  
  # fruits
  pred_data_new$food <- mapvalues(pred_data_new$food,
                                  from = c("Äpfel", "Apfel", "Birnen", "Tafeltrauben", "Orangen",
                                           "Zitronen", "Bananen", "Auberginen", "Möhren",
                                           "Bohnen", "Endivien", "Gurken", "Tomaten",
                                           "Gemüsepaprika", "Lauch", "Speisezwiebeln", "Zwiebeln",
                                           "Pfirsiche", "Nektarinen", "Pflaumen", "Kiwis",
                                           "Artischocken", "Blumenkohl", "Eissalat",
                                           "Kopfsalat", "Zucchini", "Erdbeeren", "Süßkirschen",
                                           "Kirschen", "Kartoffeln", "Speiselagerkartoffeln", 
                                           "Speisefrühkartoffeln", "Aprikosen", "Spargel", "Rosenkohl",
                                           "Clementinen", "Mandarinen"),
                                  to = c("Apple", "Apple", "Pear", "Grapes", "Orange",
                                         "Lemon", "Banana", "Eggplant", "Carrot",
                                         "Beans", "Endive", "Cucumber", "Tomato",
                                         "Bell Pepper", "Leek", "Onion", "Onion",
                                         "Peach", "Nectarine", "Plum", "Kiwi",
                                         "Artichoke", "Cauliflower", "Iceberg Lettuce",
                                         "Lettuce", "Zucchini", "Strawberry", "Cherry", "Cherry",
                                         "Potatoes", "Potatoes", "Potatoes", "Apricot", "Asparagus",
                                         "Brussel Sprouts", "Clementine", "Mandarin"))
  
  # countries (only countries used in the analysis)
  pred_data_new$country <- mapvalues(pred_data_new$country,
                                     from = c("Deutschland", "Niederlande", "Italien",
                                              "Frankreich", "Südafrika", "Spanien",
                                              "Türkei", "Belgien", "Marokko", "Griechenland",
                                              "Ägypten", "Neuseeland", "Polen", "Österreich",
                                              "Argentinien"),
                                     to = c("Germany", "Netherlands", "Italy",
                                            "France", "South Africa", "Spain",
                                            "Turkey", "Belgium", "Morocco", "Greece",
                                            "Egypt", "New Zealand", "Poland", "Austria",
                                            "Argentina"))
  
  # types
  pred_data_new$type <- mapvalues(pred_data_new$type,
                                  from = c("Sonstige Sorten", "Sonstige Marken",
                                           "Buschbohnen", "Stangenbohnen",
                                           "Mini", "Fleisch", "Kirsch",
                                           "Rispen", "runde", "gelber", "grüner", "roter",
                                           "Haushaltsware", "weißfleischig", "kleinfruchtig",
                                           "kleinfrüchtig", "Sonstige Clubsorten",
                                           "Sonstige Blondorangen", "Sonstige Blutorangen",
                                           "weißer", "gelbfleischig", "großfruchtig", 
                                           "lose", "Kg-Schale", "Gemüsezwiebeln",
                                           "Schlangengurken", "Erstmarke"),
                                  to = c("other types", "other brands", "Bush bean",
                                         "Runner bean", "mini", "flesh", "cherry",
                                         "truss", "round", "yellow", "green", "red",
                                         "Housewares", "white-fleshed", "small-fruited",
                                         "small-fruited", "other club types",
                                         "other blond oranges", "other blood orange",
                                         "white", "yellow-fleshed", "big-fruited",
                                         "loose", "kg-bowl", "Spanish onion",
                                         "normal", "first brand"))
  
  
  ## Indicator for fruits and vegetables ##
  
  fruits <- c("Apple", "Peach", "Pear", "Grapes", "Strawberry", "Kiwi", "Orange", "Clementinen",
              "Mandarin", "Lemon", "Banana", "Satsumas", "Plum", "Nectarine", "Apricot", "Cherry")
  veggies <- c("Artichoke", "Eggplant", "Cauliflower", "Carrot", "Beans", "Iceberg Lettuce", 
               "Endive", "Cucumber", "Tomato", "Bell Pepper", "Leek", "Onion", "Zucchini",
               "Potatoes", "Lettuce", "Asparagus", "Brussel Sprouts")
  pred_data_new$fruits <- ifelse(pred_data_new$food %in% fruits, 1, 0)
  pred_data_new$veggies <- ifelse(pred_data_new$food %in% veggies, 1, 0)

  
  ## Append to Final Data Frame ##
  
  # load base food data frame
  #df_food_all <- readRDS("output/price_data_all.rds")
  df_food_all <- readRDS("output/full_price_data_updated_app.rds")
  
  # append
  df_food_updated <- rbind(
    df_food_all, pred_data_new
  )

  # save new data frame
  saveRDS(df_food_updated, "output/full_price_data_updated_app.rds")
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation ####
#%%%%%%%%%%%%%%%%%%%%%%%%#


df_food_price <- df_food_updated

#  ensure that all week numbers have two entries
df_food_price$week <- stri_pad_left(df_food_price$week, 2, 0)

# add month to calender week (needed to merge season)
df_food_price$year_week <- paste0(df_food_price$year, "-W", 
                                  df_food_price$week, "-1")

for (i in 1:nrow(df_food_price)) {
  # generate date 
  df_food_price[i, "date"] <- 
    ISOweek2date(df_food_price[i, "year_week"])
}

# df_food_price$month <- as.numeric(df_food_price$month)
## drop year_week variable (not needed anymore)
df_food_price <- df_food_price %>% dplyr::select(-"year_week")
## all price variables as numeric
df_food_price[, c("price_total", "price_F", "price_H", 
                  "price_K", "price_M", "price_B")] <- sapply(
                    df_food_price[, c("price_total", "price_F", "price_H", 
                                      "price_K", "price_M", "price_B")],
                    as.numeric
                  )

# drop all observations with any missings in food, type, country and size
# something definitely went wrong with the data input
df_food_price <- df_food_price[complete.cases(
  df_food_price[, c("food", "type", "country", "size")]
),]


# Keep only selection of food-type-country-size observations #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

df_keep <- read_csv("output/food_types_final_analysis.csv")

# filter final data frame
df_food_price <- inner_join(
  df_food_price, df_keep, by = c("food", "type", "country", "size")
)


# replace missing values of prices #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## to do so, we use chained random forests
## setting pmm.k to a positive number leads to combining the imputations with a 
## predictive mean matching step. This option avoids imputation with values not 
## present in the original data. Moreover, the resulting imputed values are more
## natural and the distributional properties are improved. 
## we do explicitly not use a linear regression based approach as this may
## result in negative values.

# set unrealistic prices to NA
## we determine as unrealistic prices, prices below 10Euro/100kg
df_food_price <- df_food_price %>%
  mutate_at(vars(starts_with("price_")), ~ replace(., .< 10, NA))

# extract all food-type-country-size combinations
combis_replace <- df_food_price %>%
  dplyr::select(food, type) %>%
  distinct() 

# iterate over all combinations in order to replace the missing values
df_food_price_replace_final <- data.frame()

for (i in 1:nrow(combis_replace)) {
  
  # extract selected combination in iteration i
  combis_replace_sub <- combis_replace[i, ]
  
  # create data set for selected combination
  df_food_price_replace <- df_food_price %>%
    filter(food == combis_replace_sub$food, type == combis_replace_sub$type#,
           #country == combis_replace_sub$country, size == combis_replace_sub$size
    )
  
  # make replacement
  ## check if enough observations are available to replace using predictive mean matching
  ## if less than 5 for any price, no predictive mean matching is used
  ## if between 5 and 19 price observations, predictive mean matching using 5 values is supplied
  ## if 20 or more, predictive mean matching using 20 values is supplied. 
  ## number of trees is set to a small number to decrease the computation time
  ## the candidate of non-missing values to sample from in the predictive mean
  ## matching step is specified by pmm.k
  ## a seed is set to ensure reproducibility 
  if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, formula = price_total + price_F + price_H + price_K + price_M + price_B ~
                   price_total + price_F + price_H + price_K + price_M + country + size,
                 num.trees = 100, pmm.k = 0, returnOOB = TRUE, seed = 42)
  } else if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 20) &
             any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) >= 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, formula = price_total + price_F + price_H + price_K + price_M + price_B ~
                   price_total + price_F + price_H + price_K + price_M + country + size,
                 num.trees = 100, pmm.k = 5, returnOOB = TRUE, seed = 42)
  } else {
    df_food_price_replace <-
      missRanger(df_food_price_replace, formula = price_total + price_F + price_H + price_K + price_M + price_B ~
                   price_total + price_F + price_H + price_K + price_M + country + size,
                 num.trees = 100, pmm.k = 20, returnOOB = TRUE, seed = 42)
  }
  
  df_food_price_replace_final <- rbind(df_food_price_replace_final, df_food_price_replace)
}

# round all prices to 0 decimal places (in original document they are rounded)
df_food_price_replace_final <- df_food_price_replace_final %>%
  mutate_at(vars(starts_with("price_")), ~round(.))

# check distribution of prices (are they realistic?)
## yes: relatively low prices are for onions and relatively high prices for strawberries
## this matches the prices from the PDF documents
summary(df_food_price_replace_final$price_total)
summary(df_food_price_replace_final$price_F)
summary(df_food_price_replace_final$price_H)
summary(df_food_price_replace_final$price_K)
summary(df_food_price_replace_final$price_M)
summary(df_food_price_replace_final$price_B)


# adjust df_food_price data frame with new results
df_food_price <- df_food_price_replace_final
## check if all missing values are replaced
sum(is.na(df_food_price))


# create data set without missing weeks per food-type-country-size combination #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

df_food_price_continuous <- df_food_price %>%
  # group by food-type-country-size information
  group_by(food, type, country, size) %>%
  # add missing dates
  tidyr::complete(date = seq(min(df_food_price$date), 
                             max(df_food_price$date), by = "week")) %>%
  # extract year and week from dates
  mutate(
    year = as.character(year(date)),
    week = str_sub(date2ISOweek(date), 7, 8) 
  ) %>%
  # fill missing values
  group_by(food, type, country, size) %>%
  fill(fruits, veggies, food, type, country, size) %>%
  # ungroup data frame
  ungroup() 

# add indicator to identify later if observation is real or inserted
df_food_price_check <- df_food_price %>%
  dplyr::select("year", "week", "date", "country", "food", "type", "size") %>%
  mutate(flag = 1) %>%
  mutate_at(vars(year), as.character)
df_food_price_continuous <- left_join(
  df_food_price_continuous, df_food_price_check,
  by = c("year", "week", "date", "country", "food", "type", "size")
)
df_food_price_continuous <- df_food_price_continuous %>%
  mutate_at(vars(flag), ~replace(., is.na(.), 0))

# in case we generated duplicates, we keep the observation with flag = 1, i.e.
# original information
df_food_price_continuous <- df_food_price_continuous %>%
  arrange(food, type, country, size, year, week, flag)
df_food_price_continuous <- df_food_price_continuous[
  !duplicated(df_food_price_continuous[, 
                                       c("year", "week", "country", "food", "type", "size")]), 
]

# there are still missing values in fruits and veggies 
## create indicator for fruit and vegetables again
fruits <- c("Apple", "Peach", "Pear", "Grapes", "Strawberry", "Kiwi", "Orange", "Clementinen",
            "Mandarin", "Lemon", "Banana", "Satsumas", "Plum", "Nectarine", "Apricot", "Cherry")
veggies <- c("Artichoke", "Eggplant", "Cauliflower", "Carrot", "Beans", "Iceberg Lettuce", 
             "Endive", "Cucumber", "Tomato", "Bell Pepper", "Leek", "Onion", "Zucchini",
             "Potatoes", "Lettuce", "Asparagus", "Brussel Sprouts")
df_food_price_continuous$fruits <- 
  ifelse(df_food_price_continuous$food %in% fruits, 1, 0)
df_food_price_continuous$veggies <- 
  ifelse(df_food_price_continuous$food %in% veggies, 1, 0)

# create month from date
df_food_price_continuous$month <- as.integer(month(df_food_price_continuous$date))

# replace missing values of prices again #
##%%%

# extract all food-type-country-size combinations
combis_replace <- df_food_price_continuous %>%
  dplyr::select(food, type) %>%
  distinct() 

# iterate over all combinations in order to replace the missing values
df_food_price_replace_final <- data.frame()

for (i in 1:nrow(combis_replace)) {
  
  # get column names with missings
  col_missings <-
    colnames(df_food_price_continuous)[sapply(df_food_price_continuous, 
                                              function(x)any(is.na(x)))]
  
  # create columns used for replacement
  formula_replace <- paste(paste(col_missings, collapse = " + "), "~", 
                           "price_total + price_F + price_H + price_K + price_M + country + size")
  
  # extract selected combination in iteration i
  combis_replace_sub <- combis_replace[i, ]
  
  # create data set for selected combination
  df_food_price_replace <- df_food_price_continuous %>%
    filter(food == combis_replace_sub$food, type == combis_replace_sub$type#,
           #country == combis_replace_sub$country, size == combis_replace_sub$size
    )
  
  # make replacement
  ## check if enough observations are available to replace using predictive mean matching
  ## if less than 5 for any price, no predictive mean matching is used
  ## if between 5 and 19 price observations, predictive mean matching using 5 values is supplied
  ## if 20 or more, predictive mean matching using 20 values is supplied. 
  ## number of trees is set to a small number to decrease the computation time
  ## the candidate of non-missing values to sample from in the predictive mean
  ## matching step is specified by pmm.k
  ## a seed is set to ensure reproducibility 
  if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, 
                 formula = formula(formula_replace),
                 num.trees = 100, pmm.k = 0, returnOOB = TRUE, seed = 42)
  } else if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 20) &
             any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) >= 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, 
                 formula = formula(formula_replace),
                 num.trees = 100, pmm.k = 5, returnOOB = TRUE, seed = 42)
  } else {
    df_food_price_replace <-
      missRanger(df_food_price_replace, 
                 formula = formula(formula_replace),
                 num.trees = 100, pmm.k = 20, returnOOB = TRUE, seed = 42)
  }
  
  df_food_price_replace_final <- rbind(df_food_price_replace_final, df_food_price_replace)
}
df_food_price_continuous <- df_food_price_replace_final


# if there are still missing values left, replace by mean price
if (sum(is.na(df_food_price_continuous[, 
                                       c("price_total", "price_F",
                                         "price_H", "price_K", "price_M", 
                                         "price_B")])) > 0) {
  # get column names with missings
  col_missings <-
    colnames(df_food_price_continuous)[sapply(df_food_price_continuous, 
                                              function(x)any(is.na(x)))]
  # iterate ober column
  for (i in col_missings) {
    df_food_price_continuous[is.na(df_food_price_continuous[,i]), i] <- 
      round(mean(df_food_price_continuous[,i] %>% pull(), na.rm = TRUE))
  }
  
}

# ensure that prices across combinations are not constant
## this is unrealistic and makes problems for bsts model as standard deviation 
## or rather variance is zero.
## if they are we drop this food-type-size-country combination, as this emerged
## due to very few observations for this combination
## to do so, we calculate the variance for each food-type-size-country combination
## combinations
df_food_combis_zero_sd <-
  df_food_price_continuous %>%
  # group by food-type-size-country combinations
  group_by(food, type, size, country) %>%
  # calculate variance
  dplyr::summarise(
    var_price_F = var(price_F, na.rm = TRUE),
    var_price_H = var(price_H, na.rm = TRUE),
    var_price_K = var(price_K, na.rm = TRUE),
    var_price_M = var(price_M, na.rm = TRUE),
    var_price_B = var(price_B, na.rm = TRUE)
  ) %>%
  # set variance with 0 NA
  mutate_all(na_if, 0)
# extract combinations where variance is zero
df_combis_zero_sd_drop <-
  df_food_combis_zero_sd[rowSums(is.na(df_food_combis_zero_sd)) > 0,
                         c("food", "type", "size", "country")] 
# drop those combinations
df_food_price_continuous <-
  # keep data which is in df_food_price_continuous, but not in df_combis_zero_sd_drop
  anti_join(
    df_food_price_continuous, 
    subset(df_food_price_continuous, 
           food %in% df_combis_zero_sd_drop$food &
             type %in% df_combis_zero_sd_drop$type &
             size %in% df_combis_zero_sd_drop$size &
             country %in% df_combis_zero_sd_drop$country  
    )
  )

sum(is.na(df_food_price_continuous[, c("price_total", "price_F",
                                       "price_H", "price_K", "price_M", "price_B")]))
sum(is.na(df_food_price_continuous))

# check if no duplicates are left
sum(duplicated(df_food_price_continuous[, c("year", "week", "country", "food", "type", "size")]))

# round all prices to 0 decimal places (in original document they are rounded)
df_food_price_replace_final <- df_food_price_replace_final %>%
  mutate_at(vars(starts_with("price_")), ~round(.))

## Adding lags of prices: one week, 
df_food_price_continuous <- df_food_price_continuous %>%
  dplyr::group_by(type, size, country) %>%
  arrange(date) %>% 
  
  # Now insert lags
  mutate(price_total_lag_one_week = dplyr::lag(price_total, n = 1), 
         price_total_lag_one_month = dplyr::lag(price_total, n = 4), 
         price_total_lag_one_year = dplyr::lag(price_total, n = 52), 
         
         price_F_lag_one_week = dplyr::lag(price_F, n = 1), 
         price_F_lag_one_month = dplyr::lag(price_F, n = 4), 
         price_F_lag_one_year = dplyr::lag(price_F, n = 52), 
         
         price_H_lag_one_week = dplyr::lag(price_H, n = 1), 
         price_H_lag_one_month = dplyr::lag(price_H, n = 4), 
         price_H_lag_one_year = dplyr::lag(price_H, n = 52),
         
         price_K_lag_one_week = dplyr::lag(price_K, n = 1), 
         price_K_lag_one_month = dplyr::lag(price_K, n = 4), 
         price_K_lag_one_year = dplyr::lag(price_K, n = 52),
         
         price_M_lag_one_week = dplyr::lag(price_M, n = 1), 
         price_M_lag_one_month = dplyr::lag(price_M, n = 4), 
         price_M_lag_one_year = lag(price_M, n = 52),
         
         price_B_lag_one_week = dplyr::lag(price_B, n = 1), 
         price_B_lag_one_month = dplyr::lag(price_B, n = 4), 
         price_B_lag_one_year = dplyr::lag(price_B, n = 52)) %>%
  
  mutate_at(vars(price_total_lag_one_week, price_total_lag_one_month, price_total_lag_one_year, 
                 price_F_lag_one_week, price_F_lag_one_month, price_F_lag_one_year, 
                 price_H_lag_one_week, price_H_lag_one_month, price_H_lag_one_year, 
                 price_K_lag_one_week, price_K_lag_one_month, price_K_lag_one_year, 
                 price_M_lag_one_week, price_M_lag_one_month, price_M_lag_one_year, 
                 price_B_lag_one_week, price_B_lag_one_month, price_B_lag_one_year
  ),
  funs(na.locf(., na.rm = FALSE, fromLast = TRUE))) %>%
  ungroup()


# check for missings
sum(is.na(df_food_price_continuous))

# drop duplicates
df_food_price_continuous <- df_food_price_continuous %>%
  distinct()
sum(duplicated(df_food_price_continuous))

# check distribution of prices (are they realistic?)
## yes: relatively low prices are for onions and relatively high prices for strawberries
## this matches the prices from the PDF documents
summary(df_food_price_continuous$price_total)
summary(df_food_price_continuous$price_F)
summary(df_food_price_continuous$price_H)
summary(df_food_price_continuous$price_K)
summary(df_food_price_continuous$price_M)
summary(df_food_price_continuous$price_B)

# keep only observations after this date
df_food_updated_final <- df_food_price_continuous
df_food_updated_final <- df_food_updated_final %>%
  filter(year == year_today, week > week_today)

# replace sizes
food_sizes <- read_excel("data/food_sizes_mapping.xlsx")
df_food_updated_final <- left_join(
  df_food_updated_final, food_sizes, by = c("food", "size")
) %>%
  dplyr::select(-c(size)) %>%
  dplyr::rename(size = size_descr)

# further adjustments
df_food_updated_final <- df_food_updated_final %>%
  dplyr::mutate(type = dplyr::recode(type, Cucumber = "normal", 
                                     Erstmarke = "First Brand"))

df_food_updated_final$type <- str_to_title(df_food_updated_final$type)
df_food_updated_final$country <- str_to_title(df_food_updated_final$country)

# keep only columns of interest
df_food_updated_final <- df_food_updated_final %>%
  dplyr::select(food, type, country, size, date, price_F, price_H, price_K, price_M, price_B)


# save data frame with predictions of next weeks
saveRDS(df_food_updated_final, "output/price_data_updated_app.rds")
