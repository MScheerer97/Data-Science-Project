##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


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
    # prevent error resulting from non-existing files (not published every week)
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

#### From here our own code is applied to 1. obtain the data and 2. to 
#### transform the data and append it to the data base

## Now source API key
source("API_Key_Food_Price_Update.R")

credits <- check_credits(api_key = .price_key)$usage
input_location <- paste0(getwd(), "/Data/prices/", new_files)

# new data is only read if there exists new data! 

if(!rlang::is_empty(new_files)){
  
  #### Read new data and append
  n <- length(new_files)
  
  fruits_veggies_new <- readRDS("output/fruits_veggies_new.rds")
  potatoes_new <- readRDS("output/potatoes_new.rds")
  
  new_potatoes_pred <- data.frame()
  new_fruits_pred <- data.frame()
  
  for (i in 1:n) { 
    
    results <- ExtractTable(api_key = .price_key, filepath = input_location[i])
    
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
  
  saveRDS(fruits_veggies_new, "output/fruits_veggies_new.rds")
  write.csv(fruits_veggies_new, "output/fruits_veggies_new.csv")
  
  saveRDS(potatoes_new, "output/potatoes_new.rds")
  write.csv(potatoes_new, "output/potatoes_new.csv")
  
  
  
  #### Data Preparation of Updated Fruit/Veggi Prices ####
  
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
  
  
  #### Data Preparation of Updated Potatoe Prices ####
  
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
  
  #### Save new data frame for predictions of next week
  ## create data frame only if it does not exist already
  
  if (!file.exists("output/new_prices_prediction.rds")){
    
    data <- data.frame()
    saveRDS(data, "output/new_prices_prediction.rds")
    
  }
  
  # to use new prices to compare to our prediction, only the new prices 
  # are saved in the following data frame
  
  pred_data <- readRDS("output/new_prices_prediction.rds")
  pred_data_new <- rbind(pred_data, potatoes_final, fruits_veggies_final) %>%
    filter((year %in% new_fruits_pred$year) & (week %in% new_fruits_pred$week))
  
  #### English Translation ####
  
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
  
  
  #### Indicator for fruits and vegetables ####
  
  fruits <- c("Apple", "Peach", "Pear", "Grapes", "Strawberry", "Kiwi", "Orange", "Clementinen",
              "Mandarin", "Lemon", "Banana", "Satsumas", "Plum", "Nectarine", "Apricot", "Cherry")
  veggies <- c("Artichoke", "Eggplant", "Cauliflower", "Carrot", "Beans", "Iceberg Lettuce", 
               "Endive", "Cucumber", "Tomato", "Bell Pepper", "Leek", "Onion", "Zucchini",
               "Potatoes", "Lettuce", "Asparagus", "Brussel Sprouts")
  pred_data_new$fruits <- ifelse(pred_data_new$food %in% fruits, 1, 0)
  pred_data_new$veggies <- ifelse(pred_data_new$food %in% veggies, 1, 0)
  
  saveRDS(pred_data_new, "output/new_prices_prediction.rds")
  
  
  #### Append to Final Data Frame ####
  
  # load base food data frame
  df_food_all <- readRDS("output/price_data_all.rds")
  
  # append
  df_food_updated <- rbind(
    df_food_all, potatoes_final, fruits_veggies_final
  )
  
  #### English Translation ####
  
  # fruits
  df_food_updated$food <- mapvalues(df_food_updated$food,
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
  df_food_updated$country <- mapvalues(df_food_updated$country,
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
  df_food_updated$type <- mapvalues(df_food_updated$type,
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
                                           "normal", "premium"))
  
  
  #### Indicator for fruits and vegetables ####
  
  fruits <- c("Apple", "Peach", "Pear", "Grapes", "Strawberry", "Kiwi", "Orange", "Clementinen",
              "Mandarin", "Lemon", "Banana", "Satsumas", "Plum", "Nectarine", "Apricot", "Cherry")
  veggies <- c("Artichoke", "Eggplant", "Cauliflower", "Carrot", "Beans", "Iceberg Lettuce", 
               "Endive", "Cucumber", "Tomato", "Bell Pepper", "Leek", "Onion", "Zucchini",
               "Potatoes", "Lettuce", "Asparagus", "Brussel Sprouts")
  df_food_updated$fruits <- ifelse(df_food_updated$food %in% fruits, 1, 0)
  df_food_updated$veggies <- ifelse(df_food_updated$food %in% veggies, 1, 0)
  
  
  #### Save Data Frame ####
  
  # save new data frame
  saveRDS(df_food_updated, "output/price_data_updated.rds")
  
  
}


