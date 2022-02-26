##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - Inflation and Exchange Rates Data Acquisition 
## Nasdaq API

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file
 

if (!require("httr")) install.packages("httr")
if (!require("jsonlite")) install.packages("jsonlite")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")

library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)

# Data Acquisition --------------------------------------------------------

### Access nasdaq API to obtain inflation and exchange rates data
# 2000 calls per 10 minutes: Sys.sleep(0.3)

## First Inflation Data
source("API_Key_Inflation_and_Exchange_Rates.R")

# Specify countries of interest manually using API's abbreviation
countries <- c("DEU", "USA", "FRA", "ITA", "EUR")

url <- "https://data.nasdaq.com/api/v3/datasets/RATEINF/INFLATION_DEU.json?"
time_series <- GET(url, query = list(start_date = "2016-01-01", end_date = "2021-10-31", 
                                     api_key = .nasdaq))

cont <- content(time_series, as = "text")
inflation <- fromJSON(cont)

inflation_data <- data.frame(Date = inflation$dataset$data[, 1], 
                             Inflation = inflation$dataset$data[, 2]) %>%
  mutate(Country = "DEU")

l <- length(countries)

for(j in 2:l){
  
  inf_url <- sprintf("https://data.nasdaq.com/api/v3/datasets/RATEINF/INFLATION_%s.json?", 
                   countries[j])
  
  time_series <- GET(inf_url, query = list(start_date = "2016-01-01", end_date = "2021-10-31", 
                                       api_key = .nasdaq))
  
  cont <- content(time_series, as = "text")
  inflation <- fromJSON(cont)
  
  data <- data.frame(Date = inflation$dataset$data[, 1], 
                     Inflation = inflation$dataset$data[, 2]) %>%
    mutate(Country = countries[j])
  
  inflation_data <- rbind(inflation_data, data)
  
  Sys.sleep(0.3)
}

saveRDS(inflation_data, "output/inflation.rds")
write.csv(inflation_data, "output/inflation.csv")

## Now Exchange Rates for Euro
# Specify countries of interest manually using API's abbreviation
# and add currency code

country <- c("Poland", "New Zealand", "Israel", "South Africa", "Brasil", "Turkey", "USA")
currency <- c("PLN", "NZD", "ILS", "ZAR", "BRL", "TRY", "USD")

ecb_url <- "https://data.nasdaq.com/api/v3/datasets/ECB/EURPLN.json?"
time_series <- GET(ecb_url, query = list(start_date = "2016-01-01", end_data = "2021-10-31", 
                                         api_key = .nasdaq))

cont <- content(time_series, as = "text")
exchange_rate <- fromJSON(cont)

exchange_data <- data.frame(Date = exchange_rate$dataset$data[, 1], 
                            Rate = exchange_rate$dataset$data[, 2])

exchange_data <- exchange_data %>% 
  mutate(Date = as.Date(Date), Rate = as.numeric(Rate), Currency = "PLN", Country = "Poland")

n <- length(currency)


for(i in 2:n){

  ecb_url <- sprintf("https://data.nasdaq.com/api/v3/datasets/ECB/EUR%s.json?", currency[i])

  
  time_series <- GET(ecb_url, query = list(start_date = "2016-01-01", end_date = "2021-10-31", 
                                           api_key = .nasdaq))
  
  cont <- content(time_series, as = "text")
  exchange_rate <- fromJSON(cont)
  
  data <- data.frame(Date = exchange_rate$dataset$data[, 1], 
                     Rate = exchange_rate$dataset$data[, 2])

  data <- data %>% 
    mutate(Date = as.Date(Date), Rate = as.numeric(Rate), Currency = currency[i], 
           Country = country[i])
  
  exchange_data <- rbind(exchange_data, data)
  
  Sys.sleep(0.3)
  
}

## Further data preparation
# Convert daily inflation rates to weekly data

exchange_data <- exchange_data %>%
  mutate(Date = parse_date_time(Date,"ymd")) %>% 
  arrange(Country) %>% 
  group_by(year_week = paste(year(Date), week(Date)), Country, Currency) %>%
  mutate(WeeklyRate = mean(Rate)) %>% 
  ungroup() %>% 
  select(-c("Rate")) %>% 
  distinct(year_week, WeeklyRate, .keep_all = TRUE)


saveRDS(exchange_data, "output/exchange_rates.rds")
write.csv(exchange_data, "output/exchange_rates.csv")




