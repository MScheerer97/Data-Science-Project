##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - Kerosene/Jet Fuel and Crude Oil Price Data Acquisition 
## U.S. Energy Information Administration

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

### Using the public API from the U.S. Energy Information Administration we obtain 
### weekly data on the price for US Kerosene-Type Jet Fuel

source("API_Key_Kerosene_Price.R")

url <- "http://api.eia.gov/series/?"

time_series <- GET(url, query = list(series_id = "PET.EER_EPJK_PF4_RGC_DPG.W", 
                                     start = "20160101", end = "20211031",
                                     api_key = .jet_key))

cont <- content(time_series, as = "text")
kerosene <- fromJSON(cont)

kerosene_data <- data.frame(kerosene$series$data[[1]]) %>% 
  rename(Date = X1, Kerosene_Price = X2) %>% 
  mutate(Date = as.Date(Date, format = "%Y%m%d"), 
         Kerosene_Price = as.numeric(Kerosene_Price), 
         year_week = paste(year(Date), week(Date)))

### Now we access monthly data on the price for Crude Oil; url is the same

time_series_oil <- GET(url, query = list(series_id = "STEO.BREPUUS.M ", 
                                         start = "20160101", end = "20211031",
                                         api_key = .jet_key))

cont <- content(time_series_oil, as = "text")
crude_oil <- fromJSON(cont)

crude_oil_data <- data.frame(crude_oil$series$data[[1]]) %>% 
  rename(Date = X1, Crude_Oil_Price = X2) %>% 
  mutate(Date = as.Date(paste0(Date, "01"), format = "%Y%m%d"), 
         Crude_Oil_Price = as.numeric(Crude_Oil_Price), 
         year_week = paste(year(Date), week(Date)))

## Convert to EUR

usd_rate <- readRDS("output/exchange_rates.rds") %>%
  filter(Country == "USA") %>%
  select(year_week, WeeklyRate)

# Kerosene 

kerosene_data <- inner_join(kerosene_data, usd_rate, by = "year_week") %>%
  mutate(Kerosene_Price = Kerosene_Price*(1/3.78541)*(1/WeeklyRate)) %>%
  select(-WeeklyRate)

# Crude Oil 

crude_oil_data <- inner_join(crude_oil_data, usd_rate, by = "year_week") %>%
  mutate(Crude_Oil_Price = Crude_Oil_Price*(1/159)*(1/WeeklyRate)) %>%
  select(-WeeklyRate)




saveRDS(kerosene_data, "output/kerosene_prices.rds")
write.csv(kerosene_data, "output/kerosene_prices.csv")

saveRDS(crude_oil_data, "output/crude_oil_prices.rds")
write.csv(crude_oil_data, "output/crude_oil_prices.csv")
