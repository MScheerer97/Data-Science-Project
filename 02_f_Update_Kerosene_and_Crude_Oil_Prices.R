##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - Kerosene/Jet Fuel and Crude Oil Price Data Acquisition 
## U.S. Energy Information Administration

## Update Code

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


source("API_Key_Kerosene_Price.R")
url <- "http://api.eia.gov/series/?"

# Load data and add new data to the data frame; also load exchange rates

usd_rate <- readRDS("output/exchange_rates.rds") %>%
  filter(Country == "USA") %>%
  dplyr::select(year_week, WeeklyRate)

kerosene_price <- readRDS("output/kerosene_prices.rds")

# Obtain start and end date
start_date <- str_remove_all(max(kerosene_price$Date), "[[:punct:]]")
end_date <- str_remove_all(Sys.Date(), "[[:punct:]]")

time_series <- GET(url, query = list(series_id = "PET.EER_EPJK_PF4_RGC_DPG.W", 
                                     start = start_date, end = end_date,
                                     api_key = .jet_key))

cont <- content(time_series, as = "text")
kerosene <- fromJSON(cont)

new_kerosene <- data.frame(kerosene$series$data[[1]]) %>% 
  dplyr::rename(Date = X1, Kerosene_Price = X2) %>% 
  mutate(Date = as.Date(Date, format = "%Y%m%d"), 
         Kerosene_Price = as.numeric(Kerosene_Price), 
         year_week = paste(year(Date), week(Date)))

new_kerosene <- inner_join(new_kerosene, usd_rate, by = "year_week") %>%
  mutate(Kerosene_Price = Kerosene_Price*(1/3.78541)*(1/WeeklyRate)) %>%
  dplyr::select(-c(WeeklyRate, year_week))


kerosene_price <- rbind(kerosene_price, new_kerosene) %>%
  arrange(Date) %>%
  distinct(Date, .keep_all = TRUE) 

# Same for Crude Oil

crude_oil_prices <- readRDS("output/crude_oil_prices.rds")

# Obtain start and end date

time_series_oil <- GET(url, query = list(series_id = "STEO.BREPUUS.M ", 
                                         start = start_date, end = end_date,
                                         api_key = .jet_key))

cont <- content(time_series_oil, as = "text")
crude_oil <- fromJSON(cont)

new_oil <- data.frame(crude_oil$series$data[[1]]) %>% 
  dplyr::rename(Date = X1, Crude_Oil_Price = X2) %>% 
  mutate(Date = as.Date(paste0(Date, "01"), format = "%Y%m%d"), 
         Crude_Oil_Price = as.numeric(Crude_Oil_Price), 
         year_week = paste(year(Date), week(Date)))

new_oil <- inner_join(new_oil, usd_rate, by = "year_week") %>%
  mutate(Crude_Oil_Price = Crude_Oil_Price*(1/159)*(1/WeeklyRate)) %>%
  dplyr::select(-c(WeeklyRate, year_week))

crude_oil_prices <- rbind(crude_oil_prices, new_oil) %>%
  arrange(Date) %>%
  distinct(Date, .keep_all = TRUE) 


saveRDS(kerosene_price, "output/kerosene_prices.rds")
write.csv(kerosene_price, "output/kerosene_prices.csv")

saveRDS(crude_oil_prices, "output/crude_oil_prices.rds")
write.csv(crude_oil_prices, "output/crude_oil_prices.csv")
