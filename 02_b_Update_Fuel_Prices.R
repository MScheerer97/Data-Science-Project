##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - Fuel Price Data Acquisition 
## European Commission Weekly Oil Bulletin
## Update Code

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file
if (!require("pdftools")) install.packages("pdftools") 
if (!require("pdfsearch")) install.packages("pdfsearch") 
if (!require("pdftables")) install.packages("pdftables") 
if (!require("tabulizer")) install.packages("tabulizer") 
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readxl")) install.packages("readxl")

library(pdftools)
library(pdfsearch)
library(pdftables)
library(tabulizer)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)


# Data Acquisition --------------------------------------------------------
### Using download.file() to obtain the pdf with Bulletin Numbers and Dates
### from the website of the European Commission; using this information we can 
### access the individual urls (depend on Bulletin Numbers)


url <- "https://ec.europa.eu/energy/observatory/reports/List-of-WOB.pdf"
temp = tempfile(fileext = ".pdf")
download.file(url, destfile = temp, mode='wb')

pages <- pdf_text(temp) %>% 
  str_split("\n") %>% 
  length(.)

areas <- vector(mode = "list", length = pages)
areas[[1]] <- c(127.7013, 103.1086, 753.5156, 523.7914)
for(i in 2:pages) areas[[i]] <- c(100, 107.2329, 760.3927, 519.6671)

tables <- extract_tables(
  temp, guess = FALSE, 
  method = "decide", area = areas, 
  output = "data.frame")

# First Page: easy because tidy already

reports <- tables[[1]][-1, 1:2] %>% 
  dplyr::rename("Date" = Without.Taxes) 

# Determine last page needed by date and then manipulate all other pages:
# Column names are actually rows

last_page <- last(which(str_detect(tables, "2019")))

for(i in 2:last_page){
  
  first_row <- colnames(tables[[i]])[1:2]
  tables[[i]] <- tables[[i]][, 1:2]
  
  n <- nrow(tables[[i]])
  tables[[i]][n + 1, ] <- str_replace_all(str_remove(first_row, "X"), "\\.", "/") 
  
  colnames(tables[[i]]) <- c("Bulletin", "Date")
  tables[[i]] <- tables[[i]] 
  
  reports <- rbind(reports, tables[[i]])
  
}

## Update Code

fuel_price <- readRDS("output/eu_fuel_prices.rds")
start_date <- max(fuel_price$Date)

new_reports <- reports %>% 
  mutate(Date = as.Date(Date, format = "%d/%m/%Y"), Bulletin = as.numeric(Bulletin)) %>%
  arrange(Date) %>% 
  filter(Date > start_date)

# Obtain new data

for(j in 1:nrow(new_reports)){
    
    published <- str_replace_all(new_reports$Date[j], "-", "_")
    url <- paste0("http://ec.europa.eu/energy/observatory/reports/", published, "_raw_data_", 
                  new_reports$Bulletin[j], ".xlsx")
    
    temp = tempfile(fileext = ".xlsx")
    download.file(url, destfile = temp, mode='wb')
    
    data <- read_excel(temp) %>% 
      dplyr::rename(Date = `Prices in force on`)
    
    # Format of first column differs, preventing rbind to combine the data
    if(is.POSIXct(data$Date)){
      
      data$Date <- as.Date(data$Date)
      
    }
    
    else if(is.character(data$Date)){
      
      # Choosing origin to match the origin of Excel
      data$Date <-  as.Date(as.numeric(data$Date), origin = "1899-12-30")
      
    }
    
    fuel_price <- rbind(fuel_price, data)
  
}

fuel_price <- fuel_price %>%
  filter(!is.na(Date))

saveRDS(fuel_price, "output/eu_fuel_prices.rds")
write.csv(fuel_price, "output/eu_fuel_prices.csv")

#### Now add exchange rates to the exchange rates data frame

ex_rate <- fuel_price %>%
  filter(!(`Currency Code` %in% c("BRL", "ILS", "NZD", "PLN", "TRY", "ZAR", "EUR", 
                                  "GBP", "BGN"))) %>%
  dplyr::select(Date, `Country Name`, `Currency Code`, `Euro exchange rate`) %>%
  distinct()

colnames(ex_rate)[2:4] <- c("Country", "Currency", "Rate")

ex_rate <- ex_rate %>% 
  mutate(Date = parse_date_time(Date,"ymd"), Rate = 1/as.numeric(Rate)) %>% 
  arrange(Country) %>% 
  group_by(year_week = paste(year(Date), week(Date)), Country, Currency) %>%
  mutate(WeeklyRate = mean(Rate)) %>% 
  ungroup() %>% 
  dplyr::select(-c("Rate")) %>% 
  distinct(year_week, WeeklyRate, .keep_all = TRUE) %>%
  arrange(Country, desc(Date))

exchange_rates <- readRDS("output/exchange_rates.rds")
start_date_ex <- exchange_rates %>%
  filter(Country == "Sweden") %>%
  summarise(Date = max(Date))

ex_rate <- ex_rate %>%
  filter(Date > start_date_ex) %>%
  rbind(., exchange_rates) %>%
  arrange(Country, desc(Date))

saveRDS(ex_rate, "output/exchange_rates.rds")
write.csv(ex_rate, "output/exchange_rates.csv")
































































