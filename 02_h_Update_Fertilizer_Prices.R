##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - Fertilizer Prices and Price Index
#### Use this file for updates

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readxl")) install.packages("readxl")

library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(readxl)

# Data Acquisition --------------------------------------------------------

### Using download.file() the data is downloaded from the World Bank

url <- "https://thedocs.worldbank.org/en/doc/5d903e848db1d1b83e0ec8f744e55570-0350012021/related/CMO-Historical-Data-Monthly.xlsx"
temp = tempfile(fileext = ".xlsx")
download.file(url, destfile = temp, mode='wb')

fertilizer <- read_excel(temp, sheet = "Monthly Prices", skip = 3)

# Now find start row and determine last row

n <- ncol(fertilizer)
col_names <- c("Date", paste(fertilizer[1, 2:n], fertilizer[2, 2:n]))

fertilizer <- fertilizer[-1:-3, c(1, 58:62)] 
colnames(fertilizer) <- col_names[c(1, 58:62)]

fertilizer <- fertilizer %>%
  mutate_at(vars(!matches("Date")), as.numeric)

start_row <- which(str_detect(fertilizer$Date, "2016"))[1]
end_row <- nrow(fertilizer)

fertilizer_prices <- fertilizer[start_row:end_row, ] %>% 
  mutate(Year = str_split_fixed(Date, "M", 2)[, 1]) %>%
  mutate(Month = str_split_fixed(Date, "M", 2)[, 2]) %>%
  dplyr::select(-Date)

# Now same for prince index

fertilizer_ind <- read_excel(temp, sheet = "Monthly Indices", skip = 8)[-1, c(1, 13)]
colnames(fertilizer_ind) <- c("Date", "Price_Index")

start_ind <- which(str_detect(fertilizer_ind$Date, "2016"))[1]
end_ind <- nrow(fertilizer_ind)

fertilizer_index <- fertilizer_ind[start_ind:end_ind, ] %>%
  mutate(Year = str_split_fixed(Date, "M", 2)[, 1]) %>%
  mutate(Month = str_split_fixed(Date, "M", 2)[, 2]) %>%
  mutate(Price_Index = as.numeric(Price_Index)) %>%
  dplyr::select(-Date)

fertilizer_data <- inner_join(fertilizer_prices, fertilizer_index, by = c("Year", "Month")) %>%
  mutate(Year = as.numeric(Year), Month = as.numeric(Month))

colnames(fertilizer_data)[1:5] <- c("Phosphate_Rock", "DAP", "TSP", "Urea", "Potassium_chloride")

# Convert to EUR per mt

usd_rate <- readRDS("output/exchange_rates.rds") %>%
  filter(Country == "USA") %>%
  dplyr::select(Date, WeeklyRate) %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  dplyr::select(-Date) %>%
  group_by(Year, Month) %>%
  dplyr::summarise(monthly_rate = mean(WeeklyRate))

# Remove Price Index, but add after 

fertilizer_data <- inner_join(fertilizer_data, usd_rate, by = c("Year", "Month"))

fertilizer_data[, 1:5] <- fertilizer_data[, 1:5] * (1/fertilizer_data$monthly_rate)

fertilizer_data <- fertilizer_data %>%
  dplyr::select(-monthly_rate)


saveRDS(fertilizer_data, "output/fertilizer_prices.rds")
write.csv(fertilizer_data, "output/fertilizer_prices.csv")











