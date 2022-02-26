##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


#### Data Project - Fertilizer Prices and Price Index

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
  select(-Date)

# Now same for prince index

fertilizer_ind <- read_excel(temp, sheet = "Monthly Indices", skip = 8)[-1, c(1, 13)]
colnames(fertilizer_ind) <- c("Date", "Price_Index")

start_ind <- which(str_detect(fertilizer_ind$Date, "2016"))[1]
end_ind <- nrow(fertilizer_ind)

fertilizer_index <- fertilizer_ind[start_ind:end_ind, ] %>%
  mutate(Year = str_split_fixed(Date, "M", 2)[, 1]) %>%
  mutate(Month = str_split_fixed(Date, "M", 2)[, 2]) %>%
  mutate(Price_Index = as.numeric(Price_Index)) %>%
  select(-Date)

fertilizer_data <- inner_join(fertilizer_prices, fertilizer_index, by = c("Year", "Month"))


saveRDS(fertilizer_data, "output/fertilizer_prices.rds")
write.csv(fertilizer_data, "output/fertilizer_prices.csv")











