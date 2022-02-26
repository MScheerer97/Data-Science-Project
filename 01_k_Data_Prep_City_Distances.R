##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany

#### Data Project - City Distances

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")

library(readxl)
library(dplyr)
library(tidyverse)

#### Load data and save as rds

# the data is manually collected from "https://www.luftlinie.org"

city_distances <- read_excel("Data/City_Distances.xlsx")
saveRDS(city_distances, "Output/city_distances.rds")









