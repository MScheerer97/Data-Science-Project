##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany


rm(list = ls())

#### Data Project - Food Prices

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("pdftools")) install.packages("pdftools")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("plyr")) install.packages("plyr")





library(tidyverse)
library(pdftools)
library(dplyr)
library(stringr)
library(stringi)
library(plyr)




files <- list.files()
files <- files[str_detect(files, "pdf")]


price_2019_02 <- pdf_text(files[1]) %>%
  str_split("\n")

# Remove redundant pages without data

start <- str_detect(price_2019_02, "Preisbericht")
start <- which(start)[1]

end <- str_detect(price_2019_02, "Speisekartoffeln")
end <- which(end)[1]

price_2019_02 <- price_2019_02[start:end]

# Testing for page 1 

price_2019_02[[1]] <- price_2019_02[[1]] %>%
  stri_omit_empty()

price_2019_02[[1]]

first_entry <- str_match(price_2019_02[[1]], "2019")
first_entry <- which(first_entry == "2019")[1]

# Add and Subtract 1 to remove first row with columns and page indicating row
price_2019_02[[1]] <- price_2019_02[[1]][(first_entry):(length(price_2019_02[[1]])-1)] %>%
  strsplit(split = "\\,\\s\\\"")











