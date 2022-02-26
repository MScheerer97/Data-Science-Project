##### Data Science Project - Price Prediction of Fruits and Vegetables in Germany

#### Data Project - Machine Learning - Standardized Values

# Libraries  --------------------------------------------------------------

rm(list = ls())

## Now: load all necessary packages used throughout the R file

if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("modelr")) install.packages("modelr")
if (!require("stringr")) install.packages("stringr")
if (!require("fastDummies")) install.packages("fastDummies")

library(dplyr)
library(tidyverse)
library(stringr)
library(fastDummies)

#### Loading data and prepare cross-validation
#### General Setup ####

# load machine learning data frame
df_ML_raw <- readRDS("output/final_food_ML.rds")

# establish row number column
## needed to later find correct food etc. 
#df_ML_raw$row_number <- row.names(df_ML_raw)

# shuffle data frame
set.seed(42)
random_rows <- sample(1:nrow(df_ML_raw), nrow(df_ML_raw), replace = FALSE)
df_ML_raw <- df_ML_raw[random_rows, ]

# ensure that no missing values and no character variables are present
sum(is.na(df_ML_raw))

## those four are: food, type, country, size (needed later for prediction analysis)
ncol(df_ML_raw) - ncol(df_ML_raw[, sapply(df_ML_raw, is.numeric) | sapply(df_ML_raw, is.integer)])

## all columns integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer,as.numeric)
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor,as.character)

# drop date column
df_ML_raw <- df_ML_raw %>% select(-date)

# drop all total price variables 
totals <- colnames(df_ML_raw)[str_detect(colnames(df_ML_raw), "price_total")]
df_ML_raw <- df_ML_raw %>% select(-all_of(totals))

# machine learning data frame
df_ML <- df_ML_raw

# scaled values for future preds of models with ALL data
df_scale <- df_ML %>%
  select(-food)
df_scale[, c("type", "country", "size")] <- lapply(df_scale[, c("type", "country", "size")], as.factor)

df_scale <- dummy_cols(
  df_scale,
  remove_first_dummy = FALSE,
  remove_selected_columns = TRUE, 
  select_columns = c("type", "country", "size")
)

scale_mean <- as.data.frame(colMeans(df_scale))
scale_sd <- as.data.frame(sapply(df_scale, sd))

# combine 
scale_parameters <- cbind(scale_mean, scale_sd) %>%
  mutate(variable = rownames(.))
rownames(scale_parameters) <- NULL  
colnames(scale_parameters)[1:2] <- c("mean", "sd")

saveRDS(scale_parameters, paste0("output/data_scaler_complete.rds"))

# now obtain scaling values for different food types

items <- unique(df_ML$food)

for(item in items){
  
  data <- df_ML %>%
    filter(food == item) %>%
    select(-food)

  data[, c("type", "country", "size")] <- lapply(data[, c("type", "country", "size")], as.factor)
  
  data <- dummy_cols(
    data,
    remove_first_dummy = FALSE,
    remove_selected_columns = TRUE, 
    select_columns = c("type", "country", "size")
  )
  
  scale_mean_food <- as.data.frame(colMeans(data))
  scale_sd_food <- as.data.frame(sapply(data, sd))
  
  # combine 
  scaler <- cbind(scale_mean_food, scale_sd_food) %>%
    mutate(variable = rownames(.))
  rownames(scaler) <- NULL  
  colnames(scaler)[1:2] <- c("mean", "sd")
  
  scaler <- scaler %>% 
    filter(sd != 0 )
  
  
  saveRDS(scaler, paste0("models/scaler/data_scaler_", item, "_food.rds"))

}










