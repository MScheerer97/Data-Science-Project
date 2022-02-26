#%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation ####
#%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Content of File ##

# In this script, we prepare all data sources individually.
# Afterwards, we merge them to a three data frames: 
  ## one for descriptive statistics
  ## one for price predictions 
  ## one for the food basket 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### Load Packages ####

# clear workspace
rm(list = ls())

# load packages
  ## for mapvalues() function
if (!require("plyr")) install.packages("plyr") 
library(plyr)
  ## for read_csv() function
if (!require("readr")) install.packages("readr") 
library(readr)
  ## for read_excel() function
if (!require("readxl")) install.packages("readxl") 
library(readxl)
  ## for data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
  ## to identify month from calender week + year
if (!require("ISOweek")) install.packages("ISOweek") 
library(ISOweek)
  ## naniar contains good functions to work with missing values
if (!require("naniar")) install.packages("naniar") 
library(naniar)
  ## to tidy the data frame
if (!require("tidyr")) install.packages("tidyr") 
library(tidyr)
  ## for string manipulation
if (!require("stringr")) install.packages("stringr") 
library(stringr)
if (!require("stringi")) install.packages("stringi") 
library(stringi)
 ## for imputation of NA with last available value
if (!require("zoo")) install.packages("zoo") 
library(zoo)
  ## for missing values imputation
if (!require("missRanger")) install.packages("missRanger") 
library(missRanger)
  ## to create dummy variables
if (!require("fastDummies")) install.packages("fastDummies") 
library(fastDummies)
  ## to work with dates
if (!require("lubridate")) install.packages("lubridate") 
library(lubridate)
  ## to import data with the fread() function
if (!require("data.table")) install.packages("data.table") 
library(data.table)
  ## to create lags with the Hmisc package
if (!require("Hmisc")) install.packages("Hmisc") 
library(Hmisc)

#### Load data ####

df_food_price <- readRDS("output/price_data_updated.rds")
df_kerosene <- readRDS("output/kerosene_prices.rds")
df_inflation <- readRDS("output/inflation.rds")
df_footprint <- readRDS("output/footprint.Rds")
df_exchange_rates <- readRDS("output/exchange_rates.rds")
df_fuel <- readRDS("output/eu_fuel_prices.rds")
df_oil <- readRDS("output/crude_oil_prices.rds")
df_nutrition <- read_csv("output/food_nutrition.csv")
df_season <- read_csv("data/season.csv")
df_fertilizer <- readRDS("output/fertilizer_prices.rds")
df_city_distances <- readRDS("output/city_distances.rds")
df_weather_2000_2014 <- fread("output/weather_2000_2014.csv")
df_weather_2000_2014_banana <- fread("output/weather_banana_2000_2014.csv")
df_weather_2015_j <- fread("output/updated_df_weather.csv")
df_world_container_index <- fread("output/world_container_index.csv")
df_weather_forecast <- fread("output/prediction_weather_forecast.csv")  


#### Final Data Preparation for Merging ####

#### food prices ####
#%%%%%%%%%%%%%%%%%%%# 

#  ensure that all week numbers have two entries
df_food_price$week <- stri_pad_left(df_food_price$week, 2, 0)

# add month to calender week (needed to merge season)
df_food_price$year_week <- paste0(df_food_price$year, "-W", 
                                  df_food_price$week, "-1")

for (i in 1:nrow(df_food_price)) {
  # generate date 
  df_food_price[i, "date"] <- 
    ISOweek2date(df_food_price[i, "year_week"])
  # extract month
  # df_food_price[i, "month"] <- 
  #   str_remove(format(df_food_price[i, "date"], "%m"), "^0+") 
}

# df_food_price$month <- as.numeric(df_food_price$month)
## drop year_week variable (not needed anymore)
df_food_price <- df_food_price %>% dplyr::select(-"year_week")
## all price variables as numeric
df_food_price[, c("price_total", "price_F", "price_H", 
                  "price_K", "price_M", "price_B")] <- sapply(
                    df_food_price[, c("price_total", "price_F", "price_H", 
                                      "price_K", "price_M", "price_B")],
                    as.numeric
                  )

# drop all observations with any missings in food, type, country and size
# something definitely went wrong with the data input
df_food_price <- df_food_price[complete.cases(
  df_food_price[, c("food", "type", "country", "size")]
  ),]


# Keep only selection of food-type-country-size observations #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## we keep only fruits which were observed at least 20 weeks in the year 2021
## we identified those fruit-type-country-size combination as follows
## and stored them into a .xlsx file.
## In the following only this file is loaded.
## We do not change this anymore, as already 23 fruits and vegetables are
## selected. Otherwise we get too many models. 
# df_keep <- df_food_price %>%
#   filter(year == "2021") %>%
#   select(food, type, country, size) %>%
#   group_by(food, type, country, size) %>%
#   count() %>%
#   na.omit()
# 
# if ("freq" %in% colnames(df_keep)) {
#   df_keep <- df_keep %>%
#     filter(freq >= 20) %>%
#     select(-freq)
# } else if ("n" %in% colnames(df_keep)) {
#   df_keep <- df_keep %>%
#     filter(n >= 20) %>%
#     select(-n)
# }
# 
# unique(df_keep$food)
# unique(df_keep$type)
# unique(df_keep$country)
# unique(df_keep$size)
# 
# write.csv(df_keep, "output/food_types_final_analysis.csv", row.names = FALSE)

df_keep <- read_csv("output/food_types_final_analysis.csv")

# filter final data frame
df_food_price <- inner_join(
  df_food_price, df_keep, by = c("food", "type", "country", "size")
)


# replace missing values of prices #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## to do so, we use chained random forests
## setting pmm.k to a positive number leads to combining the imputations with a 
## predictive mean matching step. This option avoids imputation with values not 
## present in the original data. Moreover, the resulting imputed values are more
## natural and the distributional properties are improved. 
## we do explicitly not use a linear regression based approach as this may
## result in negative values.

# set unrealistic prices to NA
  ## we determine as unrealistic prices, prices below 10Euro/100kg
df_food_price <- df_food_price %>%
  mutate_at(vars(starts_with("price_")), ~ replace(., .< 10, NA))

# extract all food-type-country-size combinations
combis_replace <- df_food_price %>%
  dplyr::select(food, type) %>%
  distinct() 

# iterate over all combinations in order to replace the missing values
df_food_price_replace_final <- data.frame()

for (i in 1:nrow(combis_replace)) {
  
  # extract selected combination in iteration i
  combis_replace_sub <- combis_replace[i, ]
  
  # create data set for selected combination
  df_food_price_replace <- df_food_price %>%
    filter(food == combis_replace_sub$food, type == combis_replace_sub$type#,
           #country == combis_replace_sub$country, size == combis_replace_sub$size
           )
  
  # make replacement
    ## check if enough observations are available to replace using predictive mean matching
    ## if less than 5 for any price, no predictive mean matching is used
    ## if between 5 and 19 price observations, predictive mean matching using 5 values is supplied
    ## if 20 or more, predictive mean matching using 20 values is supplied. 
    ## number of trees is set to a small number to decrease the computation time
    ## the candidate of non-missing values to sample from in the predictive mean
    ## matching step is specified by pmm.k
    ## a seed is set to ensure reproducibility 
  if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, formula = price_total + price_F + price_H + price_K + price_M + price_B ~
                 price_total + price_F + price_H + price_K + price_M + country + size,
                 num.trees = 100, pmm.k = 0, returnOOB = TRUE, seed = 42)
  } else if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 20) &
                 any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) >= 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, formula = price_total + price_F + price_H + price_K + price_M + price_B ~
                 price_total + price_F + price_H + price_K + price_M + country + size,
                 num.trees = 100, pmm.k = 5, returnOOB = TRUE, seed = 42)
  } else {
    df_food_price_replace <-
      missRanger(df_food_price_replace, formula = price_total + price_F + price_H + price_K + price_M + price_B ~
                   price_total + price_F + price_H + price_K + price_M + country + size,
                 num.trees = 100, pmm.k = 20, returnOOB = TRUE, seed = 42)
  }
  
  df_food_price_replace_final <- rbind(df_food_price_replace_final, df_food_price_replace)
}

# round all prices to 0 decimal places (in original document they are rounded)
df_food_price_replace_final <- df_food_price_replace_final %>%
  mutate_at(vars(starts_with("price_")), ~round(.))

# check distribution of prices (are they realistic?)
  ## yes: relatively low prices are for onions and relatively high prices for strawberries
  ## this matches the prices from the PDF documents
summary(df_food_price_replace_final$price_total)
summary(df_food_price_replace_final$price_F)
summary(df_food_price_replace_final$price_H)
summary(df_food_price_replace_final$price_K)
summary(df_food_price_replace_final$price_M)
summary(df_food_price_replace_final$price_B)


# adjust df_food_price data frame with new results
df_food_price <- df_food_price_replace_final
  ## check if all missing values are replaced
sum(is.na(df_food_price))


# create data set without missing weeks per food-type-country-size combination #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

df_food_price_continuous <- df_food_price %>%
  # group by food-type-country-size information
  group_by(food, type, country, size) %>%
  # add missing dates
  tidyr::complete(date = seq(min(df_food_price$date), 
                             max(df_food_price$date), by = "week")) %>%
  # extract year and week from dates
  mutate(
    year = as.character(year(date)),
    week = str_sub(date2ISOweek(date), 7, 8) 
  ) %>%
  # fill missing values
  group_by(food, type, country, size) %>%
  fill(fruits, veggies, food, type, country, size) %>%
  # ungroup data frame
  ungroup() 

# add indicator to identify later if observation is real or inserted
df_food_price_check <- df_food_price %>%
  dplyr::select("year", "week", "date", "country", "food", "type", "size") %>%
  mutate(flag = 1) %>%
  mutate_at(vars(year), as.character)
df_food_price_continuous <- left_join(
  df_food_price_continuous, df_food_price_check,
  by = c("year", "week", "date", "country", "food", "type", "size")
)
df_food_price_continuous <- df_food_price_continuous %>%
  mutate_at(vars(flag), ~replace(., is.na(.), 0))

# in case we generated duplicates, we keep the observation with flag = 1, i.e.
# original information
df_food_price_continuous <- df_food_price_continuous %>%
  arrange(food, type, country, size, year, week, flag)
df_food_price_continuous <- df_food_price_continuous[
  !duplicated(df_food_price_continuous[, 
                                       c("year", "week", "country", "food", "type", "size")]), 
  ]

# there are still missing values in fruits and veggies 
  ## create indicator for fruit and vegetables again
fruits <- c("Apple", "Peach", "Pear", "Grapes", "Strawberry", "Kiwi", "Orange", "Clementinen",
            "Mandarin", "Lemon", "Banana", "Satsumas", "Plum", "Nectarine", "Apricot", "Cherry")
veggies <- c("Artichoke", "Eggplant", "Cauliflower", "Carrot", "Beans", "Iceberg Lettuce", 
             "Endive", "Cucumber", "Tomato", "Bell Pepper", "Leek", "Onion", "Zucchini",
             "Potatoes", "Lettuce", "Asparagus", "Brussel Sprouts")
df_food_price_continuous$fruits <- 
  ifelse(df_food_price_continuous$food %in% fruits, 1, 0)
df_food_price_continuous$veggies <- 
  ifelse(df_food_price_continuous$food %in% veggies, 1, 0)

# create month from date
df_food_price_continuous$month <- as.integer(month(df_food_price_continuous$date))

# replace missing values of prices again #
##%%%

# extract all food-type-country-size combinations
combis_replace <- df_food_price_continuous %>%
  dplyr::select(food, type) %>%
  distinct() 

# iterate over all combinations in order to replace the missing values
df_food_price_replace_final <- data.frame()

for (i in 1:nrow(combis_replace)) {
  
  # get column names with missings
  col_missings <-
    colnames(df_food_price_continuous)[sapply(df_food_price_continuous, 
                                              function(x)any(is.na(x)))]
  
  # create columns used for replacement
  formula_replace <- paste(paste(col_missings, collapse = " + "), "~", 
                           "price_total + price_F + price_H + price_K + price_M + country + size")
  
  # extract selected combination in iteration i
  combis_replace_sub <- combis_replace[i, ]
  
  # create data set for selected combination
  df_food_price_replace <- df_food_price_continuous %>%
    filter(food == combis_replace_sub$food, type == combis_replace_sub$type#,
           #country == combis_replace_sub$country, size == combis_replace_sub$size
    )
  
  # make replacement
  ## check if enough observations are available to replace using predictive mean matching
  ## if less than 5 for any price, no predictive mean matching is used
  ## if between 5 and 19 price observations, predictive mean matching using 5 values is supplied
  ## if 20 or more, predictive mean matching using 20 values is supplied. 
  ## number of trees is set to a small number to decrease the computation time
  ## the candidate of non-missing values to sample from in the predictive mean
  ## matching step is specified by pmm.k
  ## a seed is set to ensure reproducibility 
  if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, 
                 formula = formula(formula_replace),
                 num.trees = 100, pmm.k = 0, returnOOB = TRUE, seed = 42)
  } else if (any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) < 20) &
             any(colSums(!is.na(df_food_price_replace %>% dplyr::select(starts_with("price_")))) >= 5)) {
    df_food_price_replace <-
      missRanger(df_food_price_replace, 
                 formula = formula(formula_replace),
                 num.trees = 100, pmm.k = 5, returnOOB = TRUE, seed = 42)
  } else {
    df_food_price_replace <-
      missRanger(df_food_price_replace, 
                 formula = formula(formula_replace),
                 num.trees = 100, pmm.k = 20, returnOOB = TRUE, seed = 42)
  }
  
  df_food_price_replace_final <- rbind(df_food_price_replace_final, df_food_price_replace)
}
df_food_price_continuous <- df_food_price_replace_final


# if there are still missing values left, replace by mean price
if (sum(is.na(df_food_price_continuous[, 
                                       c("price_total", "price_F",
                                         "price_H", "price_K", "price_M", 
                                         "price_B")])) > 0) {
  # get column names with missings
  col_missings <-
    colnames(df_food_price_continuous)[sapply(df_food_price_continuous, 
                                              function(x)any(is.na(x)))]
  # iterate ober column
  for (i in col_missings) {
    df_food_price_continuous[is.na(df_food_price_continuous[,i]), i] <- 
      round(mean(df_food_price_continuous[,i] %>% pull(), na.rm = TRUE))
  }
  
}

# ensure that prices across combinations are not constant
  ## this is unrealistic and makes problems for bsts model as standard deviation 
  ## or rather variance is zero.
  ## if they are we drop this food-type-size-country combination, as this emerged
  ## due to very few observations for this combination
  ## to do so, we calculate the variance for each food-type-size-country combination
  ## combinations
df_food_combis_zero_sd <-
  df_food_price_continuous %>%
  # group by food-type-size-country combinations
  group_by(food, type, size, country) %>%
  # calculate variance
  dplyr::summarise(
    var_price_F = var(price_F, na.rm = TRUE),
    var_price_H = var(price_H, na.rm = TRUE),
    var_price_K = var(price_K, na.rm = TRUE),
    var_price_M = var(price_M, na.rm = TRUE),
    var_price_B = var(price_B, na.rm = TRUE)
    ) %>%
  # set variance with 0 NA
  mutate_all(na_if, 0)
# extract combinations where variance is zero
df_combis_zero_sd_drop <-
  df_food_combis_zero_sd[rowSums(is.na(df_food_combis_zero_sd)) > 0,
                       c("food", "type", "size", "country")] 
# drop those combinations
df_food_price_continuous <-
  # keep data which is in df_food_price_continuous, but not in df_combis_zero_sd_drop
  anti_join(
    df_food_price_continuous, 
    subset(df_food_price_continuous, 
           food %in% df_combis_zero_sd_drop$food &
           type %in% df_combis_zero_sd_drop$type &
           size %in% df_combis_zero_sd_drop$size &
           country %in% df_combis_zero_sd_drop$country  
    )
  )

sum(is.na(df_food_price_continuous[, c("price_total", "price_F",
                                       "price_H", "price_K", "price_M", "price_B")]))
sum(is.na(df_food_price_continuous))

# check if no duplicates are left
sum(duplicated(df_food_price_continuous[, c("year", "week", "country", "food", "type", "size")]))

# round all prices to 0 decimal places (in original document they are rounded)
df_food_price_replace_final <- df_food_price_replace_final %>%
  mutate_at(vars(starts_with("price_")), ~round(.))

## Adding lags of prices: one week, 
df_food_price_continuous <- df_food_price_continuous %>%
    dplyr::group_by(type, size, country) %>%
    arrange(date) %>% 
  
  # Now insert lags
    mutate(price_total_lag_one_week = dplyr::lag(price_total, n = 1), 
           price_total_lag_one_month = dplyr::lag(price_total, n = 4), 
           price_total_lag_one_year = dplyr::lag(price_total, n = 52), 
           
           price_F_lag_one_week = dplyr::lag(price_F, n = 1), 
           price_F_lag_one_month = dplyr::lag(price_F, n = 4), 
           price_F_lag_one_year = dplyr::lag(price_F, n = 52), 
           
           price_H_lag_one_week = dplyr::lag(price_H, n = 1), 
           price_H_lag_one_month = dplyr::lag(price_H, n = 4), 
           price_H_lag_one_year = dplyr::lag(price_H, n = 52),
           
           price_K_lag_one_week = dplyr::lag(price_K, n = 1), 
           price_K_lag_one_month = dplyr::lag(price_K, n = 4), 
           price_K_lag_one_year = dplyr::lag(price_K, n = 52),
           
           price_M_lag_one_week = dplyr::lag(price_M, n = 1), 
           price_M_lag_one_month = dplyr::lag(price_M, n = 4), 
           price_M_lag_one_year = lag(price_M, n = 52),
           
           price_B_lag_one_week = dplyr::lag(price_B, n = 1), 
           price_B_lag_one_month = dplyr::lag(price_B, n = 4), 
           price_B_lag_one_year = dplyr::lag(price_B, n = 52)) %>%
  
  mutate_at(vars(price_total_lag_one_week, price_total_lag_one_month, price_total_lag_one_year, 
                 price_F_lag_one_week, price_F_lag_one_month, price_F_lag_one_year, 
                 price_H_lag_one_week, price_H_lag_one_month, price_H_lag_one_year, 
                 price_K_lag_one_week, price_K_lag_one_month, price_K_lag_one_year, 
                 price_M_lag_one_week, price_M_lag_one_month, price_M_lag_one_year, 
                 price_B_lag_one_week, price_B_lag_one_month, price_B_lag_one_year
                 ),
            funs(na.locf(., na.rm = FALSE, fromLast = TRUE))) %>%
  ungroup()


# check for missings
sum(is.na(df_food_price_continuous))

# drop duplicates
df_food_price_continuous <- df_food_price_continuous %>%
  distinct()
sum(duplicated(df_food_price_continuous))

# check distribution of prices (are they realistic?)
## yes: relatively low prices are for onions and relatively high prices for strawberries
## this matches the prices from the PDF documents
summary(df_food_price_continuous$price_total)
summary(df_food_price_continuous$price_F)
summary(df_food_price_continuous$price_H)
summary(df_food_price_continuous$price_K)
summary(df_food_price_continuous$price_M)
summary(df_food_price_continuous$price_B)

#### kerosene ####
#%%%%%%%%%%%%%%%%#

weeks_food <- unique(paste0(df_food_price_continuous$year, df_food_price_continuous$week))
  ## calender week from date
df_kerosene$week <- 
  strftime(df_kerosene$Date, format = "%V")
  ## year from date
df_kerosene$year <- 
  strftime(df_kerosene$Date, format = "%Y")
  ## drop date (not needed anymore)
df_kerosene <- df_kerosene %>% dplyr::select(-Date) %>%
  mutate("Kerosene_Price_lag_one_week" = lag(Kerosene_Price, n = 1), 
          "Kerosene_Price_lag_two_week" = lag(Kerosene_Price, n = 2), 
          "Kerosene_Price_lag_three_week" = lag(Kerosene_Price, n = 3))
df_kerosene <- na.locf(df_kerosene, na.rm = TRUE, fromLast = TRUE)

mis <- which(!weeks_food %in% sort(paste0(df_kerosene$year, df_kerosene$week)))
mis_weeks <- str_extract(weeks_food[mis], "[[:digit:]]{2}$")
mis_years <- str_extract(weeks_food[mis], "[[:digit:]]{4}")

fill_kerosene <- data.frame(matrix(nrow = length(mis), ncol = length(colnames(df_kerosene))))
colnames(fill_kerosene) <- colnames(df_kerosene)
fill_kerosene <- fill_kerosene %>%
  mutate(year = mis_years, week = mis_weeks)
  
df_kerosene <- rbind(df_kerosene, fill_kerosene) %>%
  arrange(year, week) 

df_kerosene <- na.locf(df_kerosene, na.rm = FALSE, fromLast = FALSE)
  
#### inflation ####
#%%%%%%%%%%%%%%%%%#

# calender week from date
df_inflation$week <- 
  strftime(df_inflation$Date, format = "%V")

# year from date
df_inflation$year <- 
  strftime(df_inflation$Date, format = "%Y")

# drop date (not needed anymore)
df_inflation <- df_inflation %>% dplyr::select(-Date)

# map country names to country code
df_inflation$Country <- mapvalues(
  df_inflation$Country,
  from = c("DEU", "FRA", "ITA"),
  to = c("Germany", "France", "Italy")
)
colnames(df_inflation) <- c("inflation_perc", "country", "week", "year")

# keep only countries from which we have price information
countries_inflation_keep <- unique(df_inflation$country)[
  unique(df_inflation$country) %in% unique(df_food_price_continuous$country)
]
df_inflation <- df_inflation %>%
  filter(country %in% countries_inflation_keep)

# drop rows with missing values (in country)
df_inflation <- df_inflation[!is.na(df_inflation$country), ]
df_inflation <- na.omit(df_inflation)

## Add values for missing calendar weeks and insert latest available infl_rate
## Creating temp dataframe

years <- sort(rep(as.character(unique(df_inflation$year)), 53))
kws <- rep(c(paste0("0", 1:9), as.character(10:53)), length(unique(years))) 
countries <- unique(df_inflation$country)
temp_infl <- data.frame(country = countries[1], week = kws, year = years)

for (i in 2:length(countries)) {
  
  data <- data.frame(country = countries[i], week = kws, year = years)
  temp_infl <- rbind(temp_infl, data)
  
}

# compute missing week year combination and bind to inflation with NA as values

available <- paste0(df_inflation[, 2], df_inflation[, 3], df_inflation[, 4])
check_values <- paste0(temp_infl[, 1], temp_infl[, 2], temp_infl[, 3])
impute_values <- which(!(check_values %in% available))

last_infl <- NA
temp_infl <- cbind(inflation_perc = last_infl, temp_infl)

df_inflation <- rbind(df_inflation, temp_infl[impute_values, ]) %>%
  group_by(country) %>%
  arrange(year, week) %>%
  mutate(inflation_perc = na.locf(inflation_perc, na.rm = FALSE, fromLast = TRUE)) %>%
  mutate(inflation_perc = na.locf(inflation_perc, na.rm = FALSE)) %>%
  mutate("inflation_perc_lag_one_month" = lag(inflation_perc, n = 4), 
         "inflation_perc_lag_two_month" = lag(inflation_perc, n = 8), 
         "inflation_perc_lag_three_month" = lag(inflation_perc, n = 12)) %>%
  ungroup() %>%
  arrange(country, year, week) 

df_inflation <- na.locf(df_inflation, na.rm = TRUE, fromLast = TRUE)

df_inflation <- df_inflation %>%
  pivot_wider(names_from = country, 
              values_from = c(inflation_perc, inflation_perc_lag_one_month, 
                              inflation_perc_lag_two_month, 
                              inflation_perc_lag_three_month))

y <- year(Sys.Date())
w <- week(Sys.Date())

# keep only German inflation rate
drop_infl <- c("EUR", "France", "USA", "Italy")
drop_col_nums <- colnames(df_inflation)[str_detect(colnames(df_inflation), 
                                               paste(drop_infl, collapse = "|"))]


df_inflation <- df_inflation %>%
  filter(!(year == y & week > w)) %>%
  dplyr::select(-all_of(drop_col_nums))

# fill data with last value
weeks_food <- unique(paste0(df_food_price_continuous$year, df_food_price_continuous$week))

mis <- which(!weeks_food %in% sort(paste0(df_inflation$year, df_inflation$week)))
mis_weeks <- str_extract(weeks_food[mis], "[[:digit:]]{2}$")
mis_years <- str_extract(weeks_food[mis], "[[:digit:]]{4}")

fill_infl <- data.frame(matrix(nrow = length(mis), ncol = length(colnames(df_inflation))))
colnames(fill_infl) <- colnames(df_inflation)
fill_infl <- fill_infl %>%
  mutate(year = mis_years, week = mis_weeks)

df_inflation <- rbind(df_inflation, fill_infl) %>%
  arrange(year, week) 

df_inflation <- na.locf(df_inflation, na.rm = FALSE, fromLast = FALSE)
  
  
#### fertilizer prices ####
#%%%%%%%%%%%%%%%%%%%%%%%%%#

## use date from exchange_rates to obtain weeks per month

colnames(df_fertilizer)[c(6, 7)] <- c("year", "month")

weeks <- 
  strftime(df_exchange_rates$Date, format = "%V")
year <- 
  as.numeric(strftime(df_exchange_rates$Date, format = "%Y"))
month <- 
  parse_number(strftime(df_exchange_rates$Date, format = "%m"))

time_data <- data.frame(week = weeks, year = year, month = month)

df_fertilizer <- inner_join(df_fertilizer, time_data, by = c("year", "month")) %>%
  distinct() %>%
  arrange(year, week) %>%
  dplyr::select(-month) %>%
  mutate("Phosphate_Rock_lag_one_month" = lag(Phosphate_Rock, n = 5), 
         "Phosphate_Rock_lag_two_month" = lag(Phosphate_Rock, n = 9), 
         "Phosphate_Rock_lag_three_month" = lag(Phosphate_Rock, n = 13), 
         "DAP_lag_one_month" = lag(DAP, n = 5), 
         "DAP_lag_two_month" = lag(DAP, n = 9), 
         "DAP_lag_three_month" = lag(DAP, n = 13),
         "TSP_lag_one_month" = lag(TSP, n = 5), 
         "TSP_lag_two_month" = lag(TSP, n = 9), 
         "TSP_lag_three_month" = lag(TSP, n = 13),
         "Urea_lag_one_month" = lag(Urea, n = 5), 
         "Urea_lag_two_month" = lag(Urea, n = 9), 
         "Urea_lag_three_month" = lag(Urea, n = 13),
         "Potassium_chloride_lag_one_month" = lag(Potassium_chloride, n = 5), 
         "Potassium_chloride_lag_two_month" = lag(Potassium_chloride, n = 9), 
         "Potassium_chloride_lag_three_month" = lag(Potassium_chloride, n = 13))

df_fertilizer <- na.locf(df_fertilizer, na.rm = FALSE, fromLast = TRUE) %>%
  distinct(year, week, .keep_all = TRUE)
  
# year as character
df_fertilizer$year <- as.character(df_fertilizer$year)

mis <- which(!weeks_food %in% sort(paste0(df_fertilizer$year, df_fertilizer$week)))
mis_weeks <- str_extract(weeks_food[mis], "[[:digit:]]{2}$")
mis_years <- str_extract(weeks_food[mis], "[[:digit:]]{4}")

fill_fertilizer <- data.frame(matrix(nrow = length(mis), ncol = length(colnames(df_fertilizer))))
colnames(fill_fertilizer) <- colnames(df_fertilizer)
fill_fertilizer <- fill_fertilizer %>%
  mutate(year = mis_years, week = mis_weeks)

df_fertilizer <- rbind(df_fertilizer, fill_fertilizer) %>%
  arrange(year, week) 

df_fertilizer <- df_fertilizer %>% 
  na.locf(., na.rm = FALSE) %>%
  na.locf(., na.rm = FALSE, fromLast = TRUE)


#### exchange rate ####
#%%%%%%%%%%%%%%%%%%%%%#

# calender week from date
df_exchange_rates$week <- 
  strftime(df_exchange_rates$Date, format = "%V")

# year from date
df_exchange_rates$year <- 
  strftime(df_exchange_rates$Date, format = "%Y")

# drop date and year_week (not needed anymore)
df_exchange_rates <- df_exchange_rates %>% 
  dplyr::select(-c(Date, year_week))

# column names
colnames(df_exchange_rates) <- 
  c("country", "currency", "exchange_rate", "week", "year")

# keep only countries which are also used in our analyis, i.e. countries
# from which fruit and vegetables come from
df_exchange_rates[df_exchange_rates$country == "Czechia", "country"] <- "Czech Republic"
countries_exchange_keep <- unique(df_exchange_rates$country)[
  unique(df_exchange_rates$country) %in% unique(df_food_price_continuous$country)
]
df_exchange_rates <- df_exchange_rates %>%
  filter(country %in% countries_exchange_keep)

# in case of duplicate keep mean
df_exchange_rates <- df_exchange_rates %>% 
  # group_by(currency, country, week, year) %>% 
  # mutate(exchange_rate = mean(exchange_rate)) %>%
  # distinct() %>%
  # ungroup() %>%
  
  # now first values is chosen (for simplicity, as the code before leads to the same value for all elements)
  group_by(country, currency) %>%
  arrange(year, week) %>%
  dplyr::mutate("exchange_rate_lag_one_week" = lag(exchange_rate, n = 1), 
         "exchange_rate_lag_two_week" = lag(exchange_rate, n = 2), 
         "exchange_rate_lag_three_week" = lag(exchange_rate, n = 3)) %>%
  arrange(country, year, week) %>%
  distinct(year, week, .keep_all = TRUE) %>%
  ungroup()

df_exchange_rates <- na.locf(df_exchange_rates, na.rm = FALSE, fromLast = TRUE)

df_exchange_rates <- df_exchange_rates %>%
  pivot_wider(names_from = c(country, currency), 
              values_from = c(exchange_rate, exchange_rate_lag_one_week, 
                              exchange_rate_lag_two_week, 
                              exchange_rate_lag_three_week))

df_exchange_rates <- na.locf(df_exchange_rates, na.rm = FALSE)

# adjust column names: remove last digits indicating country code and add "_"
  ## remove country code: last digits after "_"
colnames(df_exchange_rates) <- sub("_[^_]+$", "", colnames(df_exchange_rates))
  ## ad "_"
colnames(df_exchange_rates) <- str_replace_all(colnames(df_exchange_rates), " ", "_")

# remove_cols <- colnames(df_exchange_rates)[str_detect(colnames(df_exchange_rates), 
#                                                       "CZK|DKK|HUF|SEK|RON|HRK")]
# df_exchange_rates <- df_exchange_rates %>%
#   select(-all_of(remove_cols))

mis <- which(!weeks_food %in% sort(paste0(df_exchange_rates$year, df_exchange_rates$week)))
mis_weeks <- str_extract(weeks_food[mis], "[[:digit:]]{2}$")
mis_years <- str_extract(weeks_food[mis], "[[:digit:]]{4}")

fill_exchange_rates <- data.frame(matrix(nrow = length(mis), ncol = length(colnames(df_exchange_rates))))
colnames(fill_exchange_rates) <- colnames(df_exchange_rates)
fill_exchange_rates <- fill_exchange_rates %>%
  mutate(year = mis_years, week = mis_weeks)

df_exchange_rates <- rbind(df_exchange_rates, fill_exchange_rates) %>%
  arrange(year, week) 

df_exchange_rates <- df_exchange_rates %>% 
  na.locf(., na.rm = FALSE) 



#### oil ####
#%%%%%%%%%%%#

# calender week from date
df_oil$week <- 
  strftime(df_oil$Date, format = "%V")

# year from date
df_oil$year <- 
  strftime(df_oil$Date, format = "%Y")

#### fill missing weeks with latest value

years <- sort(rep(as.character(unique(df_oil$year)), 53))
kws <- rep(c(paste0("0", 1:9), as.character(10:53)), length(unique(years))) 
temp_oil <- data.frame(week = kws, year = years)

available_oil <- paste0(df_oil$year, df_oil$week)
check_values_oil <- paste0(temp_oil$year, temp_oil$week)
impute_values_oil <- which(!(check_values_oil %in% available_oil))

last_oil <- NA
temp_oil <- cbind(Crude_Oil_Price = last_oil, temp_oil)

## drop date 
df_oil <- df_oil %>% 
  dplyr::select(-Date)

df_oil <- rbind(df_oil, temp_oil[impute_values_oil, ]) %>%
  arrange(year, week) %>%
  mutate(Crude_Oil_Price = na.locf(Crude_Oil_Price, na.rm = FALSE)) %>%
  mutate(Crude_Oil_Price = na.locf(Crude_Oil_Price, na.rm = FALSE, fromLast = TRUE)) %>%
  mutate("Crude_Oil_Price_lag_one_month" = lag(Crude_Oil_Price, n = 4), 
         "Crude_Oil_Price_lag_two_month" = lag(Crude_Oil_Price, n = 8), 
         "Crude_Oil_Price_lag_three_month" = lag(Crude_Oil_Price, n = 12))

df_oil <- na.locf(df_oil, na.rm = FALSE, fromLast = TRUE)

mis <- which(!weeks_food %in% sort(paste0(df_oil$year, df_oil$week)))
mis_weeks <- str_extract(weeks_food[mis], "[[:digit:]]{2}$")
mis_years <- str_extract(weeks_food[mis], "[[:digit:]]{4}")

fill_oil <- data.frame(matrix(nrow = length(mis), ncol = length(colnames(df_oil))))
colnames(fill_oil) <- colnames(df_oil)
fill_oil <- fill_oil %>%
  mutate(year = mis_years, week = mis_weeks)

df_oil <- rbind(df_oil, fill_oil) %>%
  arrange(year, week) 

df_oil <- df_oil %>% 
  na.locf(., na.rm = FALSE) 

y <- year(Sys.Date())
w <- week(Sys.Date())

df_oil <- df_oil %>%
  filter(!(year == y & week > w)) %>%
  distinct(year, week, .keep_all = TRUE)

#### fuel ####
#%%%%%%%%%%%%#

# calender week from date
df_fuel$week <- 
  strftime(df_fuel$Date, format = "%V")

# year from date
df_fuel$year <- 
  strftime(df_fuel$Date, format = "%Y")

# drop date 
df_fuel <- df_fuel %>% 
  dplyr::select(-c(Date, `Country EU Code`))

# keep only countries for which we also have prices
countries_fuel_keep <- unique(df_fuel$`Country Name`)[
  unique(df_fuel$`Country Name`) %in% unique(df_food_price_continuous$country)
]
df_fuel <- df_fuel %>%
  filter(`Country Name` %in% countries_fuel_keep)

# further data prep
df_fuel <- df_fuel %>%
    ## correctly identify missing values as such
  replace_with_na(replace = list(`Weekly price with taxes` = "N.A",
                                 `Euro exchange rate` = "N.A")) %>%
    ## drop rows that contain missing values in price or exchange rate
  drop_na(`Weekly price with taxes`, `Euro exchange rate`) %>%
    ## We keep only super and diesel since they are commonly used
    ## for transportation
  filter(`Product Name` %in% c("Automotive gas oil", "Euro-super 95")) %>%
    ## drop weird fuel price including a comma (removing comma does not make sense)
  #filter(!str_detect(`Weekly price with taxes`, ",")) %>%
    ## Since prices are always paid with taxes, we keep this price information
    ## Moreover, all prices should be in Euro -> we convert them
  mutate(
    fuel_price_1000l = str_replace_all(`Weekly price with taxes`, ",", "")#, 
    ) %>%
    ## select variables of interest
  dplyr::select(`Country Name`, `Product Name`, fuel_price_1000l, week, year) %>%
    ## spread data frame
  spread(`Product Name`, fuel_price_1000l) %>%
    ## rename columns
  dplyr::rename(
    country = `Country Name`,
    fuel_price_diesel = `Automotive gas oil`, 
    fuel_price_super = `Euro-super 95`
  ) 

df_fuel <- df_fuel %>% 
  group_by(country) %>%
  arrange(year, week) %>%
  mutate(fuel_price_diesel = as.numeric(fuel_price_diesel)/1000, 
         fuel_price_super = as.numeric(fuel_price_super)/1000) %>%
  mutate("fuel_price_diesel_lag_one_week" = lag(fuel_price_diesel, n = 1), 
         "fuel_price_diesel_lag_two_week" = lag(fuel_price_diesel, n = 2), 
         "fuel_price_diesel_lag_three_week" = lag(fuel_price_diesel, n = 3), 
         "fuel_price_super_lag_one_week" = lag(fuel_price_super, n = 1), 
         "fuel_price_super_lag_two_week" = lag(fuel_price_super, n = 2), 
         "fuel_price_super_lag_three_week" = lag(fuel_price_super, n = 3)) %>%
  ungroup() %>%
  arrange(country, year, week)

df_fuel <- na.locf(df_fuel, na.rm = FALSE, fromLast = TRUE)

# change data frame structure in wide format 
df_fuel <- df_fuel %>%
  pivot_wider(names_from = country, 
              values_from = df_fuel %>% 
                dplyr::select(starts_with("fuel_price_")) %>% 
                colnames())

mis <- which(!weeks_food %in% sort(paste0(df_fuel$year, df_fuel$week)))
mis_weeks <- str_extract(weeks_food[mis], "[[:digit:]]{2}$")
mis_years <- str_extract(weeks_food[mis], "[[:digit:]]{4}")

fill_fuel <- data.frame(matrix(nrow = length(mis), ncol = length(colnames(df_fuel))))
colnames(fill_fuel) <- colnames(df_fuel)
fill_fuel <- fill_fuel %>%
  mutate(year = mis_years, week = mis_weeks)

df_fuel <- rbind(df_fuel, fill_fuel) %>%
  arrange(year, week) 

df_fuel <- df_fuel %>% 
  na.locf(., na.rm = FALSE) 

y <- year(Sys.Date())
w <- week(Sys.Date())

df_fuel <- df_fuel %>%
  filter(!(year == y & week > w)) %>%
  distinct(year, week, .keep_all = TRUE)

rem_cols_fuel <- c("Belgium", "Italy", "France", "Greece", "Netherlands", "Spain")
rem_cols_number <- colnames(df_fuel)[str_detect(colnames(df_fuel), paste(rem_cols_fuel, collapse = "|"))]

super <- colnames(df_fuel)[str_detect(colnames(df_fuel), "super")]

df_fuel <- df_fuel %>%
  dplyr::select(-c(all_of(rem_cols_number), all_of(super)))


#### season ####
#%%%%%%%%%%%%%%#

# convert month name in number
df_season$season <- match(df_season$season, month.name)
df_season <- df_season %>%
  dplyr::rename(season_month = season)

# create indicator indicating season
df_season$season <- 1

# season: generate dummy variables for cultivation
df_season$storage <- 
  ifelse(str_detect(df_season$cultivation, 
                    "storage"), 1, 0)
df_season$openland <- 
  ifelse(str_detect(df_season$cultivation, 
                    "open field"), 1, 0)
df_season$greenhouse_heated <- 
  ifelse(str_detect(df_season$cultivation, 
                    "heated greenhouse"), 1, 0)
df_season$greenhouse_unheated <- 
  ifelse(str_detect(df_season$cultivation, 
                    "unheated greenhouse"), 1, 0)
df_season$film <- 
  ifelse(str_detect(df_season$cultivation, 
                    "under mat, film"), 1, 0)
df_season <- df_season %>% 
  mutate_at(vars(storage, openland, greenhouse_heated, greenhouse_unheated, film), 
            ~replace(., is.na(.), 0)) %>%
  dplyr::select(-cultivation)



#### footprint ####
#%%%%%%%%%%%%%%%%%%#

# fruit names need to start with capital letter followed by lower case letters
df_footprint$Item <- str_to_title(df_footprint$Item)

# replace Item values so that they match with the food values
df_footprint$Item <- mapvalues(
  df_footprint$Item,
  from = c("Pepper"),
  to = c("Bell Pepper")
)

# replace Food_Type values so that they match with the values from season
df_footprint$Food_Type <- mapvalues(
  df_footprint$Food_Type, 
  from = c("FRUIT HEATED GREENHOUSE", "FRUIT NOT HEATED GREENHOUSE* (melon)",
           "FRUIT OPENFIELD", "VEGETABLES HEATED GREENHOUSE", 
           "VEGETABLES NOT HEATED GREENHOUSE", "VEGETABLES OPENFIELD"),
  to = c("heated_greenhouse", "unheated_greenhouse", 
         "open_field", "heated_greenhouse", 
         "unheated_greenhouse",
         "open_field")
)

# spread data frame
df_footprint <- df_footprint %>%
  pivot_wider(
    names_from = Food_Type,
    values_from = c("Carbon_Footprint", "Water_Footprint")
  )


#### nutrition ####
#%%%%%%%%%%%%%%%%%#

# generate nutrition for bell pepper (not distinguished by column)
# mean nutrients over red, green and yellow
df_nutrition <- df_nutrition %>% 
  # select all food_types including Pepper, that is green, yellow and red pepper
  filter(str_detect(food_type, "Pepper")) %>%
  # group by nutrient and nutrient_unit...
  group_by(nutrient, nutrient_unit) %>%
  # ... to calculate mean over all three pepper types...
  dplyr::summarise(nutrient_value = mean(nutrient_value)) %>%
  # ... those are the nutrient values for overall pepper category
  mutate(food_type = "Bell Pepper") %>%
  # add original data frame
  rbind(df_nutrition)


# for energy we keep unit "KCAL"
df_nutrition <- df_nutrition[!(df_nutrition$nutrient == "Energy" & 
                               df_nutrition$nutrient_unit == "kJ"), ]

## Nutrition Recommendation

df_nutrition_rec <- read_excel("Data/nutrition_recommendation.xlsx") %>%
  dplyr::mutate(dplyr::across(-1, round, 2))

# save
saveRDS(df_nutrition_rec, "output/nutrition_recommendation.rds")

#### city distances ####
#%%%%%%%%%%%%%%%%%%%%%%#

# sometimes we have weather data for multiple cities in two countries.
# in this case, we take the mean in order to have one row per country
df_city_distances <- df_city_distances %>%
  # drop city variable since it is not necessary anymore
  dplyr::select(-City) %>%
  # all distance variables should be numeric
  mutate_at(vars(starts_with("distance_")), as.numeric) %>%
  # group by country
  group_by(Country) %>%
  # take mean of all distance variables across country
  summarise_at(vars(starts_with("distance_")), mean)

# for bananas, the origin is not indicated in the price data
# in the food price data frame, the country for bananas is 
# unknown. Since bananas typically come from Colombia, Ecuador, Panama and
# Costa Rica. We insert the mean of those countries here.
  ## first, we create this columns
df_city_distances_unknown <- data.frame(
  "Country" = "unknown"
)
df_city_distances_unknown <- cbind(
  df_city_distances_unknown, 
  df_city_distances %>% 
    filter(Country %in% c("Colombia", "Ecuador", "Panama", "Costa Rica")) %>%
    summarise_at(vars(starts_with("distance_")), mean)
)
  ## secondly, we add it
df_city_distances <- rbind(df_city_distances, df_city_distances_unknown)

# For Germany, we decided to set distances to 0 to not discriminate against
# markets. We do so because cultivation area is not totally clear
df_city_distances[df_city_distances$Country == "Germany", 
                  df_city_distances %>% 
                    dplyr::select(starts_with("distance_")) %>% colnames()] <- 0



#### weather ####
#%%%%%%%%%%%%%%%#

# Combining 3 weather data frames, the banana countries were added later in the project, so they had to be generated 
# separately to the weather information for all other fruits & vegetables

daily_weather_df <- 
  rbind(df_weather_2000_2014, df_weather_2015_j, df_weather_2000_2014_banana)

daily_weather_df$year <- year(daily_weather_df$datetime)

daily_weather_df$week <- strftime(daily_weather_df$datetime, format = "%V")

daily_weather_df$day <- day(daily_weather_df$datetime)

daily_weather_df$week <- as.factor(daily_weather_df$week)
daily_weather_df$location <- as.factor(daily_weather_df$location)
daily_weather_df$country <- as.factor(daily_weather_df$country)

# Later, when grouping by year and week, bias is induced, as e.g. the dates 01.02.2022 are year 2022 and week 52/52, when
# in reality they are the weeks 52/52 of the prevailing year

# Check how many and which dates are biased!

test <- daily_weather_df %>%  # 
  filter(day %in% c(1:7) & week %in% c(51:53)) 

# Create function for future multiple use. This function return the corrected years for those false dates. 

correct_false_years <- function(df) {
  test <- df %>%  # weekly_normal_df
    filter(day %in% c(1:7) & week %in% c(51:53)) %>%
    transmute(year = year - 1)
  return(test)
}

# Assign to vector

test <- correct_false_years(daily_weather_df)

# Actually change these years in the main data frame

daily_weather_df$year[daily_weather_df$day %in% c(1:7) & daily_weather_df$week %in% c(51:53)] <- test$year

## Ensures that we automatically get the needed years for the for-loop ##

# As I will do this calculation multiple times, a simple function will reduce space$ coding mistakes & ease code understanding 

generate_years <- function(df) {
s_years <- as.data.frame(unique(df$year))  # daily_weather_df$year
colnames(s_years)[1] <- "years"
s_years <- s_years$year[17:length(s_years$year)]
return(s_years)
}

s_years <- generate_years(daily_weather_df)

## Summary historical normals ##

weekly_normal_df <- daily_weather_df %>%
  filter(year > 1999 & year < 2015) %>%
  group_by(location, country, week, latitude, longitude, fruits, vegetables) %>%
  dplyr::summarise(n_temp = mean(temp, na.rm = T),
            #n_tempmax = mean(tempmax, na.rm = T),
            #n_tempmin = mean(tempmin, na.rm = T),
            #n_dew = mean(dew, na.rm = T),
            n_humidity = mean(humidity, na.rm = T),
            n_precip = mean(precip, na.rm = T),
            #n_precipcover = mean(precipcover, na.rm = T),
            n_windspeed = mean(windspeed, na.rm = T),
            #n_winddir = mean(winddir, na.rm = T),
            #n_mean_pressure = mean(pressure, na.rm = T),
            n_cloudcover = mean(cloudcover, na.rm = T)) %>%
  #n_visibility = mean(visibility, na.rm = T),
  #n_moonphase = mean(moonphase, na.rm = T)) %>%
  mutate(year = 2015) %>%
  dplyr::select(location, country, year, week:n_cloudcover)

for (i in s_years) {
  c <- daily_weather_df %>%
    filter(year > 1999 & year < i) %>% 
    group_by(location, country, week, latitude, longitude, fruits, vegetables) %>%
    dplyr::summarise(n_temp = mean(temp, na.rm = T),
              #n_tempmax = mean(tempmax, na.rm = T),
              #n_tempmin = mean(tempmin, na.rm = T),
              #n_dew = mean(dew, na.rm = T),
              n_humidity = mean(humidity, na.rm = T),
              n_precip = mean(precip, na.rm = T),
              #n_precipcover = mean(precipcover, na.rm = T),
              n_windspeed = mean(windspeed, na.rm = T),
              #n_winddir = mean(winddir, na.rm = T),
              #n_mean_pressure = mean(pressure, na.rm = T),
              n_cloudcover = mean(cloudcover, na.rm = T)) %>%
    #n_visibility = mean(visibility, na.rm = T),
    #n_moonphase = mean(moonphase, na.rm = T)) %>%
    mutate(year = i) %>%
    dplyr::select(location, country, year, week:n_cloudcover)
  
  weekly_normal_df <- rbind(weekly_normal_df, c)
  
}

## The banana countries Costa Rica, Equador, Columbia, panama have to be grouped to one unit

weekly_normal_df$location <- 
  revalue(weekly_normal_df$location, 
          c("guayaquil" = "unknown", "medellin" = "unknown", 
            "panama city" = "unknown", "costa rica" = "unknown"))

# All the weather variables will be averaged over the new 4 countries:

banana_countries_summary <- weekly_normal_df %>%
  filter(location == "unknown") %>%
  group_by(location, year, week) %>%
  dplyr::summarise(n_temp = mean(n_temp, na.rm = T),
            #n_tempmax = mean(tempmax, na.rm = T),
            #n_tempmin = mean(tempmin, na.rm = T),
            #n_dew = mean(dew, na.rm = T),
            n_humidity = mean(n_humidity, na.rm = T),
            n_precip = mean(n_precip, na.rm = T),
            #n_precipcover = mean(precipcover, na.rm = T),
            n_windspeed = mean(n_windspeed, na.rm = T),
            #n_winddir = mean(winddir, na.rm = T),
            #n_mean_pressure = mean(pressure, na.rm = T),
            n_cloudcover = mean(n_cloudcover, na.rm = T)) %>%
  mutate(country = "unknown",
         fruits = 1,
         vegetables = 0,
         longitude = NA,
         latitude = NA) %>%
  dplyr::select(location, country, year, week, latitude, longitude, 
         fruits, vegetables, n_temp:n_cloudcover)

weekly_normal_df <- weekly_normal_df %>%
  filter(location != "unknown")

weekly_normal_df <- rbind(weekly_normal_df, banana_countries_summary)

## Summary observed weather data ##

weekly_observed_w_data <- daily_weather_df %>%
  filter(year > 2014) %>%
  group_by(location, country, year, week, latitude, longitude, 
           fruits, vegetables) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T),
            #tempmax = mean(tempmax, na.rm = T),
            #tempmin = mean(tempmin, na.rm = T),
            #dew = mean(dew, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            precip = mean(precip, na.rm = T),
            #precipcover = mean(precipcover, na.rm = T),
            windspeed = mean(windspeed, na.rm = T),
            #winddir = mean(winddir, na.rm = T),
            #n_mean_pressure = mean(pressure, na.rm = T),
            cloudcover = mean(cloudcover, na.rm = T))
            #visibility = mean(visibility, na.rm = T),
            #moonphase = mean(moonphase, na.rm = T))

weekly_observed_w_data$location <- 
  revalue(weekly_observed_w_data$location, 
          c("guayaquil" = "unknown", "medellin" = "unknown", 
            "panama city" = "unknown", "costa rica" = "unknown"))

banana_countries_summary_obs <- weekly_observed_w_data %>%
  filter(location == "unknown") %>%
  group_by(location, year, week) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T),
            #n_tempmax = mean(tempmax, na.rm = T),
            #n_tempmin = mean(tempmin, na.rm = T),
            #n_dew = mean(dew, na.rm = T),
            humidity = mean(humidity, na.rm = T),
            precip = mean(precip, na.rm = T),
            #n_precipcover = mean(precipcover, na.rm = T),
            windspeed = mean(windspeed, na.rm = T),
            #n_winddir = mean(winddir, na.rm = T),
            #n_mean_pressure = mean(pressure, na.rm = T),
            cloudcover = mean(cloudcover, na.rm = T)) %>%
  mutate(country = "unknown",
         fruits = 1,
         vegetables = 0,
         longitude = NA,
         latitude = NA) %>%
  dplyr::select(location, country, year, week, latitude, longitude, 
                fruits, vegetables, temp:cloudcover)

weekly_observed_w_data <- weekly_observed_w_data %>%
  filter(location != "unknown")

weekly_observed_w_data <- rbind(weekly_observed_w_data, 
                                banana_countries_summary_obs)


# They were two missing values for weeks 03 and 04 for the cloud cover values, to avoid future problems in 
# the machine learning algorithms, the NA must be imputed with the cloud cover value for the prior week, in
# this case 02. This occurred two times for Stuttgart, one time for Cologne. 

summary(is.na(weekly_observed_w_data))  # Get overall view of Na's in data
summary(is.na(weekly_observed_w_data$cloudcover))

weekly_observed_w_data %>%  # If temperature is missing, every other weather variables is missing, too
  filter(is.na(temp))

cc_na_detection <- weekly_observed_w_data %>%  # Detect specific Na's in the "cloudcover" variable
  dplyr::select(location, country, year, week, cloudcover) %>%
  filter(is.na(cloudcover))

weekly_observed_w_data %>%  # 
  filter(location=="cologne" & year ==2021 & week=="52")  # Zoom in on country specific Na's

weekly_observed_w_data %>%
  filter(location=="huanuco" & year==2022 & week=="01")  # Huanuco seems to have data issues in the first week of the 2022 year

# Huanuco is missing all weather variables for that date

fill_cc_na <- weekly_observed_w_data %>%
  filter(location == "stuttgart" & year == "2021" & week == "02")  # Fill Na's in cloudcover with value of the week before

fill_cc_na_cologne <- weekly_observed_w_data %>%
  filter(location == "cologne" & year == "2021" & week == "51")

fill_cc_na_huanuco <- weekly_observed_w_data %>%
  filter(location == "stuttgart" & year == "2021" & week == "52")  # As huanuco is missing more than just the cloudcover variable,
# We must implement multiple implementation for this row 

impute_nn_cloudcover <- fill_cc_na$cloudcover

impute_nn_cloudcover_cologne <- fill_cc_na_cologne$cloudcover

weekly_observed_w_data[is.na(weekly_observed_w_data$cloudcover) & weekly_observed_w_data$location=="stuttgart",
                       "cloudcover"] <- impute_nn_cloudcover

weekly_observed_w_data[is.na(weekly_observed_w_data$cloudcover)& weekly_observed_w_data$location=="cologne",
                       "cloudcover"] <- impute_nn_cloudcover_cologne

# Specific case of huanuco

weekly_observed_w_data[weekly_observed_w_data$location=="huanuco" & weekly_observed_w_data$week=="01" &
                         weekly_observed_w_data$year=="2022",
                       c("temp", "humidity","precip", "windspeed", "cloudcover")] <- fill_cc_na_huanuco[, 8:12]

## Create summary data frame of observed historical data with historical normals ##

weather_df <- left_join(weekly_observed_w_data, weekly_normal_df, 
                        by = c("location", "country", "year", "week", "latitude", 
                               "longitude", "fruits", "vegetables"))

sum(is.na(weather_df))  # NA's due to missing longitude and latitude for bananas with unknown country

## Create deviations ##

weather_df <- weather_df %>%
  mutate(dev_temp = temp - n_temp,
         #dev_tempmax = tempmax - n_tempmax,
         #dev_tempmin = tempmin - n_tempmin,
         #dev_dew = dew - n_dew,
         dev_humidity = humidity - n_humidity,
         dev_precip = precip - n_precip,
         #dev_precipcover = precipcover - n_precipcover,
         dev_windspeed = windspeed - n_windspeed,
         #dev_winddir = winddir - n_winddir,
         #dev_pressure = mean_pressure - n_mean_pressure,
         dev_cloudcover = cloudcover - n_cloudcover)
         #dev_visibility = visibility -  n_visibility,
         #dev_moonphase = moonphase - n_moonphase)

## Create new variables e.g. frost ##

weather_df$frost <- ifelse(weather_df$temp <= 0, 1, 0)
weather_df$frost <- as.factor(weather_df$frost)

## Create lags for all relevant variables for machine learning ##

g <- weather_df[,9:ncol(weather_df)]
number_of_all_cols <- as.numeric(ncol(g))

lagged_variable_names <- as.data.frame(colnames(g)[1:ncol(g)])

colnames(lagged_variable_names)[1] <- "column_name"

not_needed_cols <- grep("n_", lagged_variable_names$column_name) 

number_of_not_needed_cols <- as.numeric(length(not_needed_cols))

g <- g %>%
  dplyr::select(-not_needed_cols)

f <- g

for (i in 1:ncol(g)) {
  for (j in 2:3) {
    g <- cbind(g, NA)
    names(g)[ncol(g)] <- paste0("lag_", j, "_weeks", "_", colnames(g)[i])
    g[,ncol(g)] <- Lag(as.vector(t(g[,i])), +j)
  }
}

for (i in 1:ncol(f)) {
  for (j in 1:12) {
    f <- cbind(f, NA)
    names(f)[ncol(f)] <- paste0("lag_", j, "_months", "_", colnames(f)[i])
    f[,ncol(f)] <- Lag(as.vector(t(f[,i])), +j*4)
  }
}

filtering_cols <- number_of_all_cols - number_of_not_needed_cols

g <- g[,-c(1:filtering_cols)]
f <- f[,-c(1:filtering_cols)]

df_weather <- cbind(weather_df, g, f)

# Remove some weather variables that are irrelevant, and lengthen the prediction task on the cluster #
# The weather variables are given with a lag of 2 weeks, 3 weeks, 1 month, 2 months,..., 12 months #

# We will assume that not every lag is relevant for the price prediction, therefore only variables with a 1 month, 3 months, 6 months,
# 9 months and 12 months will remain. This will substantially reduce the data frame size, but this trade-off still keeps relevant
# time lags

df_weather <- df_weather %>% 
  dplyr::select(!starts_with("lag_2_months") & !starts_with("lag_4_months") & 
                  !starts_with("lag_5_months") & !starts_with("lag_7_months") & 
                  !starts_with("lag_8_months") & !starts_with("lag_10_months") & 
                  !starts_with("lag_11_months") & !starts_with("lag_3_weeks"))

# keep only variables of interest (moved to a later point in file)
# df_weather <- df_weather %>%
#   dplyr::select(-c(location, latitude, longitude, temp, humidity, precip, windspeed,
#             cloudcover, n_temp, n_humidity, n_precip, n_windspeed, 
#             n_cloudcover, starts_with("dev_")))

# keep only data from January 2016
df_weather <- df_weather %>%
  filter(year >= 2016)

# all lead variables as numeric
df_weather <- df_weather %>%
  mutate_at(
    vars(starts_with("lead")), as.numeric
    )

# following variables as character
df_weather[, c("location", "country", "week", "year")] <- 
             sapply(df_weather[, c("location", "country", "week", "year")], 
                    as.character)

# for the merge, we create two data frames, one for fruits and one for vegetables
  ## data frame with weather information for fruits
df_weather_fruits <- df_weather %>%
  ungroup() %>%
  filter(fruits == 1) %>%
  dplyr::select(-vegetables)
  ## data frame with weather information for vegetables
df_weather_veggies <- df_weather %>%
  ungroup() %>%
  filter(vegetables == 1) %>%
  dplyr::select(-fruits) %>%
  dplyr::rename(veggies = vegetables)  # renamed for merge later

# check for NA's again
  ## in the fruits data frame are NA's since the longitude and latitude is
  ## missing for the bananas 
sum(is.na(df_weather_fruits))
  ## in the veggies data frame should no NA's be obtained
sum(is.na(df_weather_veggies))

#### Weather Forecast for ML prediction ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Prepare data from summarizing and group_by() options

# In order to calculate the dev_ variables, that calculate the deviation of e.g. weather from historical normal weather,
# all weather data frames must be combined, but filtered, so that the dates after 01-01-2022 are not the real weather data,
# but the forecast weather date

historical_dev <- 
  rbind(df_weather_2000_2014, df_weather_2015_j, df_weather_2000_2014_banana)

historical_dev <- historical_dev %>%
  dplyr::filter(datetime <= "2021-12-31") %>%
  dplyr::select(-stations)  # stations are not available in the real observed weather data frame, only in the forecast weather

# This is the new data frame with real observed weather data and forecast weather from the 01.01.2022 up to the current time

historical_dev <- rbind(historical_dev, df_weather_forecast)  # Create data frame for summarizing historical normals

historical_dev$year <- year(historical_dev$datetime)

historical_dev$week <- strftime(historical_dev$datetime, format = "%V")

historical_dev$day <- day(historical_dev$datetime)

historical_dev$week <- as.factor(historical_dev$week)
historical_dev$location <- as.factor(historical_dev$location)
historical_dev$country <- as.factor(historical_dev$country)

# Because we will later summarize by year and week, some dates must be changed. The date 01-01-2022 is the 52 week in the year 2022
# This is technically correct, but by summarizing by both year and week, this can create data that is non-existing

test <- correct_false_years(historical_dev)

# Change year to correct year

historical_dev$year[historical_dev$day %in% c(1:7) & historical_dev$week %in% c(51:53)] <- test$year

#historical_dev$year[historical_dev$year == 2022 &  ]

## Ensures that we automatically get the needed years for the for-loop ##

s_years <- generate_years(historical_dev)

## Summary historical normals ##

# The following dplyr pipeline & for-loop will summarize all the historical values for each weather variable
# of the last years. All weather variables that start in e.g. 2016 will get the average historical weather from the years 
# 2000 - 2016. Each year after 2016 gets more and more years for the average historical weather data. All crucial weather
# variables e.g. temperature, humidity, precipitation, windespeed, cloudcover are summarized by their respective mean

# Calculate mean temperature across every city-country-week combination 

weekly_normal_df_f <- historical_dev %>%
  dplyr::filter(year > 1999 & year < 2015) %>%
  dplyr::group_by(location, country, week, latitude, longitude, fruits, vegetables) %>%
  dplyr::summarise(n_temp = mean(temp, na.rm = T),
                   #n_tempmax = mean(tempmax, na.rm = T),
                   #n_tempmin = mean(tempmin, na.rm = T),
                   #n_dew = mean(dew, na.rm = T),
                   n_humidity = mean(humidity, na.rm = T),
                   n_precip = mean(precip, na.rm = T),
                   #n_precipcover = mean(precipcover, na.rm = T),
                   n_windspeed = mean(windspeed, na.rm = T),
                   #n_winddir = mean(winddir, na.rm = T),
                   #n_mean_pressure = mean(pressure, na.rm = T),
                   n_cloudcover = mean(cloudcover, na.rm = T)) %>%
  #n_visibility = mean(visibility, na.rm = T),
  #n_moonphase = mean(moonphase, na.rm = T)) %>%
  dplyr::mutate(year = 2015) %>%
  dplyr::select(location, country, year, week:n_cloudcover)

for (i in s_years) {
  c <- historical_dev %>%
    dplyr::filter(year > 1999 & year < i) %>% 
    dplyr::group_by(location, country, week, latitude, longitude, fruits, vegetables) %>%
    dplyr::summarise(n_temp = mean(temp, na.rm = T),
                     #n_tempmax = mean(tempmax, na.rm = T),
                     #n_tempmin = mean(tempmin, na.rm = T),
                     #n_dew = mean(dew, na.rm = T),
                     n_humidity = mean(humidity, na.rm = T),
                     n_precip = mean(precip, na.rm = T),
                     #n_precipcover = mean(precipcover, na.rm = T),
                     n_windspeed = mean(windspeed, na.rm = T),
                     #n_winddir = mean(winddir, na.rm = T),
                     #n_mean_pressure = mean(pressure, na.rm = T),
                     n_cloudcover = mean(cloudcover, na.rm = T)) %>%
    #n_visibility = mean(visibility, na.rm = T),
    #n_moonphase = mean(moonphase, na.rm = T)) %>%
    dplyr::mutate(year = i) %>%
    dplyr::select(location, country, year, week:n_cloudcover)
  
  weekly_normal_df_f <- rbind(weekly_normal_df_f, c)
  
}

## The banana countries Costa Rica, Equador, Columbia, panama have to be grouped to one unit

weekly_normal_df_f$location <- 
  revalue(weekly_normal_df_f$location, 
          c("guayaquil" = "unknown", "medellin" = "unknown", 
            "panama city" = "unknown", "costa rica" = "unknown"))

# All the weather variables will be averaged over the new 4 countries:

banana_countries_summary_f <- weekly_normal_df_f %>%
  dplyr::filter(location == "unknown") %>%
  dplyr::group_by(location, year, week) %>%
  dplyr::summarise(n_temp = mean(n_temp, na.rm = T),
                   #n_tempmax = mean(tempmax, na.rm = T),
                   #n_tempmin = mean(tempmin, na.rm = T),
                   #n_dew = mean(dew, na.rm = T),
                   n_humidity = mean(n_humidity, na.rm = T),
                   n_precip = mean(n_precip, na.rm = T),
                   #n_precipcover = mean(precipcover, na.rm = T),
                   n_windspeed = mean(n_windspeed, na.rm = T),
                   #n_winddir = mean(winddir, na.rm = T),
                   #n_mean_pressure = mean(pressure, na.rm = T),
                   n_cloudcover = mean(n_cloudcover, na.rm = T)) %>%
  dplyr::mutate(country = "unknown",
         fruits = 1,
         vegetables = 0,
         longitude = NA,
         latitude = NA) %>%
  dplyr::select(location, country, year, week, latitude, longitude, 
                fruits, vegetables, n_temp:n_cloudcover)

weekly_normal_df_f <- weekly_normal_df_f %>%
  dplyr::filter(location != "unknown")

weekly_normal_df_f <- rbind(weekly_normal_df_f, banana_countries_summary_f)


## Summary observed weather data ##

weekly_observed_w_data_f <- historical_dev %>%
  dplyr::filter(year > 2014) %>%
  dplyr::group_by(location, country, year, week, latitude, longitude, 
           fruits, vegetables) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T),
                   #tempmax = mean(tempmax, na.rm = T),
                   #tempmin = mean(tempmin, na.rm = T),
                   #dew = mean(dew, na.rm = T),
                   humidity = mean(humidity, na.rm = T),
                   precip = mean(precip, na.rm = T),
                   #precipcover = mean(precipcover, na.rm = T),
                   windspeed = mean(windspeed, na.rm = T),
                   #winddir = mean(winddir, na.rm = T),
                   #n_mean_pressure = mean(pressure, na.rm = T),
                   cloudcover = mean(cloudcover, na.rm = T))
#visibility = mean(visibility, na.rm = T),
#moonphase = mean(moonphase, na.rm = T))

weekly_observed_w_data_f$location <- 
  revalue(weekly_observed_w_data_f$location, 
          c("guayaquil" = "unknown", "medellin" = "unknown", 
            "panama city" = "unknown", "costa rica" = "unknown"))

banana_countries_summary_obs_f <- weekly_observed_w_data_f %>%
  dplyr::filter(location == "unknown") %>%
  dplyr::group_by(location, year, week) %>%
  dplyr::summarise(temp = mean(temp, na.rm = T),
                   #n_tempmax = mean(tempmax, na.rm = T),
                   #n_tempmin = mean(tempmin, na.rm = T),
                   #n_dew = mean(dew, na.rm = T),
                   humidity = mean(humidity, na.rm = T),
                   precip = mean(precip, na.rm = T),
                   #n_precipcover = mean(precipcover, na.rm = T),
                   windspeed = mean(windspeed, na.rm = T),
                   #n_winddir = mean(winddir, na.rm = T),
                   #n_mean_pressure = mean(pressure, na.rm = T),
                   cloudcover = mean(cloudcover, na.rm = T)) %>%
  dplyr::mutate(country = "unknown",
         fruits = 1,
         vegetables = 0,
         longitude = NA,
         latitude = NA) %>%
  dplyr::select(location, country, year, week, latitude, longitude, 
                fruits, vegetables, temp:cloudcover)

weekly_observed_w_data_f <- weekly_observed_w_data_f %>%
  filter(location != "unknown")

weekly_observed_w_data_f <- rbind(weekly_observed_w_data_f, 
                                banana_countries_summary_obs_f)


# They were two missing values for weeks 03 and 04, to avoid future problems in 
# the machine learning algorithms, the NA must be inputed
# with the cloud cover value for the prior week, in this case 02

summary(is.na(weekly_observed_w_data_f))
summary(is.na(weekly_observed_w_data_f$cloudcover))

cc_na_detection_f <- weekly_observed_w_data_f %>%
  dplyr::select(location, country, year, week, cloudcover) %>%
  dplyr::filter(is.na(cloudcover))

fill_cc_na_f <- weekly_observed_w_data_f %>%
  dplyr::filter(location == "stuttgart" & year == "2021" & week == "02")

impute_nn_cloudcover_f <- fill_cc_na_f$cloudcover
weekly_observed_w_data_f[is.na(weekly_observed_w_data_f$cloudcover), "cloudcover"] <- impute_nn_cloudcover_f

## Create summary data frame of observed historical data with historical normals ##

weather_df_f <- dplyr::left_join(weekly_observed_w_data_f, weekly_normal_df_f, 
                        by = c("location", "country", "year", "week", "latitude", 
                               "longitude", "fruits", "vegetables"))

sum(is.na(weather_df_f))

## Create deviations ##

weather_df_f <- weather_df_f %>%
  dplyr::mutate(dev_temp = temp - n_temp,
         #dev_tempmax = tempmax - n_tempmax,
         #dev_tempmin = tempmin - n_tempmin,
         #dev_dew = dew - n_dew,
         dev_humidity = humidity - n_humidity,
         dev_precip = precip - n_precip,
         #dev_precipcover = precipcover - n_precipcover,
         dev_windspeed = windspeed - n_windspeed,
         #dev_winddir = winddir - n_winddir,
         #dev_pressure = mean_pressure - n_mean_pressure,
         dev_cloudcover = cloudcover - n_cloudcover)
#dev_visibility = visibility -  n_visibility,
#dev_moonphase = moonphase - n_moonphase)

## Create new variables e.g. frost ##

weather_df_f$frost <- ifelse(weather_df_f$temp <= 0, 1, 0)
weather_df_f$frost <- as.factor(weather_df_f$frost)

## Create lags for all relevant variables for machine learning ##

g_f <- weather_df_f[,9:ncol(weather_df_f)]
number_of_all_cols_f <- as.numeric(ncol(g_f))

lagged_variable_names_f <- as.data.frame(colnames(g_f)[1:ncol(g_f)])

colnames(lagged_variable_names_f)[1] <- "column_name"

not_needed_cols_f <- grep("n_", lagged_variable_names_f$column_name) 

number_of_not_needed_cols_f <- as.numeric(length(not_needed_cols_f))

g_f <- g_f %>%
  dplyr::select(-not_needed_cols_f)

f_f <- g_f

for (i in 1:ncol(g_f)) {
  for (j in 2:3) {
    g_f <- cbind(g_f, NA)
    names(g_f)[ncol(g_f)] <- paste0("lag_", j, "_weeks", "_", colnames(g_f)[i])
    g_f[,ncol(g_f)] <- Lag(as.vector(t(g_f[,i])), +j)
  }
}

for (i in 1:ncol(f_f)) {
  for (j in 1:12) {
    f_f <- cbind(f_f, NA)
    names(f_f)[ncol(f_f)] <- paste0("lag_", j, "_months", "_", colnames(f_f)[i])
    f_f[,ncol(f_f)] <- Lag(as.vector(t(f_f[,i])), +j*4)
  }
}

filtering_cols_f <- number_of_all_cols_f - number_of_not_needed_cols_f

g_f <- g_f[,-c(1:filtering_cols_f)]
f_f <- f_f[,-c(1:filtering_cols_f)]

df_weather_f <- cbind(weather_df_f, g_f, f_f)

# Remove some weather variables that are irrelevant, and lengthen the prediction task on the cluster #
# The weather variables are given with a lag of 2 weeks, 3 weeks, 1 month, 2 months,..., 12 months #

# We will assume that not every lag is relevant for the price prediction, therefore only variables with a 1 month, 3 months, 6 months,
# 9 months and 12 months will remain. This will substantially reduce the data frame size, but this trade-off still keeps relevant
# time lags

df_weather_f <- df_weather_f %>% 
  dplyr::select(!starts_with("lag_2_months") & !starts_with("lag_4_months") & 
                  !starts_with("lag_5_months") & !starts_with("lag_7_months") & 
                  !starts_with("lag_8_months") & !starts_with("lag_10_months") & 
                  !starts_with("lag_11_months") & !starts_with("lag_3_weeks"))

# keep only variables of interest (moved to a later point in file)
# df_weather <- df_weather %>%
#   dplyr::select(-c(location, latitude, longitude, temp, humidity, precip, windspeed,
#             cloudcover, n_temp, n_humidity, n_precip, n_windspeed, 
#             n_cloudcover, starts_with("dev_")))


# Kick variables and bring to same for as observed weather data frame

df_weather_f <- df_weather_f %>%
  dplyr::filter(year >= 2022)  # Here we only need data starting in year 2022 as testing data for our ML algorithms

# all lead variables as numeric
df_weather_f <- df_weather_f %>%
  dplyr::mutate_at(
    vars(starts_with("lead")), as.numeric
  )

# following variables as character
df_weather_f[, c("location", "country", "week", "year")] <- 
  sapply(df_weather_f[, c("location", "country", "week", "year")], 
         as.character)

# Export df_weather_f as csv for prediction

fwrite(df_weather_f, 'output/weather_forecast.csv')

# for the merge, we create two data frames, one for fruits and one for vegetables
## data frame with weather information for fruits

# These are two new data frames, whereby "_f" stands for the weather forecast data. The structure and variables names identical to the 
# weather data frame

df_weather_fruits_f <- df_weather_f %>%
  ungroup() %>%
  dplyr::filter(fruits == 1) %>%
  dplyr::select(-vegetables)
## data frame with weather information for vegetables
df_weather_veggies_f <- df_weather_f %>%
  ungroup() %>%
  filter(vegetables == 1) %>%
  dplyr::select(-fruits) %>%
  dplyr::rename(veggies = vegetables)  # renamed for merge later

# check for NA's again
## in the fruits data frame are NA's since the longitude and latitude is
## missing for the bananas 
sum(is.na(df_weather_fruits_f))
## in the veggies data frame should no NA's be obtained
sum(is.na(df_weather_veggies_f))


#### World Container Index ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# change format or rather data type of week and year
df_world_container_index$year <- as.character(df_world_container_index$year)
df_world_container_index$week <- 
  str_pad(df_world_container_index$week, 2, side = "left", pad = "0")

# The container index denotes the freight rate in US $ per 40ft container. As a consequence, this must be transformed to 
# the price per kg to have comparability and interchangeability of the predictors.

# The website "icontainers.com" (https://www.icontainers.com/help/40-foot-container/) states that the empty weight of a 40ft container
# is 3 750 kg and the maximum is 29 tons. However, the amount of cargo depends on the rules/limitations of the cargo line.

# Because we cannot know the exact amount of cargo, a value of 27600kg per 40-foot-container is used. This is also stated on multiple
# other websites as the go to cargo weight.

# We transform so that the container index price is measured per kg and not the 40ft container:

df_world_container_index <- df_world_container_index %>%
  mutate(wci = wci/27600,
         wci_2_weeks = wci_2_weeks/27600,
         wci_1_month = wci_1_month/27600)


#%%%%%%%%%%%%%# 
#### Merge ####
#%%%%%%%%%%%%%#

#### Prediction Data Frame ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## Add Season to Food Prices ## 

# store original df_season
df_seaons_original <- df_season

# we can do the merge except for bell pepper since in season we
# distinguish between red and green bell pepper
# hence to merge bell pepper we create separate data frames
df_food_price_continuous_pepper <- df_food_price_continuous %>%
  filter(food == "Bell Pepper")
df_food_price_continuous <- df_food_price_continuous %>%
  filter(food != "Bell Pepper")

df_season_pepper <- df_season %>%
  # keep only observations with bell pepper
  filter(str_detect(food, "Bell Pepper")) %>%
  # generate type column
  mutate(
    type = if_else(
      food == "Green Bell Pepper", "green",
      if_else(
        food == "Red Bell Pepper", "red", "unknown"
      )
    )
  ) %>%
  # generate food variable containing "Bell Pepper"
  # necessary for merge
  mutate(
    food = "Bell Pepper"
  )

# generate season data frame without bell pepper
df_season <- df_season %>%
  filter(!str_detect(food, "Bell Pepper"))

# merge without bell pepper
df_food_price_season <- left_join(
  df_food_price_continuous, 
  df_season,
  by = c("month" = "season_month", "country", "food"))

# merge with bell pepper
  ## for "unknown" type of bell pepper, we merge simply via food and country
country_unknown <- df_season_pepper %>% filter(type == "unknown") %>% 
  dplyr::select(-type) %>% dplyr::select(country) %>% distinct() %>% pull()

df_food_price_season_pepper_1 <- left_join(
  df_food_price_continuous_pepper %>% filter(country %in% country_unknown),
  df_season_pepper %>% filter(type == "unknown") %>% dplyr::select(-type),
  by = c("month" = "season_month", "country", "food")
)
  ## for known type of bell pepper, we also use the type column for the merge
country_known <- df_season_pepper %>% filter(type != "unknown") %>% 
  dplyr::select(-type) %>% dplyr::select(country) %>% distinct() %>% pull()

df_food_price_season_pepper_2 <- left_join(
  df_food_price_continuous_pepper %>% filter(country %in% country_known),
  df_season_pepper %>% filter(type != "unknown"),
  by = c("month" = "season_month", "country", "food", "type")
)
  ## combine both data frame
df_food_price_season_pepper <- 
  rbind(df_food_price_season_pepper_1, df_food_price_season_pepper_2)

# combine pepper and non-pepper data frame
df_food_price_season <- rbind(df_food_price_season, 
                              df_food_price_season_pepper)

# NA's mean that fruit is not in season; same holds for cultivation
df_food_price_season <- df_food_price_season %>% 
  mutate_at(vars(season, storage, openland, greenhouse_heated, 
                 greenhouse_unheated, film), ~replace(., is.na(.), 0)) 


## Add cultivation area distances ##
df_food_price_season <- left_join(
  df_food_price_season, df_city_distances, by = c("country" = "Country")
)
# year as character
df_food_price_season$year <- as.character(df_food_price_season$year)


## Add weather ##

# ensure that fruit and veggy indicators are correct
fruits <- c("Apple", "Peach", "Pear", "Grapes", "Strawberry", "Kiwi", "Orange", "Clementinen",
            "Mandarin", "Lemon", "Banana", "Satsumas", "Plum", "Nectarine", "Apricot", "Cherry")
veggies <- c("Artichoke", "Eggplant", "Cauliflower", "Carrot", "Beans", "Iceberg Lettuce", 
             "Endive", "Cucumber", "Tomato", "Bell Pepper", "Leek", "Onion", "Zucchini",
             "Potatoes", "Lettuce", "Asparagus", "Brussel Sprouts")
df_food_price_season$fruits <- ifelse(df_food_price_season$food %in% fruits, 1, 0)
df_food_price_season$veggies <- ifelse(df_food_price_season$food %in% veggies, 1, 0)

# add weather for fruits
df_food_price_season_fruits <- df_food_price_season %>%
  filter(fruits == 1)
df_food_price_season_weather_1 <- left_join(
  df_food_price_season_fruits, 
  df_weather_fruits %>% dplyr::select(-c(location, latitude, longitude)),
  by = c("year", "week", "country", "fruits")
)

# add weather for vegetables
df_food_price_season_veggies <- df_food_price_season %>%
  filter(veggies == 1)
df_food_price_season_weather_2 <- left_join(
  df_food_price_season_veggies, 
  df_weather_veggies %>% dplyr::select(-c(location, latitude, longitude)),
  by = c("year", "week", "country", "veggies")
)

# combine both
df_food_price_season_weather <- rbind(
  df_food_price_season_weather_1, df_food_price_season_weather_2
)
sum(is.na(df_food_price_season_weather))


## Merge Oil, Fuel, Exchange Rate, Kerosene, Inflation and World Contained Index ##

# inflation + exchange rates are merged via week and year
# since we have dummy variables for each country included
df_transport <- full_join(df_inflation, df_exchange_rates,
                      by = c("week", "year"))

# kerosene are merged also via week and year since kerosene are world prices
df_transport <- full_join(df_transport, df_kerosene, 
                      by = c("week", "year"))

# crude oil price is also merged via week and year
df_transport <- full_join(df_transport, df_oil, 
                      by = c("week", "year"))

# fuel
df_transport <- full_join(df_transport, df_fuel, 
                          by = c("week", "year"))

# world contained index
df_transport <- full_join(df_transport, df_world_container_index,
                          by = c("week", "year"))


## Merge to food prices via year, week and country ##

# we use a left join to add only transport information
# which is also included in the price data
df_pre_final <- left_join(df_food_price_season_weather, df_transport,
                           by = c("week", "year"))

## Merge Fertilizer Prices ##

# merge via week and year
df_pre_final <- left_join(df_pre_final, df_fertilizer,
                           by = c("week", "year"))


# include size mapping
food_sizes <- read_excel("data/food_sizes_mapping.xlsx")
df_pre_final <- left_join(
  df_pre_final, food_sizes, by = c("food", "size")
) %>%
  dplyr::select(-c(size)) %>%
  dplyr::rename(size = size_descr)


# drop weather variables
df_pred_final <- df_pre_final %>%
  dplyr::select(-c(temp, humidity, precip, windspeed,
            cloudcover, n_temp, n_humidity, n_precip, n_windspeed,
            n_cloudcover, starts_with("dev_")))


# Change names of food types Cucumber to Normal and Erstmarke to First Brand
df_pred_final <- df_pred_final %>%
  dplyr::mutate(type = dplyr::recode(type, Cucumber = "normal", 
                                     Erstmarke = "First Brand"))

# Now all to types and countries to uppercase
df_pred_final$type <- str_to_title(df_pred_final$type)
df_pred_final$country <- str_to_title(df_pred_final$country)


## Save this version ##
sum(is.na(df_pred_final)) # last check with missings
saveRDS(df_pred_final, "output/df_descriptives.rds")


## Prepare Final Data Frame ##

# create dummy variables for food, type, country, size, year, calender week and month
df_pred_final[, c("food", "type", "country", "size", "year", "week", "month")] <- 
  lapply(df_pred_final[, c("food", "type", "country", 
                           "size", "year", "week", "month")], as.factor)

df_pred_final <- dummy_cols(
  df_pred_final,
  remove_first_dummy = FALSE,
  remove_selected_columns = FALSE, # keep them to identify later
  select_columns = c("food", "type", "country", "size", "year", "week", "month")
)


# correct data types
# sapply(df_pred_final, class)
df_pred_final <- df_pred_final %>% 
  mutate_at(vars(starts_with("inflation_perc")), 
            list(~ as.numeric(.)))

# check if no missing values are included
sum(is.na(df_pred_final))

# drop variables not needed for prediction 
df_pred_final <- df_pred_final %>%
  dplyr::select(
    -c(year, week, month, flag, Kerosene_Price_lag_two_week, 
       Crude_Oil_Price_lag_two_month, Crude_Oil_Price_lag_three_month,
       exchange_rate_lag_two_week_New_Zealand, exchange_rate_lag_two_week_South_Africa,
       exchange_rate_lag_two_week_Turkey, frost, Price_Index,
       fuel_price_diesel_lag_two_week_Germany, fuel_price_diesel_lag_three_week_Germany,
       inflation_perc_lag_two_month_Germany, inflation_perc_lag_three_month_Germany)
  )

# change data types
  ## all variables including frost are changed to numeric
df_pred_final <- df_pred_final %>% 
  #mutate_each(funs(as.numeric), ends_with("frost"))
  mutate_at(vars(ends_with("frost")), as.numeric)

# check data types
  ## the four factor variables are: food, type, country, size
  ## kept for prediction analysis
table(sapply(df_pred_final, class))

# create non-linear terms for LASSO


saveRDS(df_pred_final, "output/final_ML.rds")
#saveRDS(df_pred_final, "output/final_ML_regression.rds")


# create second data frame for prediction models 
# this time: all models will be tuned for each food item 
df_pred_final_food <- df_pred_final  #read_rds("output/final_ML.rds")

food_cols <- colnames(df_pred_final_food)[str_detect(colnames(df_pred_final_food), "food_")]
country_cols <- colnames(df_pred_final_food)[str_detect(colnames(df_pred_final_food), "country_")]
type_cols <- colnames(df_pred_final_food)[str_detect(colnames(df_pred_final_food), "type_")] 
size_cols <- colnames(df_pred_final_food)[str_detect(colnames(df_pred_final_food), "size_")] 

drop_cols <- c(food_cols, size_cols, type_cols, country_cols)

# Remove dummy variables indicating specific food items
df_pred_final_food <- df_pred_final_food %>%
  dplyr::select(-c(all_of(drop_cols), fruits, veggies))

saveRDS(df_pred_final_food, "output/final_food_ML.rds")


#### Food Basket Data Frame ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# merge season and footprint
  ## footprint does not distinguish between color of bell pepper
  ## we add a second column to season in which we also do not distinguish
  ## between the color of bell pepper
df_seaons_original$food_2 <- df_seaons_original$food
df_seaons_original$food_2 <- str_replace(
  df_seaons_original$food_2, ".*Pepper$", "Bell Pepper"
)
df_basket <- full_join(df_seaons_original, df_footprint,
                       by = c("food_2" = "Item"))

# add nutrition
  ## nutrition distinguishes between green, red and yellow bell pepper
  ## in season, however, bell pepper is only distinguished by red and green
sort(unique(df_basket$food))  
sort(unique(df_nutrition$food_type))
df_basket <- full_join(df_basket, df_nutrition, 
                       by = c("food" = "food_type"))


# add km distance to cultivation area
df_basket <- left_join(df_basket, df_city_distances, 
                       by = c("country" = "Country"))

# keep only food which are also in the price data frame
food_keep <- unique(df_food_price$food)
df_basket <- df_basket %>% filter(food_2 %in% food_keep)

# save data frame
saveRDS(df_basket, "output/final_basket.rds")



#### Cluster Data Frame ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%#

# For k-means Clustering, we also need a specific data frame
# In the R Shiny App we allow the user to perform a clustering on
# nutrition values and footprint 

## Nutrient ##

# generate data frame
df_cluster_nutrient <- df_basket %>%
  dplyr::select(food, nutrient, nutrient_value) %>%
  distinct()

# check for missing values
sum(is.na(df_cluster_nutrient))

# change structure of data frame
df_cluster_nutrient <- df_cluster_nutrient %>% 
  pivot_wider(names_from = nutrient, values_from = nutrient_value)

# drop columns with missing values since kmeans() cannot handle missings
df_cluster_nutrient <- 
  df_cluster_nutrient[, colSums(is.na(df_cluster_nutrient)) == 0]

# drop cholestorol which is 0 everywhere
df_cluster_nutrient <- df_cluster_nutrient %>% dplyr::select(-Cholesterol)


## Footprint ##

# generate data frame
df_cluster_footprint <- 
  df_basket %>%
  dplyr::select(food, starts_with("Water_"), starts_with("Carbon_")) %>%
  distinct()

# drop columns containing at least one missing value
  ## only open field is kept
df_cluster_footprint <- df_cluster_footprint[, colSums(is.na(df_cluster_footprint)) == 0]



## Price ##

# mean prices are used
df_cluster_price <- df_pred_final %>%
  group_by(food) %>%
  dplyr::summarise(
    mean_price_total = mean(price_total),
    mean_price_b = mean(price_B),
    mean_price_f = mean(price_F),
    mean_price_h = mean(price_H),
    mean_price_k = mean(price_K),
    mean_price_m = mean(price_M)
    )


## Combine all three ##
df_cluster <- inner_join(
  df_cluster_nutrient, df_cluster_footprint, by = "food"
)

df_cluster <- inner_join(
  df_cluster, df_cluster_price, by = "food"
)

# rename columns
df_cluster <- df_cluster %>%
  dplyr::rename(
    "Fat" = "Total lipid (fat)",
    "Carbohydrate" = "Carbohydrate, by difference", # Kohlenhydrate
    "Sugar" = "Sugars, total including NLEA",
    "Fiber" = "Fiber, total dietary", # Balaststoffe
    "Calcium" = "Calcium, Ca",
    "Iron" = "Iron, Fe", # Eisen
    "Potassium" = "Potassium, K", # Kalium
    "Sodium" = "Sodium, Na", # Natrium
    "Vitamin C" = "Vitamin C, total ascorbic acid",
    "Water Footprint" = "Water_Footprint_open_field",
    "Carbon Footprint" = "Carbon_Footprint_open_field",
    "Mean Price Total" = "mean_price_total",
    "Mean Price Berlin" = "mean_price_b",
    "Mean Price Frankfurt" = "mean_price_f",
    "Mean Price Hamburg" = "mean_price_h",
    "Mean Price Cologne" = "mean_price_k",
    "Mean Price Munich" = "mean_price_m"
  )

# ungroup data frame
df_cluster <- df_cluster %>% ungroup()

# save data frame
saveRDS(df_cluster, "output/final_cluster.rds")



#### Data Sets for Test Data of Prediction ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# extract variables which are lagged
# we need them for the lagged variables in the test data set for the prediction
df_prediction_test_data <- df_pre_final %>%
  dplyr::select(date, food, type, country, size,
         temp, humidity, precip, windspeed, cloudcover, frost,
         frost, starts_with("dev_"), exchange_rate_New_Zealand,
         exchange_rate_South_Africa, exchange_rate_Turkey,
         inflation_perc_Germany, DAP, Urea, TSP, Phosphate_Rock, Potassium_chloride, 
         wci, fuel_price_diesel_Germany, Kerosene_Price, 
         season, storage, openland, greenhouse_heated, greenhouse_unheated, 
         film, Crude_Oil_Price)

# same adjustments with small and capital letters for title
# Change names of food types Cucumber to Normal and Erstmarke to First Brand
df_prediction_test_data <- df_prediction_test_data %>%
  dplyr::mutate(type = dplyr::recode(type, Cucumber = "normal", 
                                     Erstmarke = "First Brand"))

# Now all to types and countries to uppercase
df_prediction_test_data$type <- str_to_title(df_prediction_test_data$type)
df_prediction_test_data$country <- str_to_title(df_prediction_test_data$country)


saveRDS(df_prediction_test_data, "output/prep_test_data.rds")
  
