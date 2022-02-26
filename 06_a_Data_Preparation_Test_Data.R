#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## Make Predictions in Shiny App ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## by Lana Kern ##

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# In this file, the test data set is created which is used to make predictions
# for weeks one to four.
# To create this data set, we assume that "todays'" date is the 17th January, 2022.
# Hence in the Shiny App predictions can be made for:
# Week 24th January, 2022.
# Week 31th January, 2022.
# Week 7th February, 2022.
# Week 14th February, 2022.
# Assuming that we make predictions from today (17th Januray) we do not know
# some features in the future, e.g. exchange rate, inflation rate, weather.
# For all features that are unknown, the previous value is used. However,
# for weather we use the predicted weather.
# Other features like week and month can be simply generated for future values.
# For the lagged price variables, the predictions from the previous week are
# used. 

# Note that this file can be continuously updated by simply replacing todays_date
# with the current date. 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


#### Load Packages ####

# clear workspace
rm(list = ls())

# load packages
  ## data.table() and lubridate() is used to work with dates
if (!require("data.table")) install.packages("data.table") 
library(data.table)
if (!require("lubridate")) install.packages("lubridate") 
library(lubridate)
  ## dplyr() is used to work with data manipulation
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr)
  ## to generate dummy variables
if (!require("fastDummies")) install.packages("fastDummies") 
library(fastDummies)
  ## for string manipulations
if (!require("stringr")) install.packages("stringr") 
library(stringr)


#### Create Test Data ####

# load data set which was used to train the models (from file 03)
df_ML_raw <- readRDS("output/final_food_ML.rds")

# all integer columns as numeric
df_ML_raw <- df_ML_raw %>% mutate_if(is.integer, as.numeric)

# all factor columns as character
df_ML_raw <- df_ML_raw %>% mutate_if(is.factor, as.character)

# load data set with present predictors (from file 03)
df_predictors <- readRDS("output/prep_test_data.rds")

# keep newest data row
  ## this row is identified by latest date
df_ML_current_date <-
  df_ML_raw %>%
  arrange(date) %>%
  tail(1) %>%
  dplyr::select(date, starts_with("week_"), starts_with("year_"), 
         starts_with("month_"))

# for the latest date, keep all food-type-size-country combinations
# needed later for merging current variables
df_ML_current_var <-
  df_ML_raw %>%
  filter(date == df_ML_current_date$date)

# extract "today's" date
todays_date <- df_ML_current_date$date

# generate next weeks dates
week_1_date <- todays_date + lubridate::weeks(1)
week_2_date <- todays_date + lubridate::weeks(2)
week_3_date <- todays_date + lubridate::weeks(3)
week_4_date <- todays_date + lubridate::weeks(4)


# df_ML_current_date$year_2022
# df_ML_current_date$month_1
# df_ML_current_date$week_03

# according to the latest training data, the test set is created #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

test_data <- data.frame()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### test data: week, year, month ####
#%%

# generate test data for year, month, and week dummies (they change)
df_date <- data.frame()
for (i in 1:4) {
  # generate vector with date in i weeks
  week_date <- todays_date + lubridate::weeks(i)
  # extract year, month, and calender week 
  week_year <- data.table::year(week_date) # extract year
  week_month <- data.table::month(week_date) # extract month
  week_week <- strftime(week_date, format = "%V") # extract calender week
  # replace all dummys regarding year, month, and week as 0
  df_date_sub <-
    df_ML_current_date %>% 
    dplyr::select(matches("^year_|^month_|^week_")) %>%
    replace(. == 1, 0) 
  # adjust dummies for next week
  df_date_sub[, paste0("year_", week_year)] <- 1
  df_date_sub[, paste0("month_", week_month)] <- 1
  df_date_sub[, paste0("week_", week_week)] <- 1
  # add date as it simplifies upcoming calculations
  df_date_sub$date_pred <- week_date
  # append to final test data frame
  df_date <- rbind(df_date, df_date_sub)
}

# generate lagged dates (needed to merge new info)
  # lag one year
week_date_lag_one_year <- df_date$date_pred - lubridate::weeks(52)
df_date$date_lag_one_year <- week_date_lag_one_year
  # lag one month
week_date_lag_one_month <- df_date$date_pred - lubridate::weeks(4)
df_date$date_lag_one_month <- week_date_lag_one_month
  ## two month
week_date_lag_two_month <- df_date$date_pred - lubridate::weeks(8)
df_date$date_lag_two_month <- week_date_lag_two_month
  # lag three month
week_date_lag_three_month <- df_date$date_pred - lubridate::weeks(12)
df_date$date_lag_three_month <- week_date_lag_three_month
  ## six month
week_date_lag_six_month <- df_date$date_pred - lubridate::weeks(24)
df_date$date_lag_six_month <- week_date_lag_six_month
  ## 9 month
week_date_lag_nine_month <- df_date$date_pred - lubridate::weeks(36)
df_date$date_lag_nine_month <- week_date_lag_nine_month
  # lag one week
week_date_lag_one_week <- df_date$date_pred - lubridate::weeks(1)
df_date$date_lag_one_week <- week_date_lag_one_week
  # lag two weeks
week_date_lag_two_week <- df_date$date_pred - lubridate::weeks(2)
df_date$date_lag_two_week <- week_date_lag_two_week
  # lag three weeks
week_date_lag_three_week <- df_date$date_pred - lubridate::weeks(3)
df_date$date_lag_three_week <- week_date_lag_three_week


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### test data: lagged price variables ####
#%%%

# one week, one month, one year

# create data frame with prices
df_predictors_price <-
  df_ML_raw %>% 
  dplyr::select(date, food, type, size, country, 
                matches("^price_[A-Z]$"), "price_total") %>%
  distinct() 

# create price columns
# columns with current prices stay missing as they are the outcome variables
df_date_price <- df_date %>% 
  dplyr::select(starts_with("date_"))

# one week lags
df_price_weeks <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_price, df_date_price, 
             by = c("date" = "date_lag_one_week")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_one_week" = "date") %>%
  # add suffix
  rename_at(vars(starts_with("price_")), function(x) paste0(x,"_lag_one_week"))

# one month lags
df_price_month <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_price, df_date_price, 
             by = c("date" = "date_lag_one_month")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_one_month" = "date") %>%
  # add suffix
  rename_at(vars(starts_with("price_")), function(x) paste0(x,"_lag_one_month"))

# one year lags
df_price_year <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_price, df_date_price, 
             by = c("date" = "date_lag_one_year")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_one_year" = "date") %>%
  # add suffix
  rename_at(vars(starts_with("price_")), function(x) paste0(x,"_lag_one_year"))


# merge data
  ## identify columns to merge
price_merge_cols <- c(
  "food", "type", "size", "country",
  df_price_weeks %>% dplyr::select(starts_with("date_")) %>% colnames()
)
  ## left_join with starting year is important since only year contains all
  ## observations; for weeks 2-4 variables are missing since no data is
  ## available (those are replaced later). 
df_price_test_data <-
  left_join(df_price_year, df_price_month, 
             by = price_merge_cols) %>%
  left_join(df_price_weeks, by = price_merge_cols)


# data frame should contain 660 rows
  # 165 food-type-size-country combinations
  # 4 prediction dates
  # -> 165*4 = 660
nrow(df_price_test_data) # yes: 660


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### test data: weather lags ####
#%%
  ## 2 weeks, 1 month, 3 months, 6 months, 9 months, 12 months

# merge data set
df_date_weather <-
  df_date %>% 
  dplyr::select(starts_with("date_")) #%>%
  #dplyr::select(-date_lag_one_week)

# predictor data set for weather
weather_vars <- df_predictors %>% 
  dplyr::select(temp, humidity, precip, windspeed, cloudcover,
         starts_with("dev_"), frost) %>%
  colnames()

df_predictors_weather <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(weather_vars))

# two week lags
df_weather_weeks <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_weather, df_date_weather, 
             by = c("date" = "date_lag_two_week")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_two_week" = "date") %>%
  # add suffix
  rename_at(vars(all_of(weather_vars)), function(x) paste0("lag_2_weeks_", x))


# one month lags
df_weather_one_month <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_weather, df_date_weather, 
             by = c("date" = "date_lag_one_month")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_one_month" = "date") %>%
  # add suffix
  rename_at(vars(all_of(weather_vars)), function(x) paste0("lag_1_months_", x))


# three month lags
df_weather_three_month <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_weather, df_date_weather, 
             by = c("date" = "date_lag_three_month")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_three_month" = "date") %>%
  # add suffix
  rename_at(vars(all_of(weather_vars)), function(x) paste0("lag_3_months_", x))


# six month lags
df_weather_six_month <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_weather, df_date_weather, 
             by = c("date" = "date_lag_six_month")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_six_month" = "date") %>%
  # add suffix
  rename_at(vars(all_of(weather_vars)), function(x) paste0("lag_6_months_", x))


# nine month lags
df_weather_nine_month <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_weather, df_date_weather, 
             by = c("date" = "date_lag_nine_month")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_nine_month" = "date") %>%
  # add suffix
  rename_at(vars(all_of(weather_vars)), function(x) paste0("lag_9_months_", x))


# twelve month lags
df_weather_one_year <- 
  # join data so that we get the lagged one month price values
  right_join(df_predictors_weather, df_date_weather, 
             by = c("date" = "date_lag_one_year")) %>%
  # rename date (needed for merge later)
  dplyr::rename("date_lag_one_year" = "date") %>%
  # add suffix
  rename_at(vars(all_of(weather_vars)), function(x) paste0("lag_12_months_", x))


# merge
weather_merge_cols <- c(
  "food", "type", "size", "country",
  df_weather_weeks %>% dplyr::select(starts_with("date_")) %>% colnames()
)
df_weather_test_data <-
  left_join(df_weather_one_year, df_weather_nine_month,
             by = weather_merge_cols) %>%
  left_join(df_weather_three_month, by = weather_merge_cols) %>%
  left_join(df_weather_six_month, by = weather_merge_cols) %>%
  left_join(df_weather_one_month, by = weather_merge_cols) %>%
  left_join(df_weather_weeks, by = weather_merge_cols) 

# ensure that all lag variables are numeric
df_weather_test_data <- df_weather_test_data %>%
  mutate_at(vars(starts_with("lag_")), ~as.numeric(.))

# check if weather data set contains 660 rows
nrow(df_weather_test_data)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### test data: other lags ####
#%%

# fertilizer: one month, two month, three month
# kerosene: one week, three week
# oil: one month
# wci: two weeks, one month
# exchange rate: one week, three week
# fuel price: one week
# inflation rate: one month

# merge data set
df_date_lags <-
  df_date %>% 
  dplyr::select(starts_with("date_")) 

# predictor data set for other lags
  ## three month
lag_vars_three_month <- df_predictors %>% 
  dplyr::select(DAP, Urea, TSP, Phosphate_Rock, Potassium_chloride) %>%
  colnames()
  ## two months
lag_vars_two_month <- df_predictors %>% 
  dplyr::select(DAP, Urea, TSP, Phosphate_Rock, Potassium_chloride) %>%
  colnames()
  ## one month
lag_vars_one_month <- df_predictors %>% 
  dplyr::select(DAP, Urea, TSP, Phosphate_Rock, Crude_Oil_Price, Potassium_chloride, 
         wci, inflation_perc_Germany) %>%
  colnames()
  ## three weeks
lag_vars_three_week <- df_predictors %>% 
  dplyr::select(Kerosene_Price, starts_with("exchange_")) %>%
  colnames()
  ## two weeks
lag_vars_two_week <- df_predictors %>% 
  dplyr::select(wci) %>%
  colnames()
  ## one week
lag_vars_one_week <- df_predictors %>% 
  dplyr::select(Kerosene_Price, starts_with("exchange_"), fuel_price_diesel_Germany) %>%
  colnames()


# calculate lags
  ## three months
df_predictors_lag <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(lag_vars_three_month))
df_lag_three_month <- 
  right_join(df_predictors_lag, df_date_lags, 
             by = c("date" = "date_lag_three_month")) %>%
  dplyr::rename("date_lag_three_month" = "date") %>%
  rename_at(vars(all_of(lag_vars_two_month)), 
            function(x) paste0(x, "_lag_three_month"))
  ## two month
df_predictors_lag <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(lag_vars_two_month))
df_lag_two_month <- 
  right_join(df_predictors_lag, df_date_lags, 
             by = c("date" = "date_lag_two_month")) %>%
  dplyr::rename("date_lag_two_month" = "date") %>%
  rename_at(vars(all_of(lag_vars_two_month)), 
            function(x) paste0(x, "_lag_two_month"))
  ## one month
df_predictors_lag <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(lag_vars_one_month))
df_lag_one_month <- 
  right_join(df_predictors_lag, df_date_lags, 
             by = c("date" = "date_lag_one_month")) %>%
  dplyr::rename("date_lag_one_month" = "date") %>%
  rename_at(vars(all_of(lag_vars_one_month)), 
            function(x) paste0(x, "_lag_one_month"))
  ## three weeks
df_predictors_lag <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(lag_vars_three_week))
df_lag_three_week <- 
  right_join(df_predictors_lag, df_date_lags, 
             by = c("date" = "date_lag_three_week")) %>%
  dplyr::rename("date_lag_three_week" = "date") %>%
  rename_at(vars(all_of(lag_vars_three_week)), 
            function(x) paste0(x, "_lag_three_week"))
  ## two weeks
df_predictors_lag <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(lag_vars_two_week))
df_lag_two_week <- 
  right_join(df_predictors_lag, df_date_lags, 
             by = c("date" = "date_lag_two_week")) %>%
  dplyr::rename("date_lag_two_week" = "date") %>%
  rename_at(vars(all_of(lag_vars_two_week)), 
            function(x) paste0(x, "_lag_two_weeks"))
  ## one week
df_predictors_lag <- df_predictors %>% 
  dplyr::select(date, food, type, country, size, all_of(lag_vars_one_week))
df_lag_one_week <- 
  right_join(df_predictors_lag, df_date_lags, 
             by = c("date" = "date_lag_one_week")) %>%
  dplyr::rename("date_lag_one_week" = "date") %>%
  rename_at(vars(all_of(lag_vars_one_week)), 
            function(x) paste0(x, "_lag_one_week"))

# merge data frame
df_lag_test_data <-
  left_join(df_lag_three_month, df_lag_two_month) %>%
  left_join(df_lag_one_month) %>% 
  left_join(df_lag_three_week) %>%
  left_join(df_lag_two_week) %>%
  left_join(df_lag_one_week) 

nrow(df_lag_test_data)

# rename columns
df_lag_test_data <- df_lag_test_data %>%
  dplyr::rename(
    # exchange rate
    exchange_rate_lag_three_week_South_Africa = exchange_rate_South_Africa_lag_three_week,
    exchange_rate_lag_one_week_South_Africa = exchange_rate_South_Africa_lag_one_week,
    exchange_rate_lag_one_week_Turkey = exchange_rate_Turkey_lag_one_week,
    exchange_rate_lag_three_week_Turkey = exchange_rate_Turkey_lag_three_week,
    exchange_rate_lag_three_week_New_Zealand = exchange_rate_New_Zealand_lag_three_week, 
    exchange_rate_lag_one_week_New_Zealand = exchange_rate_New_Zealand_lag_one_week, 
    # fuel price
    fuel_price_diesel_lag_one_week_Germany = fuel_price_diesel_Germany_lag_one_week, 
    # inflation rate
    inflation_perc_lag_one_month_Germany = inflation_perc_Germany_lag_one_month,
    # world container index
    wci_1_month = wci_lag_one_month,
    wci_2_weeks = wci_lag_two_weeks
  )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### test data: season ####

# extract data
df_date_season <-
  df_date %>% 
  dplyr::select(starts_with("date_")) 

df_season_test_data <- 
  df_predictors %>%
  dplyr::select(date, food, type, size, country,
         season, storage, openland, greenhouse_heated, greenhouse_unheated, film) %>%
  # season is simply merged via month
  mutate(month = data.table::month(date)) %>%
  # drop date as it is not needed
  dplyr::select(-date) %>%
  # keep only distinct columns
  distinct()

nrow(df_season_test_data) # 165*12 = 1980 (165 combinations and 12 month)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### test data: distances ####

# distance variables are constant over time
df_distance_test_data <- df_ML_raw %>%
  dplyr::select(food, type, country, size, starts_with("distance_")) %>%
  distinct()
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### test data: current level variables ####

# add other variables which are actually unknown: 
# same values than for previous week are used

df_other_test_data <-
  df_ML_current_var %>%
  dplyr::select(date, food, type, country, size, 
         inflation_perc_Germany, Kerosene_Price, Crude_Oil_Price, 
         fuel_price_diesel_Germany, wci, 
         Phosphate_Rock, Potassium_chloride, DAP, TSP, Urea,
         exchange_rate_New_Zealand, exchange_rate_South_Africa, 
         exchange_rate_Turkey) %>%
  # factor variables as character
  mutate_if(is.factor, as.character)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Merge ####

merge_date_cols <- 
  df_weather_test_data %>% dplyr::select(starts_with("date")) %>% colnames()

# Step 1: merge prices and weather
# key are all columns starting with data, food, size, type and country
df_test_data <- 
  left_join(
    df_weather_test_data, df_price_test_data, 
    by = c("food", "size", "type", "country", all_of(merge_date_cols))
  )
nrow(df_test_data)

# Step 2: add date data frame (containing week, year and month dummys)
# key is only the prediction date (date_pred) as this data frame does not
# change over time
df_test_data <-
  left_join(df_test_data, df_date, by = c(all_of(merge_date_cols)))
nrow(df_test_data)

# Step 3: add season variables
# key is food, type, size, country, and month
# hence first a month column needs to be created in df_test_data
df_test_data <- df_test_data %>%
  # create month variable for merging
  mutate("month" = data.table::month(date_pred)) %>%
  # merge 
  left_join(df_season_test_data,
            by = c("food", "size", "type", "country", "month")) %>%
  # drop month variable again
  dplyr::select(-month)

nrow(df_test_data)

# Step 4: add distance variables
# key is food, type, size, country (does not depend on date)
df_test_data <- df_test_data %>%
  left_join(df_distance_test_data, by = c("food", "size", "type", "country"))

# Step 5: add other lagged variables
df_test_data <- df_test_data %>%
  left_join(df_lag_test_data, 
            by = c("food", "size", "type", "country", all_of(merge_date_cols)))

# Step 6: add variables which are unknown in future (current level is kept)
# key is food, type, size, country (current level is unknown previous is used)
df_test_data <- df_test_data %>%
  left_join(df_other_test_data,
            by = c("food", "size", "type", "country"))

# print columns which are in final prediction but not test data set
  ## should only be price variables
colnames(df_ML_raw)[!colnames(df_ML_raw) %in% intersect(colnames(df_test_data), 
                                                        colnames(df_ML_raw))]

# check columns of test data: should be 660
nrow(df_test_data)

# prediction for next week should not contain missing in outcome variables
df_test_data_next_week <- df_test_data %>%
  filter(date_pred == week_1_date)
sum(is.na(df_test_data_next_week))
colnames(df_test_data_next_week)[colSums(is.na(df_test_data_next_week)) > 0]

# check columns with missings in general:
  ## price variables for one week -> good! -> are replaced later
  ## missing weather information is replaced by forecasts
  ## for other missing's last known value is used
colnames(df_test_data)[colSums(is.na(df_test_data)) > 0]


#### Add weather forecast ####

# load weather forcast
df_weather_forcast <- read.csv("output/weather_forecast.csv")

# make adjustments to country -> capital case
df_weather_forcast$country <- str_to_title(df_weather_forcast$country)

# generate date
df_weather_forcast$date_forcast <- 
  as.Date(paste(df_weather_forcast$year, df_weather_forcast$week, 1, 
                sep = "-"), "%Y-%U-%u")


# missing values are for predictions weeks 3 and 4 as lags of two weeks are used
# for those values the forcasted weather is inserted
# extract date of weeks 3 and 4 (actually variable as first two weeks are dropped)
date_replace_weather <- df_test_data %>%
  dplyr::select(date_pred) %>%
  distinct() %>%
  arrange(date_pred) %>%
  dplyr::slice(-c(1, 2)) %>%
  dplyr::select(date_pred) %>%
  pull()

# extract relevant weather rows and variables 
df_weather_forcast <- df_weather_forcast %>% 
  filter(date_forcast %in% date_replace_weather) %>%
  dplyr::select(date_forcast, country, fruits, vegetables,
         temp, humidity, precip, windspeed, cloudcover, frost,
         starts_with("dev")) %>% 
  distinct() 

# add prefix to indicate forcasts after merge
colnames(df_weather_forcast)[
  !colnames(df_weather_forcast) %in% 
    c("date_forcast", "country", "fruits", "vegetables")] <-
  paste0("forcast_", colnames(df_weather_forcast)[
    !colnames(df_weather_forcast) %in% 
      c("date_forcast", "country", "fruits", "vegetables")])


# define fruits
fruits <- c("Apple", "Banana", "Grapes", "Kiwi", "Lemon", "Nectarine", 
            "Peach", "Pear",  "Orange", "Strawberry")

# add indicator for fruits and vegetables to test data (needed to merge weather)
df_test_data$fruits <- ifelse(df_test_data$food %in% fruits, 1, 0)
df_test_data$vegetables <- ifelse(df_test_data$food %in% fruits, 0, 1)

# merge forcast
  ## needs to be done separately for fruits and vegetables
df_test_data_fruits <- df_test_data %>% filter(fruits == 1) %>% dplyr::select(-vegetables)
df_test_data_veggies <- df_test_data %>% filter(vegetables == 1) %>% dplyr::select(-fruits)
df_weather_forcast_fruits <- df_weather_forcast %>%
  filter(fruits == 1) %>% dplyr::select(-vegetables)
df_weather_forcast_veggies <- df_weather_forcast %>%
  filter(vegetables == 1) %>% dplyr::select(-fruits)

df_test_data_fruits <- left_join(df_test_data_fruits, df_weather_forcast_fruits,
                                 by = c("date_pred" = "date_forcast", "country",
                                        "fruits")) %>%
  mutate(vegetables = 0)

df_test_data_veggies <- left_join(df_test_data_veggies, df_weather_forcast_veggies,
                                 by = c("date_pred" = "date_forcast", "country",
                                        "vegetables")) %>%
  mutate(fruits = 0)

df_test_data <- rbind(df_test_data_fruits, df_test_data_veggies)

# extract weather lag variables
weather_lags <- 
  colnames(df_test_data)[colSums(is.na(df_test_data)) > 0][
    str_detect(colnames(df_test_data)[colSums(is.na(df_test_data)) > 0], "lag_2")]

# iterate over weather lags to make the replacement
for (i in weather_lags) {
  # extract variable by removing lag_2_weeks and add "forcast_"
  weather_vars_match <- paste0("forcast_", str_remove(i, "lag_2_weeks_"))
  # extract rows with missings and insert predicted weather
  df_test_data[is.na(df_test_data[,i]), c("date_lag_one_year", i)] <-
    df_test_data[is.na(df_test_data[,i]), c("date_lag_one_year", weather_vars_match)]
}

# drop forcast variables
df_test_data <- df_test_data %>%
  dplyr::select(-starts_with("forcast"))

# check if missing weather values are removed
colnames(df_test_data)[colSums(is.na(df_test_data)) > 0]



#### Add current values for missing lags ####

# wci
df_test_data <- 
  df_test_data %>%
  mutate_at(vars(wci_2_weeks), 
            ~tidyr::replace_na(., unique(df_ML_current_var$wci)))

# fuel price
df_test_data <- 
  df_test_data %>%
  mutate_at(vars(fuel_price_diesel_lag_one_week_Germany), 
            ~tidyr::replace_na(., unique(df_ML_current_var$fuel_price_diesel_Germany)))


# kerosene price
df_test_data <- 
  df_test_data %>%
  mutate_at(vars(Kerosene_Price_lag_one_week, Kerosene_Price_lag_three_week, Kerosene_Price), 
            ~tidyr::replace_na(., unique(df_ML_current_var$Kerosene_Price)))

# exchange rates:
new_zealand_cols <- 
  df_test_data %>% dplyr::select(matches("^exchange_.*_New_Zealand$")) %>% colnames()
df_test_data <- 
  df_test_data %>%
  mutate_at(vars(all_of(new_zealand_cols)), 
            ~tidyr::replace_na(., unique(df_ML_current_var$exchange_rate_New_Zealand)))

turkey_cols <- 
  df_test_data %>% dplyr::select(matches("^exchange_.*_Turkey$")) %>% colnames()
df_test_data <- 
  df_test_data %>%
  mutate_at(vars(all_of(turkey_cols)), 
            ~tidyr::replace_na(., unique(df_ML_current_var$exchange_rate_Turkey)))

africa_cols <- 
  df_test_data %>% dplyr::select(matches("^exchange_.*_South_Africa$")) %>% colnames()
df_test_data <- 
  df_test_data %>%
  mutate_at(vars(all_of(africa_cols)), 
            ~tidyr::replace_na(., unique(df_ML_current_var$exchange_rate_South_Africa)))


# check again for missings
  ## only price variables are still missing
colnames(df_test_data)[colSums(is.na(df_test_data)) > 0]


#### Add price variables ####

df_test_data <- df_test_data %>%
  mutate(
    price_F = NA, price_M = NA, price_K = NA, price_B = NA, price_H = NA,
    price_total = NA
  )

# the missing price variables are later replaced by the predictions...


#### Last Checks & Saving ####

# drop date variables: only date_pred is kept and renamed
drop_date_cols <- df_test_data %>% 
  dplyr::select(starts_with("date")) %>% 
  dplyr::select(-date_pred) %>%
  colnames()
df_test_data <- df_test_data %>%
  dplyr::select(-c(all_of(drop_date_cols)))
  

# check number of variables
ncol(df_test_data) == ncol(df_ML_raw) # should be TRUE
  # date columns is called differently to avoid confusion
colnames(df_ML_raw)[!colnames(df_ML_raw) %in% intersect(colnames(df_test_data), 
                                                        colnames(df_ML_raw))]
colnames(df_test_data)[!colnames(df_test_data) %in% intersect(colnames(df_test_data), 
                                                              colnames(df_ML_raw))]

# check data types
# factor variables (frost) as numeric
df_test_data <- df_test_data %>%
  mutate_if(is.factor, as.numeric)
# all integer columns as numeric
df_test_data <- df_test_data %>% mutate_if(is.integer, as.numeric)

# check for missing values; only price variables are allowed to have missings
colnames(df_test_data)[colSums(is.na(df_test_data)) > 0]

# save general test data set
saveRDS(df_test_data, "output/final_test_data.rds")



