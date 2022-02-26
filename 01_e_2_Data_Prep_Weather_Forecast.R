
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Preparation Weather Forecast for ML prediction ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# This file extracts the forecast weather data for the insertion into the ML algorithms. This file generates file till the
# 15 February 2022. First, historical statistical forecast from 01.01.2022 - 31.01.2022 is going to be generated. Additionally,
# Current (and not historical, as of 01.02.2022) forecast data is going to be added from the 01.02.2022, up to 14 days, the 14.02.2022.

# Afterwards, the file 02_e_2 is going to update the forecast data based on the newest given date. The Visual Crossing API is used as a base for
# the data retrieval. All details about it are explained in 01_e.

# We can access weather forecast data up to 16 days after the current date. This and further information can be retrieved from this URL
# (https://www.visualcrossing.com/resources/documentation/weather-api/timeline-weather-api/). However, for the data prediction with out
# machine learning algorithms, the date retrieval and updating will be limited to 14 forecast days (2 weeks).

# We are interested ion weather forecast data from the 01.01.2022 up to the current date. However, past statistical forecasts
# can not be retrieved. Only current forecast weather can be retrived. This is the reason that we will first access the historical
# forecast weather up to the 31.01.2022 and then the current statistical forecast. The updating 02_e_2 file will only update the 
# current statistical forecast. 

rm(list=ls())

# Install if needed relevant packages

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")

# Load relevant packages

library(jsonlite)  # For API data retrieval
library(httr)  # For API data retrieval
library(tidyverse)  # For data wrangling
library(data.table) # used in the second and third for-loop

source("API_Key_Weather.R")  # Source API in different folder for security reasons

# Create a data frame with a list of cities for which we want the weather information

locations <- data.frame(val=c("general roca","gent","santiago de chile", "toulouse", "rennes", "cologne", "stuttgart", "sparta", "kissamos", "alexandria", "bari", "catania",
                              "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "valencia", "paarl", "antalya", "guayaquil","medellin","panama city","costa rica"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### For-loop that retrieves date from 01.01.2022 - 31.01.2022 with historical statistical forecast data ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


# First data frame will get us the largest part of the weather data, so the RShiny App does not have to load thousands of rows every time we load the app

date_begin <- as.Date("2022-01-01")  
date_end <- as.Date("2022-01-31")

# I create an empty data frame with the dimensions that we will be filled with the weather data

days <- as.numeric(date_end - date_begin) + 1

a <- as.data.frame(matrix(0, ncol=35+3, nrow=(nrow(locations)*days)))

# The columns names were not manually retrieved but with the code from line 146. This was done from a previous dataset

# paste("'",as.character(c),"'",collapse=", ",sep="")

column_names <- c('datetime', 'datetimeEpoch', 'tempmax', 'tempmin', 'temp', 'feelslikemax', 'feelslikemin', 'feelslike', 'dew', 'humidity', 'precip', 'precipprob', 'precipcover', 'preciptype', 'snow', 'snowdepth', 'windgust', 'windspeed', 'winddir', 'pressure', 'cloudcover', 'visibility', 'solarradiation', 'solarenergy', 'uvindex', 'sunrise', 'sunriseEpoch', 'sunset', 'sunsetEpoch', 'moonphase', 'conditions', 'description', 'icon', 'stations', 'source', 'location','latitude','longitude')

colnames(a) <- column_names


# Now the 1. for-loop retrieves data from the VisualCrossing API. This API, although not free, is relatively cheap and easy to use. Additionally, it includes all relevant weather
# variables for numerous cities across the globe

for(i in 1:nrow(locations)) {
  
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",date_begin,"/",date_end,"?unitGroup=metric&key=",API_Key_weather,"&include=stats")
  
  r <- GET(url)
  
  api_content <- content(r, as="text")
  
  data <- fromJSON(api_content)
  
  d <- data[["days"]]
  
  a[(days * i - (days-1)):(days * i),] <- d
  
  a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]
  
  a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
  
  a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
  
} 

# Save the created data frame in object b, so second for-loop can use same exact code and be the newest object a.
# After successful retrieval of the second data frame, they will be rowbinded and called "a" again.

b <- a

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### For-loop that retrieves date from 01.02.2022 - 14.02.2022 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

date_begin <- as.Date("2022-02-01")  
date_end <- as.Date("2022-02-14")

# I create an empty data frame with the dimensions that we will be filled with the weather data

days <- as.numeric(date_end - date_begin) + 1

a <- as.data.frame(matrix(0, ncol=35+3, nrow=(nrow(locations)*days)))

# The columns names were not manually retrieved but with the code from line 146. This was done from a previous dataset

# paste("'",as.character(c),"'",collapse=", ",sep="")

column_names <- c('datetime', 'datetimeEpoch', 'tempmax', 'tempmin', 'temp', 'feelslikemax', 'feelslikemin', 'feelslike', 'dew', 'humidity', 'precip', 'precipprob', 'precipcover', 'preciptype', 'snow', 'snowdepth', 'windgust', 'windspeed', 'winddir', 'pressure', 'cloudcover', 'visibility', 'solarradiation', 'solarenergy', 'uvindex', 'severerisk','sunrise', 'sunriseEpoch', 'sunset', 'sunsetEpoch', 'moonphase', 'conditions', 'description', 'icon', 'source', 'location','latitude','longitude')

colnames(a) <- column_names


for(i in 1:nrow(locations)) {
  
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",date_begin,"/",date_end,"?unitGroup=metric&key=",API_Key_weather,"&include=fcst")
  
  r <- GET(url)
  
  api_content <- content(r, as="text")
  
  data <- fromJSON(api_content)
  
  d <- data[["days"]]
  
  a[(days * i - (days-1)):(days * i),] <- d
  
  a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]
  
  a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
  
  a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
  
} 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Bind both data frame into one data frame for further data transformations ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

a <- a %>% select(-severerisk)
b <- b %>% select(-stations)

a <- rbind(a, b)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Data Wrangling & add dummies for merging with the main data frame ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# The variables stations is in a list format. To facilitate further data wrangling, I change to the character format

#a$stations <- as.character(a$stations)
a$datetime <- as.Date(a$datetime)

# To merge the weather data with the main data frame, we need unique IDs. To uniquely identify the rows with the master data set, I define three new variables: country variable
# with the country name, and two variables that define weather the weather information belongs to fruits or vegetables.

a$country <- 0

a$vegetables <- 0

a$fruits <- 0

countries <- c("Argentina", "Belgium", "Chile", "France", "France", "Germany", "Germany", "Greece", "Greece", "Egypt", "Italy", "Italy", "Morocco",
               "Netherlands", "New Zealand", "Poland", "Peru", "Spain", "Spain", "South Africa", "Turkey", rep("Unknown",4))

# As the exact weather location of the vegetables and fruits is not known, the data scientist muss infer the original locations. Here, this is done by researching trough the internet
# and the website "europages.com". Here, the methodological assumption is that we infer the original location by looking manually at each countries suppliers and growing areas.
# The city with the most fruit and vegetables suppliers is chosen as the most likely original location. Of course, this is an assumption and the real original location might me completely
# different. In addition, some countries have growing areas distributed across the whole country, so the city that is in the center is chosen. Of course, this has several
# data restrictions and shortcomings. However, this is the most accurate method to retrieve the original locations. Without it, weather aggregations fro the whole country would
# have to be used. This method would severely ias the weather data for countries with different weather zones e.g. italy.

# Some cities are assumed to be the main location for both vegetables and fruits. However, some cities are assumed to be specific for vegetables and vice versa.
# Thereby, numerous cities will be included for both variables and only differ in some specific countries.

vegetables_cities <- c("general roca","gent","santiago de chile", "rennes", "cologne", "kissamos", "alexandria", "bari",
                       "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "paarl", "antalya")

fruits_cities <- c("general roca","gent","santiago de chile", "toulouse", "stuttgart", "sparta", "alexandria", "catania",
                   "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "valencia", "paarl", "antalya", "guayaquil", "medellin", "panama city", "costa rica")

# Both for-loops create the row values for the variables country, vegetables and fruits.

for (i in 1:nrow(locations)) {
  a$country[a$location == locations$val[i]] <- countries[i]
}

for (i in 1:length(vegetables_cities)) {
  a$vegetables[a$location == vegetables_cities[i]] <- 1
  #a$fruits[a$location == fruits_cities[i]] <- 1
}

for (i in 1:length(fruits_cities)) {
  a$fruits[a$location == fruits_cities[i]] <- 1
}

# Make the data more "tidy"

a <- a %>%
  select(location, country, datetime:fruits) %>%
  arrange(location, country, datetime)

# The for-loop changes the city variable to a list variable. This has to be changed to character to allow the export of the data to the 
# output folder

a$location <- as.character(a$location)
a$preciptype <- as.character(a$preciptype)

# Export the retrieved data

fwrite(a, "output/prediction_weather_forecast_first_df.csv")

