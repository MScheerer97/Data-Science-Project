rm(list=ls())

### Update Weather Data ###

# Install if needed relevant packages

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("data.table")) install.packages("data.table")

# Load relevant packages

library(jsonlite)
library(httr)
library(tidyverse)
library(data.table) 

# Source the API Key for the weather

source("API_Key_Weather.R")

#r <- GET("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/stuttgart/2022-02-02/2022-02-02?unitGroup=metric&key=MK2WLG7W2XD47CTC5NQ68NFST&include=fcst")

#api_content <- content(r, as="text")

#data <- fromJSON(api_content)

#d <- data[["days"]]


# This function update_forecast() updates the dates of the forecast csv file. This function updates the forecast weather data depending on the observational weather
# data "updated_df_weather.csv". The observational data uses historical weather 5 days before the actual date (e.g. if the actual date is 24.11.2021, the date is updated up to the
# 19.11.2021). This is done because the city "Huanuco" from Peru reports the historical data in a 5-day delay. Consequently, the forecast weather data follows the same concept.

# In contrast to the historical weather data, which is done by "02_e_Update Weather.R", the forecast function not only retrieves past weather data but also future weather predictions.
# For future machine-learning predictions, we might be interested how the future weather forecast affects current fruit and vegetable prices. To account for this future lag,
# 14-days is used as a base-timeline lag. The key interest is to assess the 14-days effect of future (forecast) weather on the current fruit/vegetable prices.

# The key objective of the function "update_forecast", is to update the forecast weather according to the historical observational data. In order to analyse the future lag effect
# of the forecast weather data, the forecast weather should always be available 14 days after the current observational weather date. 

# If the "updated_df_weather_forecast.csv" does not give statistical forecast data 14-days after the current date of the "updated_df_weather.csv" file, this has to be updated
# and adjusted. If future 14-days forecast weather is available, no changes are needed and the function signifies this by printing "No changes are needed. Up do date".


update_forecast <- function(current_date) {  # Creating function to facilitate the data acquisition in the R Shiny file
  
  updated_weather <- fread("output/prediction_weather_forecast.csv")  # Import the weather forecast updated data frame
  
  #updated_weather <- updated_weather[,-1]  # Drop the first column, as this is just an row counter for identification
  
  max_date <- as.Date(max(updated_weather$datetime))  # Retrieve the last date in the file
  
  lagged_date <- as.Date(max_date - 13)  # As the max date is the 14-days future date and does not the represent the current date, this must be subtracted 
  
  # Naming the city location from which we want to retrieve weather information
  
  locations <- data.frame(val=c("general roca","gent","santiago de chile", "toulouse", "rennes", "cologne", "stuttgart", "sparta", "kissamos", "alexandria", "bari", "catania",
                                "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "valencia", "paarl", "antalya", "guayaquil", "medellin", "panama city", "costa rica"))
  
  # As the API needs two dates to retrieve the weather data, and the function row binds the new observations, there is no need to update the next two weeks again and again, and
  # therefore creating unnecessary duplicates. For example: The current date is "20.11.2021". Because of Huanuco, a 5-days lag is needed. Consequently, only forecast weather
  # data up to the "15.11.2021" is retrieved. For the future time lag, forecast data needs to be retrieved 14-days after the lagged date "15.11.2021", which is, in this example
  # equivalent to the "29.11.2021". However, in the next data acquisition process/iteration, the current date is now e.g. "22.11.2021". The lagged date "17.11.2021" and the 
  # future forecast lag the future end date should be the "01.12.2021". However, in the previous iteration the dates are retrieved from the up to the "15.11.2021 - 29.11.2021".
  # Now retrieving weather date from the "17.11.2021" to the "01.12.2021" would end up duplicating the weather date from the "17.11.2021 to the 29.11.2021". 
  # As a result, and in order to avoid duplicates, not all weather data should be retrieved again, but only 2-days (30-11, 01-12). The difference between the new last date
  # and the old last date. The new date for the API should be one day after the last date in the previous iteration
  
  date_begin <- max_date + 1
  
  if (current_date > lagged_date) {  
    
    date_diff <- as.numeric(current_date - lagged_date)
    
    date_end <- date_begin + date_diff - 1
    
    days <- date_diff  # + 1
    
    a <- as.data.frame(matrix(0, ncol=35 + 4, nrow=(nrow(locations)*days)))  # Creating a data frame with the correct dimensions that are filled with the data from the next lines
    
    # Correctly naming the column. This was not typed in manually but retrieved by using colnames() from a previously imported data set 
    
    column_names <- c('datetime', 'datetimeEpoch', 'tempmax', 'tempmin', 'temp', 'feelslikemax', 'feelslikemin', 'feelslike', 'dew', 'humidity', 'precip', 'precipprob', 'precipcover', 'preciptype', 'snow', 'snowdepth', 'windgust', 'windspeed', 'winddir', 'pressure', 'cloudcover', 'visibility', 'solarradiation', 'solarenergy', 'uvindex', 'severerisk', 'sunrise', 'sunriseEpoch', 'sunset', 'sunsetEpoch', 'moonphase', 'conditions', 'description', 'icon', 'stations', 'source', 'location', 'latitude', 'longitude')
    
    colnames(a) <- column_names
    
    # historical observational data and historical forecast weather data
    
    for(i in 1:nrow(locations)) {
      
      url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",date_begin,"/",date_end,"?unitGroup=metric&key=",API_Key_weather,"&include=fcts")
      
      r <- GET(url)
      
      api_content <- content(r, as="text")
      
      data <- fromJSON(api_content)
      
      d <- data[["days"]]
      
      a[(days * i - (days-1)):(days * i), 1:36] <- d[,1:36] # [,1:35]
      
      a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]  # Fill rows with the information form the API
      
      a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
      
      a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
      
    } 
    
    # The variables must be in the correct format to allow row binding in the next lines
    
    a$stations <- as.character(a$stations)
    a$location <- as.character(a$location)
    a$longitude <- as.character(a$longitude)
    a$latitude <- as.character(a$latitude)
    
    # Not all variables are changed to the correct format! In this case, I solely change the format of the variable "datetime". This is needed to facilitate row binding in the
    # next steps. Without it, rows in the column "datetime" will be falsely specified. In the data transformations part, the data types of the variables will be changed.
    # In this step, the focus lies on retrieving the data and not data transformations
    
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
    # The city with the most fruit and vegetables suppliers is chosen as the most likely original location. Of course, this is an assumption and the original location might be completely
    # different. In addition, some countries have growing areas distributed across the whole country, so the city that is in the center is chosen as the original city. Of course, this has several
    # data restrictions and shortcomings. However, this is the most accurate method to retrieve the original locations. Without it, weather aggregations for the whole country would
    # have to be used. This method would severely bias the weather data for countries with different weather zones e.g. italy.
    
    # Some cities are assumed to be the main location for both vegetables and fruits. However, some cities are only "fruit-based" cities, other "vegetable-based".
    # Thereby, numerous cities will be included for both variables and only differ for some specific countries.
    
    vegetables_cities <- c("general roca","gent","santiago de chile", "rennes", "cologne", "kissamos", "alexandria", "bari",
                           "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "paarl", "antalya")
    
    fruits_cities <- c("general roca","gent","santiago de chile", "toulouse","stuttgart", "sparta", "alexandria", "catania",
                       "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "valencia", "paarl", "antalya", "guayaquil", "medellin","panama city","costa rica")
    
    # Both for-loops create the row values for the variables country, vegetables and fruits.
    
    for (i in 1:nrow(locations)) {
      a$country[a$location == locations$val[i]] <- countries[i]
    }
    
    for (i in 1:length(vegetables_cities)) {
      a$vegetables[a$location == vegetables_cities[i]] <- 1
    }
    
    for (i in 1:length(fruits_cities)) {
      a$fruits[a$location == fruits_cities[i]] <- 1
    }
    
    a <- a %>%
      select(-c(severerisk, stations))
    
    updated_df <- rbind(a, updated_weather) #updated_df
    
    updated_df <- updated_df %>%
      select(location, country, datetime:fruits) %>%
      arrange(location, country, datetime)
  
    
    fwrite(updated_df, "output/prediction_weather_forecast.csv")  # Export the new data frame with the added new dates/rows
  }  else {
    print("No Update is needed, data is up-to-date")
  }
}

current_date <- Sys.Date()  # Getting the current date

update_forecast(current_date)  
