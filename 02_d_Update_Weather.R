
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
library(data.table) # used in the second and third for-loop

# Source the API Key

source("API_Key_Weather.R")

# Manually rbind the banana country weather with the weather from the other countries. Because of errors in the size
# the size, this is done again

#first_df_weather <- fread("output/first_df_weather.csv")
#first_df_weather <- first_df_weather[,-1]
#banana_weather <- fread("output/weather_banana_2015_2021.csv")
#updated_df <- rbind(first_df_weather, banana_weather)

#fwrite(updated_df, "output/updated_df_weather.csv")

# The function "update_weather()" first checks, whether updates to the data frame are needed, if yes, it will update the current (large) data frame but it will only add
# weather information after the last date in the (large) data frame. If not, the function will let you know that no updates are needed

update_weather <- function(current_date) {
  
  lagged_date <- current_date - 7
  
  updated_weather <- fread("output/updated_df_weather.csv")  # Import the existing weather dataset 
  
  #updated_weather <- updated_weather[,-1]  # The write.csv() function added an ID column. This can be deleted
  
  if (lagged_date > max(updated_weather$datetime)) {  # Checks if new weather data is needed
  
  # Create a data frame with the cities that we want to retrieve the weather information  
    
  locations <- data.frame(val=c("general roca","gent","santiago de chile", "toulouse", "rennes", "cologne", "stuttgart", "sparta", "kissamos", "alexandria", "bari", "catania",
                                "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "valencia", "paarl", "antalya", "guayaquil", "medellin", "panama city", "costa rica"))
  
  # The API URL needs the first date to get the timeline for the weather data extraction. The first date that is used in the update has to be the first day after the last day
  # in the existing (large) data frame
  
  last_date <- as.Date(max(updated_weather$datetime)) + 1 
  
  # To boost loading time and get efficiency, a data frame "a" with the existing dimensions is made, so in the next step it can be filled with the weather data
  
  days <- as.numeric(lagged_date - last_date) + 1
  
  a <- as.data.frame(matrix(0, ncol=35+3, nrow=(nrow(locations)*days)))
  
  # The data frame included the columns "V1,..., VN". They have to be renamed with the weather variable names
  
  # The column names are not manually typed in but are retrieved with one line of code that can be seen in "01_e_Data_Prep_Weather.R"
  
  column_names <- c('datetime', 'datetimeEpoch', 'tempmax', 'tempmin', 'temp', 'feelslikemax', 'feelslikemin', 'feelslike', 'dew', 'humidity', 'precip', 'precipprob', 'precipcover', 'preciptype', 'snow', 'snowdepth', 'windgust', 'windspeed', 'winddir', 'pressure', 'cloudcover', 'visibility', 'solarradiation', 'solarenergy', 'uvindex', 'sunrise', 'sunriseEpoch', 'sunset', 'sunsetEpoch', 'moonphase', 'conditions', 'description', 'icon', 'stations', 'source', 'location','latitude','longitude')
  
  colnames(a) <- column_names
  
  # The for loop retrieves the weather Data from the VisualCrossings API. The URL most important factors are the changing locations, "the first date", "end-date" and the API-Key
  
  for(i in 1:nrow(locations)) {
    
    url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",last_date,"/",lagged_date,"?unitGroup=metric&key=",API_Key_weather,"&include=obs")
    
    r <- GET(url)
    
    api_content <- content(r, as="text")
    
    data <- fromJSON(api_content)
    
    d <- data[["days"]]
    
    a[(days * i - (days-1)):(days * i),] <- d
    
    a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]  # Fill rows with the information form the API
    
    a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
    
    a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
    
  } 
  
  # The variables stations is in the list format. To facilitate further data wrangling, I change to the character format
  
  a$stations <- as.character(a$stations)
  
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
                 "Netherlands", "New Zealand", "Poland", "Peru", "Spain", "Spain", "South Africa", "Turkey", "Unknown", "Unknown", "Unknown", "Unknown")
  
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
                     "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "valencia", "paarl", "antalya")
  
  banana_cities <- c("guayaquil", "medellin", "panama city", "costa rica")
  
  # Both for-loops create the row values for the variables country, vegetables and fruits.
  
  for (i in 1:nrow(locations)) {
    a$country[a$location == locations$val[i]] <- countries[i]
  }
  
  for (i in 1:length(vegetables_cities)) {
    a$vegetables[a$location == vegetables_cities[i]] <- 1
    a$vegetables[a$location ==  banana_cities[i]] <- 0
    a$fruits[a$location == fruits_cities[i]] <- 1
    a$fruits[a$location == banana_cities[i]] <- 1
  }
  
  updated_df <- rbind(a, updated_weather) #updated_df
  
  updated_df <- updated_df %>%
    dplyr::select(location, country, datetime:fruits) %>%
    arrange(location, country, datetime)
  
  fwrite(updated_df, "output/updated_df_weather.csv")
  }  else {
    print("No Update is needed, data is up-to-date")
  }
}

# Retrieve the current date

current_date <- as.Date(Sys.Date())

# Run updating function

update_weather(current_date)