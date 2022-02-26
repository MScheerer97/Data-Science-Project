### Historical weather data extraction with the Visual Crossing API ###

rm(list=ls())

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

source("API_Key_Weather.R")

# Create a data frame with a list of cities for which we want the weather information

locations <- data.frame(val=c("general roca","gent","santiago de chile", "toulouse", "rennes", "cologne", "stuttgart", "sparta", "kissamos", "alexandria", "bari", "catania",
                              "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "valencia", "paarl", "antalya"))

#### Generate weather data from 01.01.2000 - 31.12.2014  #### 
#%%%%%%%%%%%%%%%%%%%# 

date_begin <- as.Date("2000-01-01")
date_end <- as.Date("2014-12-31")

# I create an empty data frame with the dimensions that we will be filled with the weather data

days <- as.numeric(date_end - date_begin) + 1

a <- as.data.frame(matrix(0, ncol=35+3, nrow=(nrow(locations)*days)))

# The columns names were not manually retrieved but with the code from line 146. This was done from a previous data set

# paste("'",as.character(c),"'",collapse=", ",sep="")

column_names <- c('datetime', 'datetimeEpoch', 'tempmax', 'tempmin', 'temp', 'feelslikemax', 'feelslikemin', 'feelslike', 'dew', 'humidity', 'precip', 'precipprob', 'precipcover', 'preciptype', 'snow', 'snowdepth', 'windgust', 'windspeed', 'winddir', 'pressure', 'cloudcover', 'visibility', 'solarradiation', 'solarenergy', 'uvindex', 'sunrise', 'sunriseEpoch', 'sunset', 'sunsetEpoch', 'moonphase', 'conditions', 'description', 'icon', 'stations', 'source', 'location','latitude','longitude')

colnames(a) <- column_names

# Now the 1. for-loop retrieves data from the Visual Crossing API. This API, although not free, is relatively cheap and easy to use. Additionally, it includes all relevant weather
# variables for numerous cities across the globe

for(i in 1:nrow(locations)) {
  
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",date_begin,"/",date_end,"?unitGroup=metric&key=",API_Key_weather,"&include=obs")
  
  r <- GET(url)
  
  api_content <- content(r, as="text")
  
  data <- fromJSON(api_content)
  
  d <- data[["days"]]
  
  a[(days * i - (days-1)):(days * i),] <- d
  
  a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]
  
  a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
  
  a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
  
} 

# The variables stations is in a list format. To facilitate further data wrangling, I change to the character format

a$stations <- as.character(a$stations)
a$datetime <- as.Date(a$datetime)

# To merge the weather data with the main data frame, we need unique IDs. To uniquely identify the rows with the master data set, I define three new variables: country variable
# with the country name, and two variables that define weather the weather information belongs to fruits or vegetables.

a$country <- 0

a$vegetables <- 0

a$fruits <- 0

countries <- c("Argentina", "Belgium", "Chile", "France", "France", "Germany", "Germany", "Greece", "Greece", "Egypt", "Italy", "Italy", "Morocco",
               "Netherlands", "New Zealand", "Poland", "Peru", "Spain", "Spain", "South Africa", "Turkey")

# As the exact weather location of the vegetables and fruits is not known, the data scientist muss infer the original locations. Here, this is done by researching trough the internet
# and the website "europages.com". Here, the methodological assumption is that we infer the original location by looking manually at each countries suppliers and growing areas.
# The city with the most fruit and vegetables suppliers is chosen as the most likely original location. Of course, this is an assumption and the real original location might me completely
# different. In addition, some countries have growing areas distributed across the whole country, so the city that is in the center is chosen. Of course, this has several
# data restrictions and shortcomings. However, this is the most accurate method to retrieve the original locations. Without it, weather aggregations fro the whole country would
# have to be used. This method would severely ias the weather data for countries with different weather zones e.g. italy.

# Some cities are assumed to be the main location for both vegetables and fruits. However, some cities are assumed to be specific for vegetables and vice versa.
# Thereby, numerous cities will be inclued for both variables and only differ in some specific countries.

vegetables_cities <- c("general roca","gent","santiago de chile", "rennes", "cologne", "kissamos", "alexandria", "bari",
                       "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "paarl", "antalya")

fruits_cities <- c("general roca","gent","santiago de chile", "toulouse","stuttgart", "sparta", "alexandria", "catania",
                   "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "valencia", "paarl", "antalya")

# Both for-loops create the row values for the variables country, vegetables and fruits.

for (i in 1:nrow(locations)) {
  a$country[a$location == locations$val[i]] <- countries[i]
}

for (i in 1:length(vegetables_cities)) {
  a$vegetables[a$location == vegetables_cities[i]] <- 1
  a$fruits[a$location == fruits_cities[i]] <- 1
}

# Make the data more "tidy"

a <- a %>%
  select(location, country, datetime:fruits) %>%
  arrange(location, country, datetime)

# Export the retrieved data

fwrite(a, "output/weather_2000_2014.csv")

#### Generate weather data from 01.01.2015 - 31.10.2021  ####
#%%%%%%%%%%%%%%%%%%%# 

date_begin <- as.Date("2015-01-01")
date_end <- as.Date("2021-10-31")

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
  
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",date_begin,"/",date_end,"?unitGroup=metric&key=",API_Key_weather,"&include=obs")
  
  r <- GET(url)
  
  api_content <- content(r, as="text")
  
  data <- fromJSON(api_content)
  
  d <- data[["days"]]
  
  a[(days * i - (days-1)):(days * i),] <- d
  
  a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]
  
  a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
  
  a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
  
} 

# The variables stations is in a list format. To facilitate further data wrangling, I change to the character format

a$stations <- as.character(a$stations)
a$datetime <- as.Date(a$datetime)

# To merge the weather data with the main data frame, we need unique IDs. To uniquely identify the rows with the master data set, I define three new variables: country variable
# with the country name, and two variables that define weather the weather information belongs to fruits or vegetables.

a$country <- 0

a$vegetables <- 0

a$fruits <- 0

countries <- c("Argentina", "Belgium", "Chile", "France", "France", "Germany", "Germany", "Greece", "Greece", "Egypt", "Italy", "Italy", "Morocco",
               "Netherlands", "New Zealand", "Poland", "Peru", "Spain", "Spain", "South Africa", "Turkey")

# As the exact weather location of the vegetables and fruits is not known, the data scientist muss infer the original locations. Here, this is done by researching trough the internet
# and the website "europages.com". Here, the methodological assumption is that we infer the original location by looking manually at each countries suppliers and growing areas.
# The city with the most fruit and vegetables suppliers is chosen as the most likely original location. Of course, this is an assumption and the real original location might me completely
# different. In addition, some countries have growing areas distributed across the whole country, so the city that is in the center is chosen. Of course, this has several
# data restrictions and shortcomings. However, this is the most accurate method to retrieve the original locations. Without it, weather aggregations fro the whole country would
# have to be used. This method would severely ias the weather data for countries with different weather zones e.g. italy.

# Some cities are assumed to be the main location for both vegetables and fruits. However, some cities are assumed to be specific for vegetables and vice versa.
# Thereby, numerous cities will be inclued for both variables and only differ in some specific countries.

vegetables_cities <- c("general roca","gent","santiago de chile", "rennes", "cologne", "kissamos", "alexandria", "bari",
                       "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "murcia", "paarl", "antalya")

fruits_cities <- c("general roca","gent","santiago de chile", "toulouse","stuttgart", "sparta", "alexandria", "catania",
                   "casablanca", "rotterdam", "napier", "warsaw", "huanuco", "valencia", "paarl", "antalya")

# Both for-loops create the row values for the variables country, vegetables and fruits.

for (i in 1:nrow(locations)) {
  a$country[a$location == locations$val[i]] <- countries[i]
}

for (i in 1:length(vegetables_cities)) {
  a$vegetables[a$location == vegetables_cities[i]] <- 1
  a$fruits[a$location == fruits_cities[i]] <- 1
}

# Make the data more "tidy"

a <- a %>%
  select(location, country, datetime:fruits) %>%
  arrange(location, country, datetime)

# Export the retrieved data

write.csv(a, "output/first_df_weather.csv")

#### Generate weather data for Ecuador, Columbia, Panama, Costa Rica from 01.01.2000 - 31.10.2021  ####
#%%%%%%%%%%%%%%%%%%%# 

# As we need four new countries and their respective weather, we need another block of code that generates this data.
# Sadly, we can not manually add those countries in the previous code, as it will retrieve weather data again for all locations

# At the beginning of the project, it was assumed that only the previous cities are relevant and the functions
# were programmed accordingly to the first goal

# However, small changes and little time is needed to get the needed data. After the csv file, the updating R scirpot only
# needs minor changes to run the previous cities and n´the new locations

locations <- data.frame(val=c("guayaquil","medellin","panama city","costa rica"))

## Generate weather data from 01.01.2000 - 31.12.2014  ## 

date_begin <- as.Date("2000-01-01")
date_end <- as.Date("2021-12-06")

# I create an empty data frame with the dimensions that we will be filled with the weather data

days <- as.numeric(date_end - date_begin) + 1

a <- as.data.frame(matrix(0, ncol=35+3, nrow=(nrow(locations)*days)))

# The columns names were not manually retrieved but with the code from line 146. This was done from a previous data set

# paste("'",as.character(c),"'",collapse=", ",sep="")

column_names <- c('datetime', 'datetimeEpoch', 'tempmax', 'tempmin', 'temp', 'feelslikemax', 'feelslikemin', 'feelslike', 'dew', 'humidity', 'precip', 'precipprob', 'precipcover', 'preciptype', 'snow', 'snowdepth', 'windgust', 'windspeed', 'winddir', 'pressure', 'cloudcover', 'visibility', 'solarradiation', 'solarenergy', 'uvindex', 'sunrise', 'sunriseEpoch', 'sunset', 'sunsetEpoch', 'moonphase', 'conditions', 'description', 'icon', 'stations', 'source', 'location','latitude','longitude')

colnames(a) <- column_names

# Now the 1. for-loop retrieves data from the Visual Crossing API. This API, although not free, is relatively cheap and easy to use. Additionally, it includes all relevant weather
# variables for numerous cities across the globe

for(i in 1:nrow(locations)) {
  
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",locations[i,],"/",date_begin,"/",date_end,"?unitGroup=metric&key=",API_Key_weather,"&include=obs")
  
  r <- GET(url)
  
  api_content <- content(r, as="text")
  
  data <- fromJSON(api_content)
  
  d <- data[["days"]]
  
  a[(days * i - (days-1)):(days * i),] <- d
  
  a$location[(days * i - (days-1)):(days * i)] <- data[["address"]]
  
  a$latitude[(days * i - (days-1)):(days * i)] <- data[["latitude"]]
  
  a$longitude[(days * i - (days-1)):(days * i)] <- data[["longitude"]]
  
} 

# The variables stations is in a list format. To facilitate further data wrangling, I change to the character format

a$stations <- as.character(a$stations)
a$datetime <- as.Date(a$datetime)

# To merge the weather data with the main data frame, we need unique IDs. To uniquely identify the rows with the master data set, I define three new variables: country variable
# with the country name, and two variables that define weather the weather information belongs to fruits or vegetables.

a$country <- 0

a$vegetables <- 0

a$fruits <- 0

countries <- rep("unknown", 4)

# As the exact weather location of the vegetables and fruits is not known, the data scientist muss infer the original locations. Here, this is done by researching trough the internet
# and the website "europages.com". Here, the methodological assumption is that we infer the original location by looking manually at each countries suppliers and growing areas.
# The city with the most fruit and vegetables suppliers is chosen as the most likely original location. Of course, this is an assumption and the real original location might me completely
# different. In addition, some countries have growing areas distributed across the whole country, so the city that is in the center is chosen. Of course, this has several
# data restrictions and shortcomings. However, this is the most accurate method to retrieve the original locations. Without it, weather aggregations fro the whole country would
# have to be used. This method would severely ias the weather data for countries with different weather zones e.g. italy.

# Some cities are assumed to be the main location for both vegetables and fruits. However, some cities are assumed to be specific for vegetables and vice versa.
# Thereby, numerous cities will be inclued for both variables and only differ in some specific countries.

vegetables_cities <- c("guayaquil","medellin","panama city","costa rica")

fruits_cities <- c("guayaquil","medellin","panama city","costa rica")

# Both for-loops create the row values for the variables country, vegetables and fruits.

for (i in 1:nrow(locations)) {
  a$country[a$location == locations$val[i]] <- countries[i]
}

for (i in 1:length(vegetables_cities)) {
  a$vegetables[a$location == vegetables_cities[i]] <- 0
  a$fruits[a$location == fruits_cities[i]] <- 1
}

# Make the data more "tidy"

a <- a %>%
  select(location, country, datetime:fruits) %>%
  arrange(location, country, datetime)

# Export the retrieved data

fwrite(a, "output/weather_banana_2000_2021.csv")

### Split the retrived data into two datasets: one dataset for the price prediction and one for the 
### historical weather normals

banana_weather <- fread("output/weather_banana_2000_2021.csv")

weather_2000_2014_banana <- banana_weather %>%
  filter(datetime < "2015-01-01")

weather_2015_2021_banana <- banana_weather %>%
  filter(datetime >= "2015-01-01" & datetime <= "2021-10-31")

fwrite(weather_2000_2014_banana, "output/weather_banana_2000_2014.csv")
fwrite(weather_2015_2021_banana, "output/weather_banana_2015_2021.csv")
