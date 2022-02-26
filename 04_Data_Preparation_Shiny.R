#%%%%%%%%%%%%%%%%%%%%%%%%%%%#
## RShiny Data Preparation ##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# For the App, the data needs to be prepared in different formats.
# We generated this file to make the code in the Shiny App file more readable.
# In this App, this file is sourced.


#### Load Data ####

df_pred <- readRDS("output/df_descriptives.rds")
df_basket <- readRDS("output/final_basket.rds")
df_price_basket <- readRDS("output/food_basket_price_predictions.rds")


#### Prices ####

df_price <- df_pred %>%
  dplyr::select(date, food, type, country, size, starts_with("price_")) %>%
  #select(-Price_Index) %>%
  dplyr::rename(
    Total = price_total,
    Berlin = price_B,
    Cologne = price_K,
    Hamburg = price_H,
    Munich = price_M,
    Frankfurt = price_F
  )

df_price <- 
  df_price %>% 
  dplyr::select(Total, Berlin, Hamburg, Frankfurt, Munich, Cologne, date, food, type, country, size) %>%
  gather("market", "price", -date, -food, -type, -country, -size) %>% 
  dplyr::mutate(price = price/100)


#### Info Tab: Nutrition and Food Print ####

## remove comma
df_basket_info <- df_basket %>%
  # drop everything after comma so that nutrition name is shorter
  mutate(
    nutrient = sub(",.+", "", nutrient)
  )  %>%
  # drop everything in bracket; remove also brackets
  mutate(
    nutrient = gsub("\\s*\\([^\\)]+\\)","", nutrient)
  ) 

df_basket_info[df_basket_info$nutrient == "Total lipid", "nutrient"] <- "Fat"


# subset food basket data on nutrition
df_nutrient_info <-
  df_basket_info %>%
  dplyr::select(food_2, nutrient, nutrient_unit, nutrient_value) %>%
  distinct() %>%
  group_by(food_2, nutrient, nutrient_unit) %>%
  # done as bell pepper is differentiated between red, green, and yellow
  dplyr::summarise(
    nutrient_value = round(mean(nutrient_value), 2)
  ) %>%
  ungroup() %>%
  # keep only rows where value is not zero
  filter(nutrient_value != 0)




#### Foot Print ####

df_footprint <- df_basket %>% 
  dplyr::select(food, Carbon_Footprint_open_field, Water_Footprint_open_field) %>%
  distinct() %>%
  dplyr::filter(!food %in% c("Green Bell Pepper", "Red Bell Pepper"))

colnames(df_footprint) <- c("food", "Carbon Footprint", "Water Footprint")

## Obtain Quantiles to create factor variable of the footprints: High - Medium - Low

co2_quantile <- unname(quantile(df_footprint$`Carbon Footprint`)[c("25%", "75%")])
water_quantile <- unname(quantile(df_footprint$`Water Footprint`)[c("25%", "75%")])

df_footprint$`Carbon Footprint Level` <- ifelse(df_footprint$`Carbon Footprint` < co2_quantile[1], "Low", 
                                                ifelse(df_footprint$`Carbon Footprint` > co2_quantile[2], "High", 
                                                "Medium"))

df_footprint$`Water Footprint Level` <- ifelse(df_footprint$`Water Footprint` < water_quantile[1], "Low", 
                                                ifelse(df_footprint$`Water Footprint` > water_quantile[2], "High", 
                                                       "Medium"))

#### Nutrition ####

df_nutrients <- df_basket %>%
  dplyr::select(food, nutrient, nutrient_unit, nutrient_value) %>%
  distinct() %>%
  dplyr::filter(nutrient != "Vitamin D (D2 + D3), International Units")

quantiles_nutrients <- data.frame()

for(k in unique(df_nutrients$nutrient)){
  
  data <- df_nutrients %>%
    dplyr::filter(nutrient == k) %>%
    unique()
  
  quants <- unname(quantile(data$nutrient_value)[c("25%", "75%")])
  
  data$nutrient_level <- ifelse(data$nutrient_value < quants[1], "Low", 
                                ifelse(data$nutrient_value > quants[2], "High", 
                                       "Medium"))
  
  quantiles_nutrients <- rbind(quantiles_nutrients, data)
  
}

# implementing nutrition recommendation

df_nutrition_rec <- readRDS("output/nutrition_recommendation.rds")

## merge to df_nutrients and then completely to df_footprint and add recommendations

df_nutrients <-dplyr::inner_join(df_nutrients, quantiles_nutrients, 
                                  by = c("food", "nutrient", "nutrient_value", 
                                         "nutrient_unit")) %>%
  dplyr::mutate(nutrient_value = round(nutrient_value, 2)) %>%
  dplyr::inner_join(., df_nutrition_rec, by = "nutrient")

df_footprint_nutrients <- dplyr::inner_join(df_footprint, df_nutrients, by = "food") 

df_footprint_nutrients$nutrient <- str_to_title(str_replace_all(str_replace_all(df_footprint_nutrients$nutrient, 
                                                                pattern = "\\,.*$", replacement = ""), 
                                                                "\\s*\\([^\\)]+\\)", ""))

remove_nutrients <- df_footprint_nutrients %>% 
  dplyr::group_by(nutrient) %>% 
  dplyr::summarise(remove_nutrients = sum(nutrient_value) == 0) %>% 
  dplyr::filter(remove_nutrients == TRUE)

df_footprint_nutrients <- df_footprint_nutrients %>% 
  dplyr::filter(!nutrient %in% remove_nutrients$nutrient) 

df_footprint_nutrients[df_footprint_nutrients$nutrient == "Total Lipid", "nutrient"] <- "Fat"


## Price Prediction Basket 
df_price_basket <- df_price_basket %>%
  dplyr::mutate(market = dplyr::recode(market, "price_F" = "Frankfurt", "price_B" = "Berlin", 
                                       "price_K" = "Cologne", "price_M" = "Munich", 
                                       "price_H" = "Hamburg")) 


#### Season ####

df_season <- df_basket %>% 
  dplyr::select(food, country, season_month) %>%
  distinct() %>%
  dplyr::arrange(food, country, season_month)
df_season$season_month <- 
  as.character(lubridate::month(df_season$season_month, label = TRUE, abbr = FALSE))

df_season_compact <- df_season %>%
  dplyr::group_by(food, country) %>%
  dplyr::summarize(season_month_help = list(season_month))

for (i in 1:nrow(df_season_compact)) {
  df_season_compact[i, "Season"] <- 
    paste(df_season_compact$season_month_help[[i]], collapse = ", ")
}

df_season_compact <- df_season_compact %>% 
  dplyr::select(-season_month_help) %>%
  #dplyr::rename(Country = country) %>% 
  ungroup()

# aggregate(season_month ~ food, df_season, c)


#### Exchange rates ####

# extract columns not including lags
colnames_exchange_rate <- colnames(df_pred)[str_detect(
  colnames(df_pred), "exchange_rate_"
)]
colnames_exchange_rate <- colnames_exchange_rate[
  !str_detect(colnames_exchange_rate, "lag")
]

# create data frame
df_exchange_rate <- df_pred %>%
  dplyr::select(date, all_of(colnames_exchange_rate))

# remove inflation_per
colnames(df_exchange_rate) <-
  str_replace_all(colnames(df_exchange_rate), "exchange_rate_", "")
colnames(df_exchange_rate) <-
  str_replace_all(colnames(df_exchange_rate), "_", " ")

# extract country columns
exchange_countries <- colnames(df_exchange_rate)[colnames(df_exchange_rate) != "date"]

# restructure data set
df_exchange_rate <- gather(dplyr::select(df_exchange_rate, "date", all_of(exchange_countries)), 
                       country, exchange, -date)


#### Inflation ####

colnames_inflation_rate <- colnames(df_pred)[str_detect(
  colnames(df_pred), "inflation_perc_"
)]
colnames_inflation_rate <- colnames_inflation_rate[
  !str_detect(colnames_inflation_rate, "lag")
]

# create data frame
df_inflation <- df_pred %>%
  dplyr::select(date, all_of(colnames_inflation_rate))

# remove inflation_per
colnames(df_inflation) <-
  str_replace_all(colnames(df_inflation), "inflation_perc_", "")

# store countries
inflation_countries <- colnames(df_inflation)[colnames(df_inflation) != "date"]

# data per month; drop everything else
df_inflation$month <- month(df_inflation$date)
df_inflation$year <- year(df_inflation$date)
  ## keep only unique values across year-month combinations
df_inflation <- df_inflation[!duplicated(df_inflation[, c("month", "year")]), ]
  ## change date to first of month
df_inflation$date <- floor_date(df_inflation$date, "month")

# inflation rate as numeric
character_columns <- sapply(df_inflation, is.character)  
df_inflation[ , character_columns] <- as.data.frame(   
  apply(df_inflation[ , character_columns], 2, as.numeric))

# restructure data set 
df_inflation <- gather(dplyr::select(df_inflation, "date", all_of(inflation_countries)), 
                       country, inflation, -date)



#### Fuel ####

# extract fuel column names
colnames_fuel <- df_pred %>% dplyr::select(starts_with("fuel_")) %>% colnames()
colnames_fuel <- colnames_fuel[!str_detect(colnames_fuel, "lag")]

# create fuel data frame
df_fuel <- df_pred %>% dplyr::select(date, year, week, all_of(colnames_fuel))

# restructure data set
df_fuel <- gather(dplyr::select(df_fuel, "date", all_of(colnames_fuel)), 
                  variable, fuel, -date)

# from variable extract type of fuel and country
fuel_type_country_list <- str_split(df_fuel$variable, "_")
  ## fuel type is the third element in the list
df_fuel$fuel_type <- sapply(fuel_type_country_list, "[[", 3)
  ## country is the fourth element in the list
df_fuel$country <- sapply(fuel_type_country_list, "[[", 4)
  ## drop variable
df_fuel <- df_fuel %>% dplyr::select(-variable) %>% distinct()


#### Crude Oil ####

# extract crude oil columns
df_oil <- df_pred %>% dplyr::select(date, year, week, Crude_Oil_Price) %>% distinct()


#### Kerosene ####

# create kerosene data frame
df_kerosene <- df_pred %>% dplyr::select(date, year, week, Kerosene_Price) %>% distinct()


#### World Container Index ####

df_wci <- df_pred %>% dplyr::select(date, year, week, wci) %>% distinct()


#### Fertilizer Prices ####




#### Weather ####

# load original weather data frames
df_weather_2000_2014 <- fread("output/weather_2000_2014.csv")
df_weather_2000_2014_banana <- fread("output/weather_banana_2000_2014.csv")
df_weather_2015_j <- fread("output/updated_df_weather.csv")

# combine data frames
df_weather <- 
  rbind(df_weather_2000_2014, df_weather_2000_2014_banana, df_weather_2015_j)

# select variables of interest
df_weather <- df_weather %>%
  dplyr::select(datetime, country, location, tempmax, tempmin, temp, precip,
         windspeed, cloudcover, vegetables, fruits)

# change structure of data frame
df_weather <- 
  gather(dplyr::select(df_weather, "datetime", "country", "temp", "tempmax", "tempmin", 
                "precip", "windspeed", "cloudcover"), 
         weather_type, value, -datetime, -country)



#### Michael Setup Econometric Analysis ####

l <- readRDS("output/df_descriptives.rds")

#f$year <- year(f$date)

# Graph template #

l <- l %>%
  dplyr::mutate(year = as.factor(year),  # 6 levels  # Time Series Data
                week = as.factor(week),  # 53 levels  # Time Series Data
                fruits = as.factor(fruits),  # 2 levels, dummy  
                veggies = as.factor(veggies),  # 2 levels, dummy 
                month = as.factor(month),  # 12 levels  # Time Series Data
                food = as.factor(food),  # 23 levels
                type = as.factor(type),  # 40 levels
                country = as.factor(country),  # 15 levels
                size = as.factor(size),  # 23 levels
                season = as.factor(season),  # 2 levels, dummy
                storage = as.factor(storage),  # 2 levels, dummy
                openland = as.factor(openland),  # 2 levels, dummy
                greenhouse_heated = as.factor(greenhouse_heated),  # 2 levels, dummy
                greenhouse_unheated = as.factor(greenhouse_unheated),
                inflation_perc_Germany = as.numeric(inflation_perc_Germany)# 2 levels, dummy
  )  # More factor variables will come (nutrients, etc. will follow)

# Creating one factor variable of veggies and fruits for data visualization & RShiny

l$food_type <- "Vegetables"
l$food_type <- ifelse(l$fruits==1, "Fruits", "Vegetables")
l$food_type <- as.factor(l$food_type)

# Because the logarithmic function can not handle 0 values, they will be transformed by a small margin e.g. 0.000001.
# This will not significantly change the value but allow an elasticity interpretation

# Writing a function that will replace/add a small integer value, so log-transformations are possible

nums <- unlist(lapply(l, is.numeric)) 
nums <- l[, nums]

# Some factor/dummy variables will be added into the regression equation, therefore we add them back

nums[nums==0] <- 0.00000000000001

nums$date <- l$date
nums$food <- l$food
nums$type <- l$type
nums$size <- l$size
nums$food_type <- l$food_type
nums$country <- l$country

l <- nums %>%
  dplyr::mutate(price_H = price_H / 100,
                price_F = price_F / 100,
                price_B = price_B / 100,
                price_K = price_K / 100,
                price_M = price_M / 100,
                price_total = price_total / 100)

predictor_reg <- l %>%
  dplyr::select(-c(date, food, type, size, flag, film, food_type, country, dplyr::contains("price_"), dplyr::contains("distance_"))) 

#### Michael Setup Predictor Analysis ####

f <- readRDS("output/df_descriptives.rds")

#f$year <- year(f$date)

# Graph template #

f <- f %>%
  dplyr::mutate(year = as.factor(year),  # 6 levels  # Time Series Data
                week = as.factor(week),  # 53 levels  # Time Series Data
                fruits = as.factor(fruits),  # 2 levels, dummy  
                veggies = as.factor(veggies),  # 2 levels, dummy 
                month = as.factor(month),  # 12 levels  # Time Series Data
                food = as.factor(food),  # 23 levels
                type = as.factor(type),  # 40 levels
                country = as.factor(country),  # 15 levels
                size = as.factor(size),  # 23 levels
                season = as.factor(season),  # 2 levels, dummy
                storage = as.factor(storage),  # 2 levels, dummy
                openland = as.factor(openland),  # 2 levels, dummy
                greenhouse_heated = as.factor(greenhouse_heated),  # 2 levels, dummy
                greenhouse_unheated = as.factor(greenhouse_unheated),
                inflation_perc_Germany = as.numeric(inflation_perc_Germany)# 2 levels, dummy
  )  

f$food_type <- "Vegetables"
f$food_type <- ifelse(f$fruits==1, "Fruits", "Vegetables")
f$food_type <- as.factor(f$food_type)

f <- f %>%
  dplyr::mutate(price_H = price_H / 100,
                price_F = price_F / 100,
                price_B = price_B / 100,
                price_K = price_K / 100,
                price_M = price_M / 100,
                price_total = price_total / 100)

g <- f %>%
  dplyr::select(date, country, year, week, month, season, storage, contains('lag_2_weeks'), inflation_perc_Germany, Kerosene_Price,
                Crude_Oil_Price, wci, DAP, TSP, Urea, Phosphate_Rock, Potassium_chloride, exchange_rate_South_Africa, exchange_rate_Turkey,
                exchange_rate_New_Zealand)

choice_price_vec <- c("Price in Hamburg" = "price_H",
                      "Price in Munich" = "price_M",
                      "Price in Berlin" = "price_B",
                      "Price in Cologne" = "price_K",
                      "Price in Frankfurt" = "price_F")

choice_predictor_vec <- c("World Container Index in EURO/KG" = "wci",
                          "Crude Oil Price in EURO/L" = "Crude_Oil_Price",
                          "Kerosene Price in EURO/L" = "Kerosene_Price",
                          "Diesel Fuel Price in Germany in EURO/L" = "fuel_price_diesel_Germany",
                          "Triple superphosphate in EURO/MT" = "TSP",
                          "Di-ammonium Phosphate in EURO/MT" = "DAP",
                          "Urea Fertilizer in EURO/MT" = "Urea",
                          "Phosphorite in EURO/MT" = "Phosphate_Rock",
                          "Potassium Chloride in EURO/MT" = "Potassium_chloride",
                          "Exchange Rate South Africa in South African Rand/EURO" = "exchange_rate_South_Africa",
                          "Exchange Rate Turkey in Turkish Lira/EURO" = "exchange_rate_Turkey",
                          "Exchange Rate New Zealand in New Zealand Dollar/EURO" = "exchange_rate_New_Zealand",
                          "Inflation in Germany in %" = "inflation_perc_Germany")

choice_weather_easy_vec <- c("Temperature in Celsius" = "lag_2_weeks_temp",
                             "Humidity in %" = "lag_2_weeks_humidity",
                             "Cloud Cover in Oktas" = "lag_2_weeks_cloudcover",
                             "Precipitation in mm" = "lag_2_weeks_precip",
                             "Wind Speed in m/s" = "lag_2_weeks_windspeed",
                             "Deviation from normal Temperature in Celsius" = "lag_2_weeks_dev_temp",
                             "Deviation from normal Humidity in %" = "lag_2_weeks_dev_humidity",
                             "Deviation from normal Cloud Cover in Oktas" = "lag_2_weeks_dev_cloudcover",
                             "Deviation from normal Precipitation in mm" = "lag_2_weeks_dev_precip",
                             "Deviation from normal Wind Speeds in m/s" = "lag_2_weeks_dev_windspeed")

#### Michael Setup Price Recommendation Analysis ####

price_recommendation_df <- readRDS("output/food_basket_price_recommendation.rds")

#past_prices <- readRDS("output/final_ML.rds")

#past_prices <- past_prices %>%
#select(date, food, type, size, country, price_F, price_H, price_M, price_K, price_B)

#past_prices$year <- year(past_prices$date)

# Data Preparation # 

future_price_recommendation <- price_recommendation_df %>%
  dplyr::group_by(market, food, type, size, country) %>%
  dplyr::filter(`predicted price` == min(`predicted price`)) %>%
  dplyr::arrange(market, type) #%>%
#group_by(market, food, type, size, country) %>%
#summarize(`predicted price` = mean(`predicted price`))

unique_recommendation_dates <- rev(unique(future_price_recommendation$date))

future_price_recommendation$date <- as.character(future_price_recommendation$date)

for(i in 1:length(unique_recommendation_dates)) {
  future_price_recommendation$date[future_price_recommendation$date==as.character(unique_recommendation_dates[i])] <- paste("In", i, "weeks")
}

future_price_recommendation$date[future_price_recommendation$date=="In 1 weeks"] <- "In 1 week"

#future_price_recommendation$date <- as.character(future_price_recommendation$date)

#future_price_recommendation$date[future_price_recommendation$date=="2022-01-24"] <- "In 1 week"
#future_price_recommendation$date[future_price_recommendation$date=="2022-01-31"] <- "In 2 weeks"
#future_price_recommendation$date[future_price_recommendation$date=="2022-02-07"] <- "In 3 weeks"
#future_price_recommendation$date[future_price_recommendation$date=="2022-02-14"] <- "In 4 weeks"

colnames(future_price_recommendation)[1] <- "Best_time_to_buy"

future_price_recommendation$`predicted price` <- paste(future_price_recommendation$`predicted price`, " EURO/KG", sep="")

#### Michael Setup Past Prices Comparison ####

price_past_recommendation <- readRDS("output/food_basket_price_recommendation.rds")

new_prediction_dates <- unique(price_past_recommendation$date)

# Data Preparation # 

unique_recommendation_dates_s <- unique(price_past_recommendation$date)

price_past_recommendation$date <- as.character(price_past_recommendation$date)

for(i in 1:length(unique_recommendation_dates_s)) {
  price_past_recommendation$date[price_past_recommendation$date==as.character(unique_recommendation_dates_s[i])] <- paste0("Week_", i)
}

price_past_recommendation <- tidyr::spread(price_past_recommendation, key="date", value=`predicted price`)

##################

past_prices <- readRDS("output/final_ML.rds")

past_prices <- past_prices %>%
  dplyr::select(date, food, type, size, country, price_F, price_H, price_M, price_K, price_B)

past_prices$year <- year(past_prices$date)


## Prices over all periods

overall_average <- past_prices %>%
  dplyr::group_by(food, type, size, country) %>%
  dplyr::summarize(price_F = mean(price_F)/100,
            price_H = mean(price_H)/100,
            price_M = mean(price_M)/100,
            price_K = mean(price_K)/100,
            price_B = mean(price_B)/100) %>%
  tidyr::gather(`price_H`, `price_F`, `price_B`, `price_K`, `price_M`,  key = "market", value = "overall_price")%>%
  dplyr::arrange(food, type, size, country, market)

## Prices in last year #

# First date to start the loop


max_date_past_prices <- max(past_prices$date)

# Start data frame for binding in loop

filter_date <- new_prediction_dates[1] - 365 - 7

last_year_average <- past_prices %>%
  dplyr::filter(date > filter_date & date < max_date_past_prices)%>%
  dplyr::group_by(food, type, size, country) %>%
  dplyr::summarize(price_F = mean(price_F)/100,
            price_H = mean(price_H)/100,
            price_M = mean(price_M)/100,
            price_K = mean(price_K)/100,
            price_B = mean(price_B)/100) %>%
  tidyr::gather(`price_H`, `price_F`, `price_B`, `price_K`, `price_M`,  key = "market", value = "overall_price")%>%
  dplyr::arrange(food, type, size, country, market)

#new_var_name <- paste0(new_prediction_dates[1],"_1_year_average") 

colnames(last_year_average)[6] <- "Week_1_1_year"

### all other dates in loop

for(i in 2:length(new_prediction_dates)) {
  filter_date <- new_prediction_dates[i] - 365 - 7
  
  last_year_average_loop <- past_prices %>%
    dplyr::filter(date > filter_date & date < max_date_past_prices)%>%
    dplyr::group_by(food, type, size, country) %>%
    dplyr::summarize(price_F = mean(price_F)/100,
              price_H = mean(price_H)/100,
              price_M = mean(price_M)/100,
              price_K = mean(price_K)/100,
              price_B = mean(price_B)/100) %>%
    tidyr::gather(`price_H`, `price_F`, `price_B`, `price_K`, `price_M`,  key = "market", value = "overall_price")%>%
    dplyr::arrange(food, type, size, country, market)
  
  new_var_name <- paste0("Week_",i,"_1_year") 
  
  colnames(last_year_average_loop)[6] <- new_var_name
  
  last_year_average <- last_year_average %>%
    dplyr::left_join(last_year_average_loop, by=c("food", "type", "size", "country", "market"))
  
}

filter_date <- new_prediction_dates[1] - 30 - 7

last_month_average <- past_prices %>%
  dplyr::filter(date > filter_date & date < max_date_past_prices)%>%
  dplyr::group_by(food, type, size, country) %>%
  dplyr::summarize(price_F = mean(price_F)/100,
            price_H = mean(price_H)/100,
            price_M = mean(price_M)/100,
            price_K = mean(price_K)/100,
            price_B = mean(price_B)/100) %>%
  tidyr::gather(`price_H`, `price_F`, `price_B`, `price_K`, `price_M`,  key = "market", value = "overall_price")%>%
  dplyr::arrange(food, type, size, country, market)

colnames(last_month_average)[6] <- "Week_1_1_month"

for(i in 2:length(new_prediction_dates)) {
  filter_date <- new_prediction_dates[i] - 30 - 7
  
  last_month_average_loop <- past_prices %>%
    dplyr::filter(date > filter_date & date < max_date_past_prices)%>%
    dplyr::group_by(food, type, size, country) %>%
    dplyr::summarize(price_F = mean(price_F)/100,
              price_H = mean(price_H)/100,
              price_M = mean(price_M)/100,
              price_K = mean(price_K)/100,
              price_B = mean(price_B)/100) %>%
    tidyr::gather(`price_H`, `price_F`, `price_B`, `price_K`, `price_M`,  key = "market", value = "overall_price")%>%
    dplyr::arrange(food, type, size, country, market)
  
  new_var_name <- paste0("Week_",i,"_1_month") 
  
  colnames(last_month_average_loop)[6] <- new_var_name
  
  last_month_average <- last_month_average %>%
    dplyr::left_join(last_month_average_loop, by=c("food", "type", "size", "country", "market"))
  
}


###

past_prices_rates <- price_past_recommendation %>%
  dplyr::left_join(last_year_average, by=c("food", "type", "size", "country", "market")) %>%
  dplyr::left_join(last_month_average, by=c("food", "type", "size", "country","market")) %>%
  dplyr::mutate(Week_1_Rate_1_year = round(((Week_1 - Week_1_1_year) / Week_1_1_year)*100, 2),
         Week_2_Rate_1_year = round(((Week_2 - Week_2_1_year) / Week_2_1_year)*100, 2),
         Week_3_Rate_1_year = round(((Week_3 - Week_3_1_year) / Week_3_1_year)*100, 2),
         Week_4_Rate_1_year = round(((Week_4 - Week_4_1_year) / Week_4_1_year)*100, 2),
         Week_1_Rate_1_month = round(((Week_1 - Week_1_1_month) / Week_1_1_month)*100, 2),
         Week_2_Rate_1_month = round(((Week_1 - Week_2_1_month) / Week_2_1_month)*100, 2),
         Week_3_Rate_1_month = round(((Week_1 - Week_3_1_month) / Week_3_1_month)*100, 2),
         Week_4_Rate_1_month = round(((Week_1 - Week_4_1_month) / Week_4_1_month)*100, 2))

past_prices_rates <- past_prices_rates%>%
  dplyr::mutate(  
    food = as.factor(food),  
    type = as.factor(type),  
    country = as.factor(country),  
    size = as.factor(size))
