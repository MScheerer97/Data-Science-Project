#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Master File: Update Data Sets #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# This file is used to update our data used to train the machine learning models.
# By running this file, all data sources used in this analysis are updated.

## Update Data ##
source("02_a_Update_Food_Prices_API.R") 
source("02_b_Update_Fuel_Prices.R") 
source("02_c_Update_Inflation_and_Exchange_Rates.R") 
source("02_d_Update_Weather.R") 
source("02_e_Update_Weather_Forecast.R")
source("02_f_Update_Kerosene_and_Crude_Oil_Prices.R") 
source("02_g_Update_World_Container_Index.R") 
source("02_h_Update_Fertilizer_Prices.R") 

## Update Data Preparation ##
source("03_Data_Preparation.R") 
source("04_Data_Preparation_Shiny.R") 

## Update Test Data and Price Predictions ##
source("06_a_Data_Preparation_Test_Data.R") 
source("06_b_Update_Predictions.R") 

## Update Prediction Analysis ##
source("07_Prediction_Analysis.R") 

## Update Food Basket Preparation ##
source("08_a_Prediction_Food_Basket.R") 
source("08_b_Prediction_Price_Recommendation.R") 