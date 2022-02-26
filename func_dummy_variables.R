#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Generating Dummy Variables #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


## by Lana Kern ##


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

## Content of File ##

# This file includes a function that generates the dummy variables.
# This function is needed to create the test data set.
# Inputs:
  ## food_sel: selected fruit/vegetable
  ## type_sel: selected fruit/vegetable type
  ## size_sel: selected size of the selected fruit/vegetable
  ## country_sel: selected country where the fruit/vegetable originates from

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#### FUNCTION: GENERATE DUMMIES ####

# func_generate_dummies
func_dummy_variables <- function(food_sel, type_sel, size_sel, country_sel) {
  
  # load packages
  # library(fastDummies)
  # library(dplyr)
  
  # load machine learning data frame
  df_ML_raw <- readRDS("output/final_food_ML.rds")
  
  # filter food; keep pnly food, type, size and country
  df_ML_food <- df_ML_raw %>% 
    filter(food == food_sel) %>%
    dplyr::select(food, type, size, country) %>%
    distinct()
  
  df_ML_food <- df_ML_food %>% mutate_if(is.factor, as.character)
  
  # generate dummy variables
  ## to do so factor variable is needed
  df_ML_food[, c("type", "country", "size")] <- 
    lapply(df_ML_food[, c("type", "country", "size")], as.factor)
  
  df_ML_food <- dummy_cols(
    df_ML_food,
    remove_first_dummy = FALSE,
    remove_selected_columns = FALSE, 
    select_columns = c("type", "country", "size")
  )
  
  # select dummy variables for food-type-size-country combination 
  df_dummies <- df_ML_food %>%
    filter(type == type_sel, size == size_sel, country == country_sel) %>%
    dplyr::select(-c(type, size, country))
  
  # df_dummies is the output
  return(df_dummies)
}