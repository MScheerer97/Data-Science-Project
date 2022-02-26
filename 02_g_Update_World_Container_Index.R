#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Update World Container Index ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### General Setup & Package loading ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

rm(list=ls())

if (!require("Hmisc")) install.packages("Hmisc") 
if (!require("lubridate")) install.packages("lubridate") 
if (!require("tidyverse")) install.packages("tidyverse") 
if (!require("data.table")) install.packages("data.table") 
if (!require("imputeTS")) install.packages("imputeTS")

library(Hmisc)
library(lubridate)
library(tidyverse)
library(data.table)
library(imputeTS)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Prepare data for updating function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# Load world container index data 

world_container_index <- fread("output/world_container_index_without_lags.csv")

# As explained in file 01_h, this data is retrieved by manually typing in the values from a graph. To supply our project with
# data after the project's ending, a function that imputes new weekly imputed world container index values is needed

last_date <- max(world_container_index$date)

# Retrieve the current date

current_date <- Sys.Date() 

# In order to update futures, we must first calculate whether updating is actually needed
# If the number of days is less than 7 days, updating will not be necessary

diff_days <- as.numeric(difftime(current_date, last_date, units="days"))

diff_weeks <- floor(diff_days / 7) 

new_rows <- last_date 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Updating function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

update_wci <- function(diff_weeks) {  # Takes the week difference between the last update in the data frame and the current date
  if(diff_weeks != 0) {  # If the difference is larger or equal 7 days, or 1 week, updates are needed
    for(j in 1:diff_weeks) {  # For the number of weeks that are missing in the current data frame, create new rows
      new_dates <- last_date + 7*j
      new_rows <- as.data.frame(rbind(new_dates, new_rows))
    }
    
    
    for(j in 1:nrow(new_rows)) {  # By using rbind() dates are converted back to numeric. In order to bring them back to date format, "origin" must be used
      new_rows$V1 <- as.Date(new_rows$V1, origin="1970-01-01")
      new_rows$wci <- NA  # Also to merge with the old data frame, the number of columns must be equal
    }
    
    colnames(new_rows)[1] <- "date"  # Create same column names to interoperability 
    
    new_rows <- new_rows %>%  # Filter out the last date value to avoid duplicates
      dplyr::filter(date!=last_date)
    
    world_container_index <- rbind(new_rows, world_container_index)  # Rbind with "old" data frame
    
    rownames(world_container_index) <- 1:nrow(world_container_index)  # Set new index names for avoid confusion
    
    world_container_index <- world_container_index %>%
      dplyr::arrange(date)
    
    # Impute new weeks, as manual typing in of values will not be done after the project's end
    
    world_container_index <- imputeTS::na_ma(world_container_index, k=10, weighting = "simple")
    
    #return(world_container_index)
    
  } else {
    print("No Update is needed!")
    return(world_container_index)
  }
}

# Update/add new rows

world_container_index <- update_wci(diff_weeks)  # Apply function

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
#### Create lags ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

world_container_index$week <- strftime(world_container_index$date, format = "%V")
world_container_index$year <- year(world_container_index$date)

# Creating variable lags

world_container_index$wci_2_weeks <- Lag(world_container_index$wci, +2)
world_container_index$wci_1_month <- Lag(world_container_index$wci, +4)

world_container_index <- world_container_index %>%
  dplyr::filter(year != 2015) %>%
  dplyr::mutate(week = as.factor(week)) %>%
  dplyr::select(week, year, wci, wci_2_weeks, wci_1_month) 

## Export data frame for merging with main data set ##

fwrite(world_container_index, "output/world_container_index.csv")



