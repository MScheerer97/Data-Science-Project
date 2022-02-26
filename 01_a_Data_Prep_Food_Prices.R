#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
# Data Preparation of Food Prices #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

# basis food price data until November 2021 #

## by Lana Kern ##

#### Load packages ####

# install (if necessary) and load required packages
if (!require("pdftools")) install.packages("pdftools") 
library(pdftools)

if (!require("pdfsearch")) install.packages("pdfsearch") 
library(pdfsearch)

if (!require("pdftables")) install.packages("pdftables") 
library(pdftables)

if (!require("tabulizer")) install.packages("tabulizer") 
library(tabulizer)

if (!require("tidyverse")) install.packages("tidyverse") 
library(tidyverse)

if (!require("stringr")) install.packages("stringr") 
library(stringr)


#### General Setup ####


# create PDF file path
  ## flexible is year and calender week 
year <- c("2019", "2020")
#year <- "2019"
calender_week <- str_pad(2:52, 2, pad = "0")

# text to drop in documents
text_to_drop <- c("München", "Erzeugnis", "Köln", "Hamburg", "Frankfurt", "Berlin", 
                  "2018", "2019", "2020", "2021",
                  "Größe", "Land", "Erzeugnis Land")
for (i in 1:52) {
  drop_calender_week <- paste0(i, ". KW")
  text_to_drop <- c(text_to_drop, drop_calender_week)
}

# define fruits and vegetables
fruits <- c("Äpfel", "Birnen", "Kiwis")

# define a function which checks if file path exists
# this is necessary since sometimes the path is different or the
# document is not uploaded
func_file_exists <- function(path){
  # establish connection with file path
  file_path <- url(path)
  # check if connection can be opened
  check_con <- 
    suppressWarnings(try(open.connection(file_path, open = "rt", timeout = 2),
                         silent = TRUE)[1])
  # close connection
  suppressWarnings(try(close.connection(file_path), silent = TRUE))
  # if check_con is NULL, file exists, otherwise it does not exist
  # check_con includes error text if file does not exist
  ifelse(is.null(check_con),TRUE,FALSE)
}


#### First page ####

df_final_1 <- data.frame()

for (j in year) {
  for (i in calender_week) {
    #p <- p + 1
    fruit_file_path <- paste0("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/",
                              j, "/Wochenbericht_", j,  "_", i, 
                              ".pdf;jsessionid=548DBD3A2408AD46E2B585AFF1B1E66D.1_cid325?__blob=publicationFile&v=2")
    
    # check if file exists
    # if file does not exists, i.e., function returns FALSE, we skip this file
    # and store year and calender week in a data frame
    df_file_does_not_exist <- data.frame()
    if (func_file_exists(fruit_file_path) == FALSE) {
      ## save file in data frame 
      df_file_does_not_exist <- rbind(df_file_does_not_exist, data.frame(j, i))
      ## and skip file
      next 
    }
    
    # check if reading in PDF file creates an error message
    result <- tryCatch({
      ## read in pdf and suppress warning
      suppressWarnings(test <- pdf_text(fruit_file_path))
      ## empty function
    }, error=function(e){})
    # if file returns error message, skip it
    if (!is.null(result)) {
      next
    }
    
    # read in whole pdf document as text to identify page where report starts
    ## read in document
    fruit_text <- pdf_text(fruit_file_path)
    ## identify page
    start_page <- which(str_detect(fruit_text, "Preisbericht"))[1]
    ## if page does not exist, skip document
    if (is.na(start_page)) {
      next
    }
    
    
    table1 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(307, 15, 815, 244)), # 287 to include Größe, 21 
      # define page
      pages = start_page,
      # Let R decide which method to use
      method = "decide",
      # output should be a matrix
      # chossing data frame may use first row as column
      output = "matrix"
    )
    df_table1 <- data.frame(table1[[1]])
    
    
    # first row determines kind of fruit
    df_table1$fruit <- df_table1[1,1]
    df_table1 <- df_table1[-1, ]
    
    # drop columns containing only missing values
    ## may be superfluous
    df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
    
    if (ncol(df_table1) != 3) {
      next
    }
    
    # rename columns
    colnames(df_table1) <- c("type", "country", "fruit")
    
    
    table2 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(326, 243, 816, 290)), 
      # define page
      pages = start_page,
      # Let R decide which method to use
      method = "decide",
      # output should be a matrix
      # chossing data frame may use first row as column
      output = "matrix"
    )
    df_table2 <- data.frame(table2[[1]]) 
    if (df_table2[1, ] == "Größe") {
      df_table2 <- data.frame(df_table2[-1, ])
    }
    
    if (ncol(df_table2) != 1) {
      next
    }
    
    colnames(df_table2) <- "size" 
    
    
    table3 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(307, 389, 815, 573)), # 329
      # define page
      pages = start_page,
      # Let R decide which method to use
      method = "decide",
      # output should be a matrix
      # choosing data frame uses the first row as column names
      output = "matrix"
    )
    df_table3 <- data.frame(table3[[1]])
    
    # rename columns
    if (ncol(df_table3) != 6) {
      next
    }
    
    colnames(df_table3) <- 
      c("price_total", "price_F", "price_H", "price_K", "price_M", "price_B")
    
    
    # combine both tables
    ## establish next statement: if something goes wrong, just skip current calender week
    ## and move on with next
    if (nrow(df_table1) < 5 | nrow(df_table2) < 5 | nrow(df_table3) < 5) {
      next
    }
    
    df_table <- cbind(df_table1, df_table2, df_table3)
    
    ## add year and calender week
    df_table$year <- j
    df_table$week <- i
    
    ## add page indicator
    df_table$page <- 1
    
    ## add empty indicator
    df_table$indicator <- df_table$indicator <- seq(1:nrow(df_table))
    
    # add to final data frame
    df_final_1 <- rbind(df_final_1, df_table)
  }
}





#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Second page ####

df_final_2 <- data.frame()

for (i in calender_week) {
  # define file path
  fruit_file_path <- paste0("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/",
                            year, "/Wochenbericht_", year,  "_", i, 
                            ".pdf;jsessionid=548DBD3A2408AD46E2B585AFF1B1E66D.1_cid325?__blob=publicationFile&v=2")
  
  
  # check if file exists
  # if file does not exists, i.e., function returns FALSE, we skip this file
  if (func_file_exists(fruit_file_path) == FALSE) {
    ## skip file
    next 
  }
  
  # read in whole pdf document as text to identify page where report starts
    ## read in document
  fruit_text <- pdf_text(fruit_file_path)
    ## identify page
  start_page <- 
    which(str_detect(fruit_text, "Preisbericht"))[length(which(str_detect(fruit_text, "Preisbericht")))]
    ## if page does not exist, skip document
  if (length(start_page) == 0) {
    next
  }
  
  
  table1 <- extract_tables(
    # define file path
    fruit_file_path, 
    # specify are where table is (skip, e.g., explanation)
    # guess = FALSE is necessary that area is recognized
    guess = FALSE,
    area = list(c(54, 12, 823, 588)), # list(c(58, 12, 810, 247))
    # define page
    pages = start_page + 1,
    # Let R decide which method to use
    method = "decide",
    # output should be a matrix
    # chossing data frame may use first row as column
    output = "matrix"
  )
  df_table1 <- data.frame(table1[[1]])
  
  # replace all "" with NA
  df_table1 <- df_table1 %>% mutate_all(na_if,"")
  
  # drop columns with only missings
  df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
  
  # drop bullshit rows 
  df_table1 <- 
    df_table1[!(df_table1[, 1] %in% text_to_drop & rowSums(is.na(df_table1)) >= (ncol(df_table1) - 3)),]
  df_table1 <- 
    df_table1[!(df_table1[, 2] %in% text_to_drop & rowSums(is.na(df_table1)) >= (ncol(df_table1) - 3)),]
  
  # drop columns with only missings or only bullshit
  df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
  for (col_name in colnames(df_table1)){
    if (all(df_table1[, col_name][complete.cases(df_table1[, col_name])] %in% text_to_drop)){
      df_table1 <- df_table1 %>% dplyr::select(-all_of(col_name))
    }
  }
  
  # keep only column 1 to 3 as well as last 6
  df_table1 <- df_table1[, c(1:3, (ncol(df_table1) - 5):ncol(df_table1))]
    ## if third column also contains numbers, drop it
  numbers_only <- function(x) !grepl("\\D", x)
  if (all(numbers_only(df_table1[, 3]))) {
    # drop third column
    df_table1 <- df_table1[, -3]
  }
  
  # drop rows containing only missing values
  df_table1 <- df_table1[rowSums(is.na(df_table1)) != ncol(df_table1), ]
  
  # identify fruits
    ## row names from 1 to n
  rownames(df_table1) <- 1:nrow(df_table1)
    ## identify row numbers that have no size and price information
  row_number <- unique(which(rowSums(is.na(df_table1[, -1])) >= 7))
  fruit <- df_table1[row_number, 1]
    ## fruit column
  df_table1$fruit <- NA
  df_table1[row_number, "fruit"] <- fruit
    ## fill missings below
  df_table1 <- df_table1 %>% fill(fruit)
    ## other missing are apples
  df_table1[is.na(df_table1$fruit), "fruit"] <- "Äpfel"
  
  # drop rows with fruit names
  df_table1 <- df_table1[-row_number, ]
  
  # split first column if number of columns is 9
  if (ncol(df_table1) == 9) {
    df_table1 <- 
      extract(df_table1, X1, into = c("type", "country"), '(.*)\\s+([^ ]+)$')
  }

  # column names
  colnames(df_table1) <- c("type", "country", "size",
                           "price_total", "price_F", "price_H",
                           "price_K", "price_M", "price_B", "fruit")
  
  # append year and calender week
  df_table1$year <- year
  df_table1$week <- i
  
  ## add page indicator
  df_table1$page <- 2
  
  ## add indicator for last entry (needed later if third page continues fruit)
  df_table1$indicator <- seq(1:nrow(df_table1))
  # df_table1[nrow(df_table1), "indicator"] <- "last"
  
  # add to final data frame
  df_final_2 <- rbind(df_final_2, df_table1)
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### Third until last page ####

df_final_3 <- data.frame()

for (i in calender_week) {
  # define file path
  fruit_file_path <- paste0("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/",
                            year, "/Wochenbericht_", year,  "_", i, 
                            ".pdf;jsessionid=548DBD3A2408AD46E2B585AFF1B1E66D.1_cid325?__blob=publicationFile&v=2")
  
  
  # check if file exists
  # if file does not exists, i.e., function returns FALSE, we skip this file
  if (func_file_exists(fruit_file_path) == FALSE) {
    ## skip file
    next 
  }
  
  # read in whole pdf document as text to identify page where report starts
    ## read in document
  fruit_text <- pdf_text(fruit_file_path)
    ## identify page
  start_page <- 
    which(str_detect(fruit_text, "Preisbericht"))[length(which(str_detect(fruit_text, "Preisbericht")))]
  end_page <- which(str_detect(fruit_text, "Speiselagerkartoffeln"))[1] - 1
    ## if page does not exist, skip document
  if (length(start_page) == 0) {
    next
  }
    ## if end page does not exist search for other keyword
  if (is.na(end_page) | length(end_page) == 0) {
    # search for Kartoffeln
    end_page <- which(str_detect(fruit_text, "Kartoffeln"))[1] - 1
  }
    ## if end page still does not exist try other keyword
  if (is.na(end_page) | length(end_page) == 0) {
    # search for E-Mail address which is usually writen at the end of the last table
    end_page <- 
      which(str_detect(
        fruit_text, "obstundgemuesemarkt@ble.de"
        ))[length(which(str_detect(fruit_text, "obstundgemuesemarkt@ble.de")))]
  }
  
  # iterate over all pages
  df_prefinal <- data.frame()
  
  for (page in (start_page + 2):end_page) {
    table1 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(54, 12, 823, 588)), # list(c(58, 12, 810, 247))
      # define page
      pages = page,
      # Let R decide which method to use
      method = "decide",
      # output should be a matrix
      # chossing data frame may use first row as column
      output = "matrix"
    )
    df_table1 <- data.frame(table1[[1]])
    
    # check number of columns:
      ## if number is two small something went very wrong and we skip this page
    if (ncol(df_table1) <= 6) {
      next
    }
    
    # replace all "" with NA
    df_table1 <- df_table1 %>% mutate_all(na_if,"")
    
    # drop columns with only missings
    #df_table1 <- df_table1[, -2]
    df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
    
    # if fruit is in one column, replace all other rows with missings
    df_table1[df_table1[, 1] %in% fruits, c(2:ncol(df_table1))] <- NA
    
    # drop bullshit rows
    # df_table1 <- df_table1[!df_table1$X1 %in% text_to_drop,]
    # drop bullshit rows 
    df_table1 <- 
      df_table1[!(df_table1[, 1] %in% text_to_drop & rowSums(is.na(df_table1)) >= (ncol(df_table1) - 3)),]
    df_table1 <- 
      df_table1[!(df_table1[, 2] %in% text_to_drop & rowSums(is.na(df_table1)) >= (ncol(df_table1) - 3)),]
    
    # drop columns with only missings or only bullshit
    df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
    for (col_name in colnames(df_table1)){
      if (all(df_table1[, col_name][complete.cases(df_table1[, col_name])] %in% text_to_drop)){
        df_table1 <- df_table1 %>% dplyr::select(-all_of(col_name))
      }
    }
    
    # keep only specific columns 
      ## column 1 and 2 as well as last 6
    if (ncol(df_table1) < 12) {
      df_table1 <- df_table1[, c(1:2, (ncol(df_table1) - 5):ncol(df_table1))]
    }
      ## column 1 to 3 as well as last 6
    if (ncol(df_table1) == 12) {
      df_table1 <- df_table1[, c(1:3, (ncol(df_table1) - 5):ncol(df_table1))]
    }
    
    # identify fruits
      ## row names from 1 to n
    rownames(df_table1) <- 1:nrow(df_table1)
      ## identify row numbers that have no size and price information
    row_number <- unique(which(rowSums(is.na(df_table1[, -1])) >= 7))
    fruit <- df_table1[row_number, 1]
      ## fruit column
    df_table1$fruit <- NA
    df_table1[row_number, "fruit"] <- fruit
      ## fill missings
    df_table1 <- df_table1 %>% fill(fruit)
    
    # drop rows with fruit names
    df_table1 <- df_table1[-row_number, ]
    
    # split column
    if (ncol(df_table1) == 9) {
      df_table1 <- 
        extract(df_table1, X1, into = c("type", "country"), '(.*)\\s+([^ ]+)$')
    }

    # column names
    colnames(df_table1) <- c("type", "country", "size",
                             "price_total", "price_F", "price_H",
                             "price_K", "price_M", "price_B", "fruit")
    
    
    # append year and calender week
    df_table1$year <- year
    df_table1$week <- i
    
    # add to final data frame
    df_prefinal <- rbind(df_prefinal, df_table1)
  }
  
  # add indidator for first row
  df_prefinal$indicator <- NA
  df_prefinal[1, "indicator"] <- "first"
  
  ## add page indicator
  df_prefinal$page <- 3
  df_prefinal$indicator <- seq(1:nrow(df_prefinal))
  
  # append to final data frame
  df_final_3 <- rbind(df_final_3, df_prefinal)
}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#### Combine all data frames ####

# sort columns
df_final_1 <- df_final_1 %>%
  dplyr::select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

df_final_2 <- df_final_2 %>%
  dplyr::select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

df_final_3 <- df_final_3 %>%
  dplyr::select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

# combine rows
df_final <- rbind(df_final_1, df_final_2, df_final_3)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### Text Analysis and Manipulation ####

# replace missing fruits by previous one 
# this is the case if for example on page 2, grapes are listed and on third page again
df_final <- df_final %>%
  arrange(year, week, page, indicator)
df_final <- df_final %>%
  fill(fruit)


# identify NAs
  ## NAs in fruits: complete row is dropped 
sum(is.na(df_final$fruit))
df_final <- df_final[!is.na(df_final$fruit),]
  ## NAs in country: complete row is dropped
sum(is.na(df_final$country))
df_final <- df_final[!is.na(df_final$country),]

# some words are incorrectly loaded into R
  ## fruits and vegetables
unique(df_final$fruit)
df_final[str_starts(df_final$fruit, "Banan"), "fruit"] <- "Bananen"
df_final[str_starts(df_final$fruit, "Birn"), "fruit"] <- "Birnen"
df_final[str_starts(df_final$fruit, "Tafeltra"), "fruit"] <- "Tafeltrauben"
df_final[str_starts(df_final$fruit, "Orang"), "fruit"] <- "Orangen"
df_final[str_starts(df_final$fruit, "Clement"), "fruit"] <- "Clementinen"
df_final[str_starts(df_final$fruit, "Satsu"), "fruit"] <- "Satsuma"
df_final[str_starts(df_final$fruit, "Zitro"), "fruit"] <- "Zitronen"
df_final[str_starts(df_final$fruit, "Kiw"), "fruit"] <- "Kiwis"
df_final[str_starts(df_final$fruit, "Möhr"), "fruit"] <- "Möhren"
df_final[str_starts(df_final$fruit, "Bohn"), "fruit"] <- "Bohnen"
df_final[str_starts(df_final$fruit, "Artischo"), "fruit"] <- "Artischocken"
df_final[str_starts(df_final$fruit, "Aubergi"), "fruit"] <- "Auberginen"
df_final[str_starts(df_final$fruit, "Gurk"), "fruit"] <- "Gurken"
df_final[str_starts(df_final$fruit, "Endivi"), "fruit"] <- "Endivien"
df_final[str_starts(df_final$fruit, "Kopfsa"), "fruit"] <- "Kopfsalat"
df_final[str_starts(df_final$fruit, "Tomat"), "fruit"] <- "Tomaten"
df_final[str_starts(df_final$fruit, "Eissa"), "fruit"] <- "Eissalat"
df_final[str_starts(df_final$fruit, "Blumen"), "fruit"] <- "Blumenkohl"
df_final[str_starts(df_final$fruit, "Rosenk"), "fruit"] <- "Rosenkohl"
unique(df_final$fruit)
  ## countries
unique(df_final$country)
for (i in text_to_drop) {
  # drop words defined previously
  df_final$country <- str_replace_all(df_final$country, i, "")
}
# drop all numbers
df_final$country <- str_replace_all(df_final$country, "[0-9]", "")

# in case of two words, keep the second one
df_final$country <- 
  do.call(rbind, strsplit(df_final$country, ' (?=[^ ]+$)', perl = TRUE))[, 2]

# manual replacement
df_final[str_ends(df_final$country, "Deutschland"), "country"] <- "Deutschland"
df_final[str_ends(df_final$country, "Griechenland"), "country"] <- "Griechenland"
df_final[str_ends(df_final$country, "dafrika"), "country"] <- "Südafrika"
df_final[str_ends(df_final$country, "lien"), "country"] <- "Italien"
df_final[str_ends(df_final$country, "Türkei"), "country"] <- "Türkei"
df_final[str_ends(df_final$country, "ile"), "country"] <- "Chile"
df_final[str_ends(df_final$country, "anien"), "country"] <- "Spanien"

unique(df_final$country)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Save data frame ####

saveRDS(df_final, "output/price_data_all.rds")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


