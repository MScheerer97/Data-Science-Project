#%%%%%%%%%%%%%%%%%%%%#
# Update Food Prices #
#%%%%%%%%%%%%%%%%%%%%#


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

if (!require("readxl")) install.packages("readxl") 
library(readxl)


#### Load Historical Price Data ####

# load old food price data frame
df_food_price_old <- readRDS("output/price_data_all.rds")

# identify most recent food price information
  ## most recent year
max_year <- max(df_food_price_old$year)
  ## for most recent year, find most recent week
max_week <- df_food_price_old %>% 
  filter(year == max_year) %>% 
  summarise(max(week)) %>% 
  pull()


#### General Setup ####

# read in document
if (max_week < 53){
  current_year <- max_year
  current_week <- as.character(as.numeric(max_week)+1)
  fruit_file_path <- paste0("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/",
                            current_year, "/Wochenbericht_", current_year, "_" ,
                            current_week, ".pdf?__blob=publicationFile&v=2")
} else{
  current_year <- as.character(as.numeric(max_year)+1)
  current_week <- as.character(as.numeric(max_week)+1)
  fruit_file_path <- paste0("https://www.ble.de/SharedDocs/Downloads/DE/BZL/Daten-Berichte/ObstGemuese/",
                            current_year, "/Wochenbericht_", current_year, "_" , current_week, 
                            ".pdf?__blob=publicationFile&v=2")
}



# text to drop in documents
text_to_drop <- c("München", "Erzeugnis", "Köln", "Hamburg", "Frankfurt", "Berlin", 
                  "2018", "2019", "2020", "2021", "KW", 
                  "Größe", "Land", "Erzeugnis Land")
for (i in 1:52){
  drop_calender_week <- paste0(i, ". KW")
  text_to_drop <- c(text_to_drop, drop_calender_week)
}

# load excel file including all possible fruit names, types and countries
df_check_type <- read_excel("data/overview_types.xlsx", sheet = 1)
df_check_kind <- read_excel("data/overview_types.xlsx", sheet = 2)
fruits <- unique(na.omit(df_check_kind$Food))
countries <- unique(na.omit(df_check_kind$Country))



#### Import Data ####

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


# only carry out operation if file exists
if (func_file_exists(fruit_file_path) == TRUE){

#### First page ####

  # empty data frame for first page
  df_final_1 <- data.frame()

  # read in whole pdf document as text to identify page where report starts
    ## read in document
  fruit_text <- pdf_text(fruit_file_path)
    ## identify page
  start_page <- 
    which(str_detect(fruit_text, 
                     "Preisbericht"))[length(which(str_detect(fruit_text, 
                                                              "Preisbericht")))]

  # only move on if start_page exists
  
  if (is_numeric(start_page) == TRUE){
    table1 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(273, 21, 899, 269)), # list(c(307, 15, 815, 244))
      # define page
      pages = start_page,
      # Let R decide which method to use
      method = "decide",
      # output should be a matrix
      # chossing data frame may use first row as column
      output = "matrix"
    )
    df_table1 <- data.frame(table1[[1]])
    
    # drop rows 
    df_table1 <- df_table1[!df_table1[,1] %in% text_to_drop, ]
    df_table1 <- df_table1[!df_table1[,2] %in% text_to_drop, ]
    df_table1 <- df_table1[!df_table1[,3] %in% text_to_drop, ]
    
    # on the first page, always apples are present
    df_table1$fruit <- "Äpfel"
    ## if first row contains fruit name, drop it
    if (df_table1[1,1] == "Äpfel"){
      df_table1 <- df_table1[-1, ]
    }
    
    # drop columns containing only missing values or whitespaces
    ## drop columns with only missings
    df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
    ## drop columns with only white spaces
    df_table1 <- df_table1[, colSums(df_table1 == "") != nrow(df_table1)]
    
    if (ncol(df_table1) == 3) {
      # rename columns
      colnames(df_table1) <- c("type", "country", "fruit")
    } else {
      next
    }
    
    
    table2 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(275, 266, 900, 320)), # list(c(326, 243, 816, 290))
      # define page
      pages = start_page,
      # Let R decide which method to use
      method = "decide",
      # output should be a matrix
      # chossing data frame may use first row as column
      output = "matrix"
    )
    
    # create data frame
    df_table2 <- data.frame(table2[[1]]) 
    
    # if we have more than one column skip document
    if (ncol(df_table2) == 1) {
      # if first row contains "Größe" drop it
      if (df_table2[1, ] == "Größe"){
        df_table2 <- data.frame(df_table2[-1, ])
      }
      colnames(df_table2) <- "size" 
    } else {
      next
    }
    
    # drop text
    df_table2 <- data.frame(df_table2[!df_table2[, 1] %in% text_to_drop, ])
   
    table3 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(276, 423, 900, 634)), # list(c(307, 389, 815, 573))
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
    if (ncol(df_table3) == 6) {
      colnames(df_table3) <- 
        c("price_total", "price_F", "price_H", "price_K", "price_M", "price_B")
    } else {
      next
    }
    
    df_table <- cbind(df_table1, df_table2, df_table3)
    
    ## add year and calender week
    df_table$year <- current_year
    df_table$week <- current_week
    
    ## add page indicator
    df_table$page <- 1
    
    # create indicator
    df_table$indicator <- df_table$indicator <- seq(1:nrow(df_table))
    
    # add to final data frame if we have the correct number of columns
    if (ncol(df_table) == 14){
      df_final_1 <- rbind(df_final_1, df_table)
    } else {
      next
    }
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    #### Second page ####
    
    df_final_2 <- data.frame()
    
    table1 <- extract_tables(
      # define file path
      fruit_file_path, 
      # specify are where table is (skip, e.g., explanation)
      # guess = FALSE is necessary that area is recognized
      guess = FALSE,
      area = list(c(63, 23, 908, 633)), # list(c(58, 12, 810, 247))
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
    
    # if fruit is in one column, replace all other rows with missings
    ## to identify fruits, we remove number, e.g. Birnen2021 -> Birnen
    ## we also remove KW
    df_table1[, 1] <- gsub('[[:digit:]]+', '', df_table1[, 1])
    df_table1[, 1] <- gsub('KW', '', df_table1[, 1])
    df_table1[, 1] <- str_trim(df_table1[, 1])
    df_table1[df_table1[, 1] %in% fruits, c(2:ncol(df_table1))] <- NA
    
    # drop bullshit rows 
    df_table1 <- 
      df_table1[!(df_table1[, 1] %in% text_to_drop & 
                    rowSums(is.na(df_table1)) >= (ncol(df_table1)-3)),]
    df_table1 <- 
      df_table1[!(df_table1[, 2] %in% text_to_drop & 
                    rowSums(is.na(df_table1)) >= (ncol(df_table1)-3)),]
    
    # drop columns with only missings or only bullshit
    df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
    for (col_name in colnames(df_table1)){
      if (all(df_table1[, col_name][complete.cases(df_table1[, col_name])] %in% text_to_drop)){
        df_table1 <- df_table1 %>% select(-all_of(col_name))
      }
    }
    
    # keep only column 1 to 3 as well as last 6
    df_table1 <- df_table1[, c(1:3, (ncol(df_table1)-5):ncol(df_table1))]
    ## if third column also contains numbers, drop it
    numbers_only <- function(x) !grepl("\\D", x)
    if (all(numbers_only(stri_replace_all_fixed(na.omit(df_table1[, 3]), " ", "")))) {
      # drop third column
      df_table1 <- df_table1[, -3]
    }
    
    # drop rows containing only missing values
    df_table1 <- df_table1[rowSums(is.na(df_table1)) != ncol(df_table1), ]
    
    # identify fruits
    ## row names from 1 to n
    rownames(df_table1) <- 1:nrow(df_table1)
    ## identify row numbers that have no size and price information
    row_number_check <- unique(which(rowSums(is.na(df_table1[, -1])) >= 7))
    fruit_check <- df_table1[row_number_check, 1]
    ## Keep fruit only if it is in specified fruits vector
    ## It may be the case that some countries or fruit types are identifed as fruit
    ## this also required adjusting the row_number vector
    fruit <- fruit_check[str_detect(fruit_check, paste(fruits, collapse = "|"))]
    row_number <- row_number_check[str_detect(fruit_check, paste(fruits, collapse = "|"))]
    ## fruit column
    df_table1$fruit <- NA
    df_table1[row_number, "fruit"] <- fruit
    ## fill missings below
    df_table1 <- df_table1 %>% fill(fruit)
    ## other missing are apples
    df_table1[is.na(df_table1$fruit), "fruit"] <- "Äpfel"
    
    # drop rows with fruit names and those who are falsely identified as such
    if (length(row_number_check)!=0){
      df_table1 <- df_table1[-row_number_check, ]
    }
    
    # split first column if number of columns is 9
    if (ncol(df_table1) == 9) {
      # df_table1 <- 
      #   extract(df_table1, X1, into = c("type", "country"), '(.*)\\s+([^ ]+)$')
      df_table1 <- df_table1 %>% separate(X1,c("type","country"),sep=" (?=[^ ]*$)")
    }
    
    # column names
    colnames(df_table1) <- c("type", "country", "size",
                             "price_total", "price_F", "price_H",
                             "price_K", "price_M", "price_B", "fruit")
    
    # append year and calender week
    df_table1$year <- current_year
    df_table1$week <- current_week
    
    ## add page indicator
    df_table1$page <- 2
    
    ## add indicator for last entry (needed later if third page continues fruit)
    df_table1$indicator <- seq(1:nrow(df_table1))
    # df_table1[nrow(df_table1), "indicator"] <- "last"
    
    # add to final data frame
    df_final_2 <- rbind(df_final_2, df_table1)
    
    
    
    #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    #### Third until last page ####
    
    df_final_3 <- data.frame()
    
    end_page <- tail(which(str_detect(fruit_text, "obstundgemuesemarkt@ble.de")), 1)
    
    ## if end page does not exist search for other keyword
    if (length(end_page) == 0){
      # search for Speiselagerkartoffeln
      end_page <- which(str_detect(fruit_text, "Speiselagerkartoffeln"))[1] - 1
    } else if (is.na(end_page)){
      end_page <- which(str_detect(fruit_text, "Speiselagerkartoffeln"))[1] - 1
      # search for Kartoffeln
    } else if(is.na(end_page)){
      end_page <- which(str_detect(fruit_text, "Kartoffeln"))[1] - 1
    } else if(length(end_page) == 0){
      end_page <- which(str_detect(fruit_text, "Kartoffeln"))[1] - 1
    }
    
    # iterate over all pages
    df_prefinal <- data.frame()
    
    for (page in (start_page+2):end_page){
      table1 <- extract_tables(
        # define file path
        fruit_file_path, 
        # specify are where table is (skip, e.g., explanation)
        # guess = FALSE is necessary that area is recognized
        guess = FALSE,
        area = list(c(58, 16, 905, 639)), # list(c(58, 12, 810, 247))
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
      if (ncol(df_table1) <= 6){
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
        df_table1[!(df_table1[, 1] %in% text_to_drop & rowSums(is.na(df_table1)) >= (ncol(df_table1)-3)),]
      df_table1 <- 
        df_table1[!(df_table1[, 2] %in% text_to_drop & rowSums(is.na(df_table1)) >= (ncol(df_table1)-3)),]
      
      # drop columns with only missings or only bullshit
      df_table1 <- df_table1[, colSums(is.na(df_table1)) != nrow(df_table1)]
      for (col_name in colnames(df_table1)){
        if (all(df_table1[, col_name][complete.cases(df_table1[, col_name])] %in% text_to_drop)){
          df_table1 <- df_table1 %>% select(-all_of(col_name))
        }
      }
      
      # keep only specific columns 
      df_table1 <- df_table1[, c(1:3, (ncol(df_table1)-5):ncol(df_table1))]
      ## if second and/or third column also contains numbers, drop it
      numbers_only <- function(x) !grepl("\\D", x) # function to identify numbers
      if (all(numbers_only(stri_replace_all_fixed(na.omit(df_table1[, 3]), " ", ""))) &
          all(numbers_only(stri_replace_all_fixed(na.omit(df_table1[, 2]), " ", "")))) {
        # drop third column
        df_table1 <- df_table1[, -c(2,3)]
      } else if (all(numbers_only(stri_replace_all_fixed(na.omit(df_table1[, 3]), " ", "")))) {
        # drop third column
        df_table1 <- df_table1[, -3]
      }
      
      
      # identify fruits
      ## row names from 1 to n
      rownames(df_table1) <- 1:nrow(df_table1)
      ## identify row numbers that have no size and price information
      row_number_check <- unique(which(rowSums(is.na(df_table1[, -1])) >= 6))
      fruit_check <- df_table1[row_number_check, 1]
      ## Keep fruit only if it is in specified fruits vector
      ## It may be the case that some countries or fruit types are identifed as fruit
      ## this also required adjusting the row_number vector
      fruit <- fruit_check[str_detect(fruit_check, paste(str_sub(fruits, 1, 4), collapse = "|"))]
      row_number <- row_number_check[str_detect(fruit_check, paste(str_sub(fruits, 1, 4), collapse = "|"))]
      ## check if fruit and row_number vector contain NA
      ## if yes drop it
      row_number <- row_number[!fruit %in% NA]
      fruit <- fruit[!fruit %in% NA]
      ## fruit column
      df_table1$fruit <- NA
      if (!all(is.na(row_number))){
        df_table1[row_number, "fruit"] <- fruit
      }
      ## fill missings below
      df_table1 <- df_table1 %>% fill(fruit)
      
      # drop rows with fruit names
      if (!all(is.na(row_number))){
        df_table1 <- df_table1[-row_number, ]
      }
      
      # drop columns with many missing values
      df_table1 <- df_table1[rowSums(is.na(df_table1)) < 6, ]
      
      # split column
      if (ncol(df_table1) == 9){
        # df_table1 <- 
        #   extract(df_table1, X1, into = c("type", "country"), '(.*)\\s+([^ ]+)$')
        df_table1 <- df_table1 %>% separate(X1,c("type","country"),sep=" (?=[^ ]*$)")
      } else if (ncol(df_table1) == 8){
        # df_table1 <- 
        #   extract(df_table1, X1, into = c("type", "size"), '(.*)\\s+([^ ]+)$')
        # df_table1 <- 
        #   extract(df_table1, type, into = c("type", "country"), '(.*)\\s+([^ ]+)$')
        df_table1 <- df_table1 %>% separate(X1,c("type","size"),sep=" (?=[^ ]*$)")
        df_table1 <- df_table1 %>% separate(type,c("type","country"),sep=" (?=[^ ]*$)")
      }
      
      # if table does not contain any rows, skip it
      if (nrow(df_table1) == 0){
        next
      }
      
      
      # column names
      colnames(df_table1) <- c("type", "country", "size",
                               "price_total", "price_F", "price_H",
                               "price_K", "price_M", "price_B", "fruit")
      
      
      # append year and calender week
      df_table1$year <- current_year
      df_table1$week <- current_week
      
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
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#### Potatoes ####

  df_final_4 <- data.frame()

  ## identify page of potato report
  end_page <- which(str_detect(fruit_text, "Speiselagerkartoffeln"))[1] 
  ## if end page does not exist search for other keyword
  if (length(end_page) == 0){
    # search for Kartoffeln
    end_page <- which(str_detect(fruit_text, "Kartoffeln"))[1]
  } else if (is.na(end_page)){
    end_page <- which(str_detect(fruit_text, "Kartoffeln"))[1]
  }
  if (length(end_page) == 0){
    # search for Afra (kind of potatoe)
    end_page <- which(str_detect(fruit_text, "Afra"))[1]
  } else if (is.na(end_page)){
    end_page <- which(str_detect(fruit_text, "Afra"))[1]
  }
  ## if page still cannot be found, skip it
  if (length(end_page) == 0){
    next
  } else if (is.na(end_page)){
    next
  }
  
  # read in first two column: country and type #
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  table1 <- extract_tables(
    # define file path
    fruit_file_path, 
    # specify are where table is (skip, e.g., explanation)
    # guess = FALSE is necessary that area is recognized
    guess = FALSE,
    area = list(c(155, 32, 522, 170)), # list(c(155, 32, 522, 560))
    # define page
    pages = end_page,
    # Let R decide which method to use
    method = "decide",
    # output should be a matrix
    # chossing data frame may use first row as column
    output = "matrix"
  )
  df_table1 <- data.frame(table1[[1]])
  
  # replace whitespace my missing values
  df_table1 <- df_table1 %>% mutate_all(na_if,"")
  
  # drop columns containing many missing values
  df_table1 <- data.frame(df_table1[, colSums(is.na(df_table1)) < (nrow(df_table1)-3)])
  
  # if the resulting data frame contains only one column
  # the country and type information is stored in the first column
  # in this case, we need to split it
  if (ncol(df_table1) == 1){
    # df_table1 <- extract(df_table1, 1, into = c("country", "type"), 
    #                      '(.*)\\s+([^ ]+)$')
    df_table1 <- df_table1 %>% separate(1,c("country", "type"),sep=" (?=[^ ]*$)")
    # if, however, the resulting data frame has two column, we just rename the
    # column names
  } else if (ncol(df_table1) == 2){
    colnames(df_table1) <- c("country", "type")
    # otherwise, we skip the document
  } else {
    next
  }
  
  # column names are "Herkunft" and "Sorte"
  ## we drop all rows until this condition
  row_number_drop_1 <- row.names(df_table1[df_table1$country %in% "Herkunft", ]) 
  row_number_drop_2 <- row.names(df_table1[df_table1$type %in% "Sorte", ]) 
  if (length(row_number_drop_1) == 0 | length(row_number_drop_2) == 0){
    df_table1 <- df_table1
  } else if (row_number_drop_1 == row_number_drop_2){
    df_table1 <- df_table1[-c(1:row_number_drop_1), ]
  } else {
    df_table1 <- df_table1
  }
  
  # drop rows containing only NA
  df_table1 <- df_table1[rowSums(is.na(df_table1)) != ncol(df_table1), ]
  
  
  
  # read in price information #
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  
  table2 <- extract_tables(
    # define file path
    fruit_file_path, 
    # specify are where table is (skip, e.g., explanation)
    # guess = FALSE is necessary that area is recognized
    guess = FALSE,
    area = list(c(155, 164, 522, 552)), # list(c(155, 32, 522, 560))
    # define page
    pages = end_page,
    # Let R decide which method to use
    method = "decide",
    # output should be a matrix
    # chossing data frame may use first row as column
    output = "matrix"
  )
  df_table2 <- data.frame(table2[[1]])
  
  # if we have only less than three columns, we skip the document
  if (ncol(df_table2) <= 3){
    next
  }
  
  # iterate over the rows to find the rows where Speisefrühkartoffeln and Speiselagerkartoffeln
  # (potatoe types) are stored
  row_number_1 <- c()
  row_number_2 <- c()
  for (k in 1:nrow(df_table2)){
    if (any(df_table2[k,] %in% "Speisefrühkartoffeln")){
      row_number_1 <- c(row_number_1, k)
    } else if (any(df_table2[k,] %in% "Speiselagerkartoffeln")){
      row_number_2 <- c(row_number_2, k)
    }
  }
  
  # keep always first occurence
  row_number_1 <- row_number_1[1]
  row_number_2 <- row_number_2[1]
  
  # if both Speisefrühkartoffeln and Speiselagerkartoffeln exist:
  if (length(row_number_1) == 1 & length(row_number_2) == 1) {
    # create indicator from first row to row_number_2 with "Speisefrühkartoffeln"
    # and from row_number_2 until the last row with "Speiselagerkartoffeln"
    # note: row_number_2 is dropped 
    df_table2$fruit <- NA
    df_table2[1:row_number_2, "fruit"] <- "Speisefrühkartoffeln"
    df_table2[row_number_2:nrow(df_table2), "fruit"] <- "Speiselagerkartoffeln"
    
    # drop rows until row_number_1, and row_number_2
    df_table2 <- df_table2[-c(1:row_number_1, row_number_2), ]
    # if only Speisefrühkartoffeln exist:
  } else if (length(row_number_1) == 1 & length(row_number_2) == 0){
    df_table2$fruit <- "Speisefrühkartoffeln"
    df_table2 <- df_table2[-c(1:row_number_1), ]
    # if only Speiselagerkartoffeln exist:
  } else if (length(row_number_1) == 0 & length(row_number_2) == 1){
    df_table2$fruit <- "Speiselagerkartoffeln"
    df_table2 <- df_table2[-row_number_2, ]
    # if no one exist, insert Kartoffeln:
  } else {
    df_table2$fruit <- "Kartoffeln"
  }
  
  
  
  # remove letters in each column (apart from fruit)
  # e.g. for "e 40/84", the e is removed
  for (k in 1:(ncol(df_table2)-1)){
    df_table2[, k] <- df_table2[, k] %>% 
      str_replace_all("[a-z]", "") %>% # replace letters by whitespace
      str_trim() # remove whitespace
  }
  
  # remove column with only missings or ""
  df_table2 <- df_table2[, !colSums(df_table2 == "") == nrow(df_table2)]
  
  # check if all columns contain "/"
  col <- c()
  for (k in 1:(ncol(df_table2)-1)){
    if (any(str_detect(df_table2[, k], "/.")) == FALSE){ # "[0-9]*/.*[0-9]"
      col <- c(col, k)
    }
  }
  # if this is not the case, i.e. column does not contain "/", combine columns
  ## if col is missing or 0, leave df_table2 as it is
  if (length(col) == 0){
    df_table2 <- df_table2
  } else if (length(col) == 1){
    for (k in col){
      df_table2 <- df_table2 %>% unite(new_col, k, k+1, sep = "")
    }
  } else {
    df_table2 <- df_table2
  }
  
  # if this is the case: number / number number / number
  # we first identify the column... 
  col <- c()
  for (k in 1:(ncol(df_table2)-1)){
    if (any(str_detect(df_table2[, k], "[0-9]*/.*[0-9]*/.*[0-9]")) == TRUE){ 
      col <- c(col, k)
    }
  }
  # ...split the column
  ## row number where to split
  rownames_split <- row.names(df_table2[str_detect(df_table2[, col], 
                                                   "[0-9]*/.*[0-9]*/.*[0-9]"), ])
  ## replace whitespace by "_"
  df_table2[rownames_split, col] <- 
    str_replace_all(df_table2[rownames_split, col][str_detect(df_table2[rownames_split, col], 
                                                              "[0-9]*/.*[0-9]*/.*[0-9]")], " ", "_")
  ## split by "_" in 4 pieces
  df_table2[rownames_split, col] <- 
    sapply(str_split(df_table2[rownames_split, col], "_", n = 4), tail, 1)
  
  ## last piece is last column
  ## to insert it there, we first create a new column after col
  df_table2 <- add_column(df_table2, price_new = NA, .after = col)
  df_table2[rownames_split, "price_new"] <- 
    ## replace "_" again by whitespace
    str_replace_all(sapply(str_split(df_table2[rownames_split, col], "_", n = 4), 
                           tail, 1), "_", " ")
  
  ## at original column, we keep only the first entry
  df_table2[rownames_split, col] <- 
    str_extract(df_table2[rownames_split, col], "[0-9]*_/._*[0-9]*")
  
  # correct column names
  ## if we do not have 6 columns, something went wrong and we skip this document
  if (ncol(df_table2) == 6){
    colnames(df_table2) <- c("price_B", "price_F", "price_H", 
                             "price_K", "price_M", "fruit")
  } else {
    next
  }
  
  # for price values with "/", we take the mean
  # -> we add both price values and divide by 2
  for (k in 1:5){
    df_table2[, k] <- 
      # number before /
      (as.numeric(gsub("/.*$","",df_table2[, k])) + 
         # number after /
         as.numeric(gsub('.*/.', '', df_table2[, k]))) / 2
  }
  
  
  # combine data frames #
  #%%%%%%%%%%%%%%%%%%%%%#
  
  # data frames can only be combined column-wise if number of rows match
  # if this is not the case, we skip
  if (nrow(df_table1) == nrow(df_table2)){
    df_potatoes <- cbind(df_table1, df_table2)
  } else {
    next
  }
  
  # add year and week 
  df_potatoes$year <- current_year
  df_potatoes$week <- current_week
  
  # price total is mean over cities
  df_potatoes <- df_potatoes %>% 
    mutate(
      price_total = rowMeans(
        df_potatoes %>% select(starts_with("price_")), na.rm = TRUE
      )
    )
  
  # add size, indicator and page variable as NA
  # needed to merge with fruit and vegetables info
  df_potatoes$size <- NA
  df_potatoes$indicator <- NA
  df_potatoes$page <- NA
  
  # same order as for fruit and vegetables data frame
  df_potatoes <- df_potatoes %>%
    select(colnames(df_final_3))
  
  # combine everything to final data frame
  df_final_4 <- rbind(df_final_4, df_potatoes)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### Combine all data frames ####

# sort columns
df_final_1 <- df_final_1 %>%
  select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

df_final_2 <- df_final_2 %>%
  select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

df_final_3 <- df_final_3 %>%
  select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

df_final_4 <- df_final_4 %>%
  select(fruit, type, country, size, price_total,
         price_F, price_H, price_K, price_M, price_B,
         year, week, page, indicator)

# combine rows
df_final <- rbind(df_final_1, df_final_2, df_final_3, df_final_4)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#### Text Analysis and Manipulation ####

# drop all rows with missing type
sum(is.na(df_final$type))
df_final <- df_final[!is.na(df_final$type), ]

# replace missing fruits by previous one 
# this is the case if for example on page 2, grapes are listed and on third page again
df_final <- df_final %>%
  arrange(year, week, page, indicator) %>%
  fill(fruit)

# identify further NAs
## NAs in fruits: complete row is dropped 
sum(is.na(df_final$fruit))
df_final <- df_final[!is.na(df_final$fruit),]
## NAs in country: 
## there are missing values in the country variable for some observations
## we fix this manually
sum(is.na(df_final$country))
countries_find <- c("afrika", "ypten", "anien", "lien", "mibia", "eru", 
                    "erkei", "chenland")
countries_rep <- c("Südafrika", "Ägypten", "Spanien", "Italien", "Namibia", "Peru",
                   "Türkei", "Griechenland")
for (i in 1:length(countries_find)){
  df_final[is.na(df_final$country) & str_detect(df_final$type, countries_find[i]), 
           "country"] <- countries_rep[i]
}
## if still NAs are kept, drop them
df_final <- df_final[!is.na(df_final$country), ]


#### Text Analysis Fruits ####

# some words for fruits and vegetables are incorrectly loaded into R
## iterate over fruits and vegetables to replace wrong word with correct one
for (i in 1:length(fruits)){
  ## since we have Speisezwiebeln, Speiselagerkartoffeln and Speisefrühkartoffeln, we need 
  ## to create an if statement, that used for above words a longer substring
  if (str_detect(fruits[i], "Speise")){
    df_final[str_starts(df_final$fruit, str_sub(fruits[i], 1, 8)) & !df_final$fruit %in% fruits, 
             "fruit"] <- fruits[i]
    df_final[str_ends(df_final$fruit, str_sub(fruits[i], -6, -1)) & !df_final$fruit %in% fruits, 
             "fruit"] <- fruits[i]
  } else {
    df_final[str_starts(df_final$fruit, str_sub(fruits[i], 1, 4)) & !df_final$fruit %in% fruits, 
             "fruit"] <- fruits[i]
    df_final[str_ends(df_final$fruit, str_sub(fruits[i], -6, -1)) & !df_final$fruit %in% fruits, 
             "fruit"] <- fruits[i]
  }
}

## two manual replacements are necessary
df_final[df_final$fruit == "25. KWMöhre2n019", "fruit"] <- "Möhren"
df_final[df_final$fruit == "GrößePfirsiche", "fruit"] <- "Pfirsiche"

## check
unique(df_final$fruit)


#### Text Analysis Countries ####

## countries

unique(df_final$country)
for (i in text_to_drop){
  # drop words defined previously
  df_final$country <- str_replace_all(df_final$country, i, "")
}
# drop all numbers
df_final$country <- str_replace_all(df_final$country, "[0-9]", "")
unique(df_final$country)
# replace
for (i in 1:length(countries)){
  df_final[str_starts(df_final$country, str_sub(countries[i], 1, 4)) & 
             !df_final$country %in% countries, 
           "country"] <- df_final$country[i]
  df_final[str_ends(df_final$country, str_sub(countries[i], -4, -1)) & 
             !df_final$country %in% countries, 
           "country"] <- df_final$country[i]
  
}
# remove whitespace
df_final$country  <- str_trim(df_final$country)
unique(df_final$country)
# manual replacement
df_final[df_final$country == "SugraoCnheile", "country"] <- "Chile"
df_final[df_final$country == "GrTöüßrkeei", "country"] <- "Türkei"
df_final[df_final$country == "S. pKanWien", "country"] <- "Spanien"
df_final[df_final$country == "SugraoCnheile", "country"] <- "Chile"
df_final[df_final$country == "SugraoPneeru", "country"] <- "Peru"

# "/" indicates unknown country such as for bananas
df_final[df_final$country == "/", "country"] = "unknown"

# For countries where value is still missing, split type variable
df_final[df_final$country == "", ] <-
  extract(df_final[df_final$country == "", ], 
          type, into = c("type", "country"), '(.*)\\s+([^ ]+)$')

# drop countries which are not in the pre-defined country vector or unknown
df_final <- df_final %>% filter(country %in% countries | country == "unknown")

# For all other missing countries, drop observations
df_final <- df_final[df_final$country != "", ]
df_final <- df_final[!is.na(df_final$country), ]

# check countries
unique(df_final$country)

#### Text Analysis Types ####

# no fruit or vegetable type #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%# 

# if type starts with "/", insert "none", meaning no type exists
df_final[str_detect(df_final$type, "^/"), "type"] <- "unknown"
# if type ends with "/" and has a number before, e.g. 2020/, no type exists
df_final[str_detect(df_final$type, "[0-9].*/$"), "type"] <- "unknown"

for (i in 1:ncol(df_check_type)){
  # select column
  df_type_sub <- data.frame(na.omit(df_check_type[, i]))
  
  # iterate over each element
  for (j in 1:nrow(df_type_sub)){
    df_final[(str_starts(str_to_lower(df_final$type),
                         str_sub(str_to_lower(df_type_sub[j, 1]), 1, 8))) &
               (df_final$fruit == colnames(df_type_sub)),
             "type"] <- df_type_sub[j, 1]
    df_final[(str_ends(str_to_lower(df_final$type),
                       str_sub(str_to_lower(df_type_sub[j, 1]), -8, -1))) &
               (df_final$fruit == colnames(df_type_sub)),
             "type"] <- df_type_sub[j, 1]
    
    # if type does not Speise, we need to use a longer sib string, since we have
    # Speisezwiebeln, Speiselagerkartoffeln and Speisefrühkartoffeln
    # if (str_detect(df_type_sub[j, 1], "Speise")){
    #   df_final[(str_starts(str_to_lower(df_final$type), 
    #                        str_sub(str_to_lower(df_type_sub[j, 1]), 1, 8))) & 
    #              (df_final$fruit == colnames(df_type_sub)), 
    #            "type"] <- df_type_sub[j, 1]
    #   df_final[(str_ends(str_to_lower(df_final$type), 
    #                      str_sub(str_to_lower(df_type_sub[j, 1]), -8, -1))) & 
    #              (df_final$fruit == colnames(df_type_sub)), 
    #            "type"] <- df_type_sub[j, 1]
    # # otherwise, we can use a smaller sub string 
    # } else {
    #   df_final[(str_starts(str_to_lower(df_final$type), 
    #                        str_sub(str_to_lower(df_type_sub[j, 1]), 1, 4))) & 
    #              (df_final$fruit == colnames(df_type_sub)), 
    #            "type"] <- df_type_sub[j, 1]
    #   df_final[(str_ends(str_to_lower(df_final$type), 
    #                      str_sub(str_to_lower(df_type_sub[j, 1]), -4, -1))) & 
    #              (df_final$fruit == colnames(df_type_sub)), 
    #            "type"] <- df_type_sub[j, 1]
    # }
  }
}

# drop words defined previously
for (i in text_to_drop){
  df_final$type <- str_replace_all(df_final$type, i, "")
}

# drop numbers
df_final$type <- str_replace_all(df_final$type, "[0-9]", "")

# drop slashes and points at the beginning or end
## since we have things like ". / /", we repeat the process
## until nothing is left
while (any(str_detect(df_final$type, "^. | .$ | ^/"))){
  # remove slashs at the beginning of the string
  df_final$type <- str_replace_all(df_final$type, "^/", "")
  # remove dots at the end of the string
  df_final$type <- gsub("\\.$","",df_final$type)
  # remove dots at the beginning of the string
  df_final$type <- gsub("^\\.","",df_final$type)
  # remove white space
  df_final$type <- str_trim(df_final$type)
}

# type with whitespace is simply unknown
df_final[df_final$type == "", "type"] <- "unknown"

# if type contains type and country split column
## all types in one vector
types <- c()
for (i in 1:ncol(df_check_type)){
  # extract all unique values from column i (exclude missing values)
  # pull() extracts from the tibble column all values as vector
  types <- c(types, pull(unique(na.omit(df_check_type[, i]))))
  # some types may appear multiple times
  # ensure we have only unique values
  types <- unique(types)
}
## iterate over rows
for (i in 1:nrow(df_final)){
  if (str_detect(df_final[i, "type"], paste(types, collapse = "|")) & 
      str_detect(df_final[i, "type"], paste(c(countries, "unknown"), collapse = "|"))){
    string_rep <- str_split(df_final[i, "type"], pattern = " ")[[1]]
    df_final[i, "type"] <- string_rep[1]
    df_final[i, "country"] <- string_rep[2]
  }
}

# drop rows where type is wrong, e.g. Orange has Kanzi
# df_types <- df_final %>% group_by(fruit) %>% summarise(unique(type))

# check
unique(df_final$type)


#### Text Analysis Size ####

# replace NA and "/ /" with "/"
df_final[is.na(df_final$size), "size"] <- "/"
df_final[df_final$size == "/ /", "size"] <- "/"

# manual replacement for some files necessary
## drop year 2020 week 42 with size as number
## drop year 2021 week 14&15
df_final <- df_final %>% 
  filter(!((str_detect(df_final$size, "^[0-9][0-9][0-9]$") | 
              str_detect(df_final$size, "^[0-9][0-9]$") |
              str_detect(df_final$size, "1.[0-9][0-9]")) &
             week %in% c("43", "14", "15", "11") & 
             year %in% c("2020", "2021")))
## for the following replace size by "/"
df_final <- df_final %>% 
  filter(((str_detect(df_final$size, "^[0-9][0-9][0-9]$") | 
             str_detect(df_final$size, "^[0-9][0-9]$") |
             str_detect(df_final$size, "1.[0-9][0-9]") ) &
            week %in% c("13", "14", "15", "11", "12", "16", "22") & 
            year %in% c("2019", "2021"))) %>%
  mutate(size = "/") %>%
  rbind(
    df_final %>% 
      filter(!((str_detect(df_final$size, "^[0-9][0-9][0-9]$") | 
                  str_detect(df_final$size, "^[0-9][0-9]$") |
                  str_detect(df_final$size, "1.[0-9][0-9]")) &
                 week %in% c("13", "14", "15", "11", "12", "16", "22") & 
                 year %in% c("2019", "2021")))
  )

# replace values
df_final[df_final$size == "Größelose", "size"] <- "lose"
df_final[df_final$size == "lose lose", "size"] <- "lose"

# in case of two keep the first one
df_final$size <- sub(" .*", "", df_final$size)

# if country is contained in size, put country in country and size "/"
df_final[df_final$size %in% countries, "country"] <- df_final[df_final$size %in% countries, "size"]
df_final[df_final$size %in% countries, "size"] <- "/"

# check
unique(df_final$size)



# final variable check #
#%%%%%%%%%%%%%%%%%%%%%%#

unique(df_final$fruit)
unique(df_final$country)
unique(df_final$type)
unique(df_final$size)




