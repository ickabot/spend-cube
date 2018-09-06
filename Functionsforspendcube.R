library(tidyverse)
library(magrittr)
library(lubridate)
library(readr)
library(readxl)
library(stringr)

Add_Transaction_ID <- function(df, dfsource, column, dropcolumn = TRUE) {
  ### this function is to create a unique transaction id for
  ### all data sources that can be then used instead of the actual id
  ### loads tidyverse and magrittr and lubridate if not already loaded
  ### df is the dataframe, of any size that can be used
  ### the last two items need to be quoted "" character strings
  ### dfsource is the source of that df to be used in the unique id
  ### column is the column that will be used to generate unique ids
  a<-df
  a %<>% mutate(trans_id = paste0(dfsource,"_",str_replace_all(as.character(today()),"-",""),match(df[[column]],
                                                        unique(df[[column]]))))
  ### once this is done the column is either dropped or not depending if dropcolumn is
  ### true or false
  if (dropcolumn==TRUE) {
  a <- a[ , !(names(a)==column)]
  }
  a
}

### this is test data

#Dat <- data.frame(site = rep(c(1,8,4), each = 3), score = runif(9))

#droppedcolumn<-Add_Transaction_ID(Dat, "SRM", "site")
#keptcolumn<-Add_Transaction_ID(Dat, "SRM", "site", FALSE)

### function to clean the column names

CleanColnames <- function(df) {
  colnames(df) <- str_replace_all(colnames(df),"#", "Num")
  colnames(df) <- str_replace_all(colnames(df),"\\$", "Spend")
  colnames(df) <- str_replace_all(colnames(df),"[[:punct:]]", "")
  colnames(df) <- str_replace_all(colnames(df)," ","_")
  colnames(df) <- str_replace_all(colnames(df),"__","_")
  colnames(df) <- tolower(colnames(df))
}

### function to create column types where you suppply number of character and number of digit columns

ColTypesdigitend <- function(n_character_cols,n_digit_cols) {
  paste0(paste0(rep("c",n_character_cols),collapse = ""),
         paste0(rep("d",n_digit_cols),collapse = ""),
         collapse = "")
}

### function to create column names for BW reports

BWColnames <- function (directory) {
  colstr1 <- na.omit(as.character(read_delim(directory[1],skip = 4, delim = ";", n_max = 1, col_names = FALSE))) 
  colstr2 <- as.character(read_delim(directory[1],skip = 5, delim = ";", n_max = 1, col_names = FALSE))
  colstr2 <- colstr2[1:(length(colstr2)-length(colstr1))]
  c(colstr2,colstr1)
}

### combined import function for BW files

BWImport <- function(directory = dir()) {
  colstr1 <- na.omit(as.character(read_delim(directory[1],skip = 4, delim = ";", n_max = 1, col_names = FALSE))) 
  colstr2 <- as.character(read_delim(directory[1],skip = 5, delim = ";", n_max = 1, col_names = FALSE))
  colstr2 <- colstr2[1:(length(colstr2) - length(colstr1))]
  columnNames <- c(colstr2, colstr1)
  columnTypes <- paste0(paste0(rep("c", length(columnNames)), collapse = ""))
  importtable <- bind_rows(lapply(directory,read_delim, delim = ";", 
                                  col_types = columnTypes, 
                                  col_names = columnNames, skip = 6))
  colnames(importtable) <- CleanColnames(importtable)
  importtable
}

### function to convert currency

convertCurrency <- function(currency) {
  currency1 <- sub('$','',as.character(currency),fixed=TRUE)
  currency2 <- as.numeric(gsub('\\,','',as.character(currency1))) 
  currency2
}

### function to create seperate quantity

seperateQuantity <- function(columnnames,df) { 
  df %>% select(columnnames) %>% 
    separate(columnnames, paste0(columnnames, c(".1",".2")), sep = "\\s+") 
}


### function to create measure columns

MeasureColumns <- function(df, measure1 = "value", measure2 = "paid", measure3 = "quantity") {
  values <- as.data.frame(lapply(select(df, ends_with(measure1), ends_with(measure2)), convertCurrency))
  quantities <- as.data.frame(select(df, ends_with(measure3))) %>%
    mutate(id = row_number())
  quantitieslength <- length(quantities)-1
  quantities <- names(quantities[1:quantitieslength]) %>%
    map(seperateQuantity,quantities) %>%
    as.data.frame()
  num_quantities <- quantities %>% select(ends_with(".1"))
  uom_quantities <- quantities %>% select(ends_with(".2"))
  num_quantities <- as.data.frame(lapply(num_quantities, as.numeric))
  df %<>% select(-ends_with(measure1), -ends_with(measure2), -ends_with(measure3)) %>%
    bind_cols(values, num_quantities, uom_quantities)
}

### function to read shared drive folders

directoryPrint <- function(directory = dir()) {
  b <- data.frame()
  for (i in 1:length(directory)) {
    setwd(directory[i])
    a <- as.data.frame(dir())
    b <- bind_rows(b,a)
    setwd("..")
  }
  b
}

