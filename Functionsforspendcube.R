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
  colnames(df) <- str_replace_all(colnames(df),"$", "Spend")
  colnames(df) <- str_replace_all(colnames(df),"[[:punct:]]", "")
  colnames(df) <- str_replace_all(colnames(df)," ","_")
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
  rm_spend <- c("$","#")
  colstr1 <- as.character(read_delim(directory[1],skip = 5, delim = ";", n_max = 1, col_names = FALSE))
  colstr1 <- colstr1[!colstr1 %in% rm_spend]
  colstr2 <- na.omit(as.character(read_delim(directory[1],skip = 4, delim = ";", n_max = 1, col_names = FALSE))) 
  c(colstr1,colstr2)
}

### combined import function for BW files

BWImport <- function(directories,x,y) {
  coltypes <- ColTypesdigitend(x,y)
  colnames <- BWColnames(directories)
  importtable <- bind_rows(lapply(directories,read_delim, delim = ";", 
                                  col_types = coltypes, 
                                  col_names = colnames, skip = 5))
  colnames(importtable) <- CleanColnames(importtable)
  importtable
}
