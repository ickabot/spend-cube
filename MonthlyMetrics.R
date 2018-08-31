library(tidyverse)
library(magrittr)
library(stringr)
library(lubridate)
library(readr)
library(readxl)

### Sets up the DocOutPut Import
setwd("C:/Users/ipoole/Desktop/Spend Cube/spend-cube/Data Sources/")
setwd("1_SRM/MM_1_DOCOUTPUT")
dir_docoutput <- dir()

### defines all docoutput column types as charcter

docoutputcolumntypes<-paste0(rep("c",11),collapse = "")

### Imports all docoutputs over eachother

t_docoutput <- bind_rows(lapply(dir_docoutput,read_tsv,col_types = docoutputcolumntypes))

### Cleans the column names

colnames(t_docoutput)<-CleanColnames(t_docoutput)

### creates the PO version number

t_docoutput %<>% group_by(document_number) %>%
  mutate(po_version = row_number(document_number),
         created_on = mdy(created_on))

### Sets up the PO Details import
setwd("..")
setwd("MM_2_PODETAILS")
dir_podetails <- dir()

t_podetails <- bind_rows(lapply(dir_podetails,read_xlsx)

