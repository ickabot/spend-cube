library(lubridate)

Add_Transaction_ID <- function(df, dfsource, column, dropcolumn = TRUE) {
  ### this function is to create a unique transaction id for
  ### all data sources that can be then used instead of the actual id
  ### loads tidyverse and magrittr and lubridate if not already loaded
  ### df is the dataframe, of any size that can be used
  ### the last two items need to be quoted "" character strings
  ### dfsource is the source of that df to be used in the unique id
  ### column is the column that will be used to generate unique ids
  library(tidyverse)
  library(magrittr)
  library(lubridate)
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

Dat <- data.frame(site = rep(c(1,8,4), each = 3), score = runif(9))


droppedcolumn<-Add_Transaction_ID(Dat, "SRM", "site")
keptcolumn<-Add_Transaction_ID(Dat, "SRM", "site", FALSE)


