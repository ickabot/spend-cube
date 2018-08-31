
### loads packages and functions
setwd("C:/Users/ipoole/Desktop/Spend Cube/spend-cube")
source("Functionsforspendcube.R")

### Sets the wd as in the data sources directory
setwd("Data_Sources/")

### Sets up the DocOutPut Import
setwd("1_SRM/MM_1_DOCOUTPUT")
dir_docoutput <- dir()

### defines all docoutput column types as charcter

coltypes_docoutput <- paste0(rep("c",11),collapse = "")

### Imports all docoutputs over eachother

t_docoutput <- bind_rows(lapply(dir_docoutput,read_tsv,col_types = coltypes_docoutput))

### Cleans the column names

colnames(t_docoutput)<-CleanColnames(t_docoutput)

### Makes created_on a date field

t_docoutput$created_on <- mdy(t_docoutput$created_on)

### filters change and non-change POs

t_docoutput_changePOs <- t_docoutput %>%
  filter(duplicated(document_number)|duplicated(document_number,fromLast = TRUE))

t_docoutput_nochangePOs <- t_docoutput %>%
  filter(!duplicated(document_number)&!duplicated(document_number,fromLast = TRUE)) %>%
  mutate(po_version = 1, original_created_on = created_on)

### creates the PO version number for all Changed POs

t_docoutput_changePOs %<>% group_by(document_number) %>%
  mutate(po_version = row_number(document_number))

### creates the PO original Created on Date

t_docoutput_changePOs_v1 <- t_docoutput_changePOs %>%
  filter(po_version == 1) %>%
  mutate(original_created_on = created_on)

t_docoutput_cpov1_dateselect <- t_docoutput_changePOs_v1 %>%
  select(document_number, original_created_on)

t_docoutput_changePOs_vup <- t_docoutput_changePOs %>%
  filter(po_version != 1) %>%
  left_join(t_docoutput_cpov1_dateselect)

t_docouput_f1 <- bind_rows(t_docoutput_changePOs_v1,t_docoutput_changePOs_vup,t_docoutput_nochangePOs)

### cleans up the current environment before starting the next part

rm(t_docoutput_cpov1_dateselect, t_docoutput_changePOs_v1,
   t_docoutput_changePOs, t_docoutput_nochangePOs, t_docoutput,
   t_docoutput_changePOs_vup)

### Imports and appends all PO details
setwd("..")
setwd("MM_2_PODETAILS")
dir_podetails <- dir()

t_podetails <- bind_rows(lapply(dir_podetails,read_xlsx))

### sets wd for PO Statuses
setwd("..")
setwd("MM_4_POSTATUS")
dir_postatus <- dir()

### creates the column types for po_status

coltypes_postatus <- ColTypesdigitend(13,2)

### creates the column names for po_status

colnames_postatus <- BWColnames(dir_postatus)

### imports the postatus files

t_postatus <- bind_rows(lapply(dir_postatus,read_delim, delim = ";", 
                               col_types = coltypes_postatus, 
                               col_names = colnames_postatus, skip = 5))

### cleans the postatus column names

colnames(t_postatus) <- CleanColnames(t_postatus)

### sets wd for shopping carts
setwd("..")
setwd("MM_3_SCEXTRACT")
dir_scextract <- dir()

dir_scextract_new <- dir_scextract[startsWith(dir_scextract,"sc")]
dir_scextract_old <- dir_scextract[!startsWith(dir_scextract,"sc")]

scextractold<-read_delim(dir_scextract_old,delim = "|") %>%
  select(1:30)

colnames(scextractold) <- CleanColnames(scextractold)

scextract_new <- lapply()