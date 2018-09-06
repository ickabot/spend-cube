
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

### seperates old and new scextract files

dir_scextract_new <- dir_scextract[startsWith(dir_scextract,"sc")]
dir_scextract_old <- dir_scextract[!startsWith(dir_scextract,"sc")]

### imports the old sc_extract file from when mary did it

t_scextractold_head <-na.omit(as.character(read_delim(dir_scextract_old, delim = "|", n_max = 1, col_names = FALSE))) 

length(t_scextractold)
r <- paste0(rep("c", 43), collapse = "")

t_scextractold <- read_delim(dir_scextract_old, delim = "|", col_types = r) %>%
  select(1:30)

### cleans colnames for scextract file

colnames(t_scextractold) <- CleanColnames(t_scextractold)

### imports the scextract file

t_scextract_new <- BWImport(dir_scextract_new)

t_scextract_new <- MeasureColumns(t_scextract_new)

### sets wd for shopping cart approval
setwd("..")
setwd("MM_6_OASC")
dir_oasc <- dir()

### imports and cleans the scextract file

testoasc <- BWImport()

### cleans the columns for the sc_extract file

testoasc %<>% MeasureColumns()

### sets wd for aravo reg
setwd("..")
setwd("..")
setwd("5_Aravo/MM_1_PLB")
dir_PLB <- dir()

t_AravoPLB <- read_csv(dir_PLB, col_types = "ccc")

### sets wd for contracts

setwd("G:/SOURCING-CENTRAL/3 - Executed Contracts")

### reads each of the shared drive folders

setwd("1 - Cruises")

cruise_contracts <- directoryPrint()

setwd("..")
setwd("2 - Event Contracts")

event_contracts <- directoryPrint()

setwd("..")
setwd("3 - Performance Agreements")

performance_contracts <- as.data.frame(dir(), stringsAsFactors = FALSE)

setwd("..")
setwd("4 - Freelance Agreements")

freelance_contracts <- as.data.frame(dir(), stringsAsFactors = FALSE)

setwd("..")
setwd("5 - Residency Agreements")

residency_contracts <- as.data.frame(dir(), stringsAsFactors = FALSE)

shared_drive_contracts <- bind_rows(cruise_contracts, event_contracts, performance_contracts,
                                    freelance_contracts, residency_contracts)

rm(residency_contracts, freelance_contracts, performance_contracts,
   event_contracts, cruise_contracts)

colnames(shared_drive_contracts) <- CleanColnames(shared_drive_contracts)

sdcontracts <- shared_drive_contracts %>%
 separate(dir,c("BUYER","DATE"),extra = "drop", fill = "left")

### reads the ariba contracts file

