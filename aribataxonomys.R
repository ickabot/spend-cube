library(readxl)
library(WriteXLS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(readr)
library(glue)
library(magrittr)
library(stringr)
library(forcats)
library(stringdist)
library(lubridate)

colnames(SpendVis_Supplier_Joined) %<>% str_replace_all(" ","_")
colnames(SpendVis_Supplier_Joined) %<>% str_replace_all("[\r\n]","")
colnames(SpendVis_Supplier_Joined) %<>% str_replace_all("#","number")
colnames(SpendVis_Supplier_Joined) %<>% str_replace_all("/","")
colnames(SpendVis_Supplier_Joined) %<>% str_replace_all("[[:punct:]]","")

ariba_taxonomy_inc_gl_descript<-SpendVis_Supplier_Joined %>% 
  select(GLAccountName,ProductCategory,AribaClassificationTaxonomyL1,
         AribaClassificationTaxonomyL2,Description,SourceSystem) %>%
  distinct(.) %>%
  filter(AribaClassificationTaxonomyL1!="Unclassified")

ariba_taxonomy <- SpendVis_Supplier_Joined %>%
  select(AribaClassificationTaxonomyL1,AribaClassificationTaxonomyL2,AribaClassificationTaxonomyL3) %>%
  filter(AribaClassificationTaxonomyL1!="Unclassified") %>%
  distinct(.)

write.csv(ariba_taxonomy,"ariba_taxonomy_L3.csv")

ariba_taxonomy <- SpendVis_Supplier_Joined %>%
  select(AribaClassificationTaxonomyL1) %>%
  filter(AribaClassificationTaxonomyL1!="Unclassified") %>%
  distinct(.)

write.csv(ariba_taxonomy,"ariba_taxonomy_L1.csv")

unknowncategory <- c("Management and Business Professionals and Administrative Services",
"Domestic Appliances and Supplies and Consumer Electronic Products",
"Electrical Systems and Lighting and Components and Accessories and Supplies",
"Industrial Manufacturing and Processing Machinery and Accessories",
"Distribution and Conditioning Systems and Equipment and Components",
"Rental or lease, Maintenance or repair, Installation",
"Power Generation and Distribution Machinery and Accessories")


questionable <- SpendVis_Supplier_Joined %>%
  filter(AribaClassificationTaxonomyL1 %in% unknowncategory) %>%
  select(AribaClassificationTaxonomyL1, SupplierERPSupplier) %>%
  distinct(.)

ariba_taxonomy_L2<-read.csv("ariba_taxonomy_L2.csv", stringsAsFactors = FALSE)

SpendVis_2<-SpendVis_Supplier_Joined

SpendVis_2 %<>% left_join(ariba_taxonomy_L2) %>%
  mutate(catcardcombo=paste(Cat_Card_L2,Cat_Card_L3," ")) %>%
  group_by(catcardcombo) %>%
  sample_n(100)

write.csv(SpendVis_2,"spendvis_2.csv")

Spendvis_undone <- SpendVis_2 %>%
  filter(Cat_Card_L2=) %>%
  select(-InvoiceSpend) %>%
  distinct(.)
