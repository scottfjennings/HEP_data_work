

library(tidyverse)
library(xlsx)
library(here)

hep_tracking <- read.xlsx(here("HEP_data/HEP Tracking Spreadsheet2_shared.xlsx"), sheetName = "2022") 

# number of sites
hep_tracking %>% 
  count(Status_2021)


# number of observers
hep_tracking %>% 
  filter(!LAST.NAME %in% c("Lumpkin", "Jennings", "Condeso", "Wechsberg")) %>% 
  distinct(LAST.NAME) %>% 
  nrow()
