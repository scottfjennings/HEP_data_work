
# 1 packages ----
library(tidyverse)
library(RColorBrewer)
library(RODBC)
options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")

# 2 data ----
zsppz <- c("BCNH", "GBHE", "GREG", "SNEG")

hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_report_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location)

hep <- hep_start %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% 
  left_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% 
  trim_hep_columns(disturbance = TRUE, stage = FALSE, meta = TRUE) %>% 
  cut_never_nested() %>% 
  mutate(site.name = as.character(site.name))
  

hep %>% 
  filter(species %in% zsppz, numbervisits < 10) %>% 
 ggplot() + geom_point(aes(x = (totalhours/numbervisits), y = peakactvnsts, color = numbervisits)) + facet_wrap(~species, scales = "free")




hep_start %>% 
  filter(SPECIES %in% zsppz) %>% 
 ggplot() + geom_point(aes(x = ((LTMARSTAGE1 + LTMARSTAGE2 + LTMARSTAGE3)/(MAYSTAGE4 + MAYSTAGE5)), y = (FOCFAILURE/FOCALNESTS))) + facet_wrap(~SPECIES, scales = "free")


