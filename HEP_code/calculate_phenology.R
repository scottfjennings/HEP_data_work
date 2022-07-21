# 1 packages ----
library(tidyverse)
library(RODBC)
library(here)
library(lubridate)
library(birdnames)

options(scipen = 999)
source(here("HEP_code/HEP_utility_functions.R"))
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")


hepdata_location = here("HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location) 


hep <- hep_start %>% 
  append_as_hepdata() %>% 
  mutate(across(contains("RESULT"), ~ as.character(.)),
         across(contains("NESTING"), ~ as.character(.))) %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% 
  right_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% 
  trim_hep_columns(stage = FALSE) %>% 
  cut_never_nested() %>% 
  mutate(site.name = as.character(site.name)) 

phenology <- hep %>% 
  filter(species %in% c("GBHE", "GREG")) %>% 
  mutate(maystgedate = as.character(maystgedate),
         junstgedate = as.character(junstgedate),
         phen.stgedate = ifelse(species == "GBHE", maystgedate, junstgedate),
         phen.stgedate = as.POSIXct(phen.stgedate, origin = "1970-01-01"),
         phen.stage1 = ifelse(species == "GBHE", maystage1, junstage1),
         phen.stage2 = ifelse(species == "GBHE", maystage2, junstage2),
         phen.stage3 = ifelse(species == "GBHE", maystage3, junstage3),
         phen.stage4 = ifelse(species == "GBHE", maystage4, junstage4),
         phen.stage5 = ifelse(species == "GBHE", maystage5, junstage5)) %>% 
  select(year, species, code, contains("phen")) %>% 
  mutate(across(contains("phen.stage"), ~replace_na(., 0)),
         phen.prop.unguarded = (phen.stage4 + phen.stage5)/(phen.stage1 + phen.stage2 + phen.stage3 + phen.stage4 + phen.stage5),
         phen.prop.unguarded.se = sqrt((phen.prop.unguarded *(1-phen.prop.unguarded)) / (phen.stage1 + phen.stage2 + phen.stage3 + phen.stage4 + phen.stage5)))



saveRDS(phenology, here("HEP_data/hep_annual_phenology"))
