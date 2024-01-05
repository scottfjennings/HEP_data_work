

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
  select(year = YEAR, code =  CODE, species = SPECIES, peakactvnsts = PEAKACTVNSTS) %>% 
  left_join(., select(hep_sites, code, site.name)) 



saveRDS(hep, "HEP_data/hep_annual_nest_abundance")



hep <- readRDS("HEP_data/hep_annual_nest_abundance")



