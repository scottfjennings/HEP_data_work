library(tidyverse)
library(RODBC)
library(here)

options(scipen = 999)
source(here("HEP_code/HEP_utility_functions.R"))
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")

hep_sites_from_access(here("HEP_data/HEPDATA.accdb")) %>% 
  dplyr::select(code, site.name, utmnorth, utmeast) %>% 
  saveRDS(here("HEP_data/HEP_site_names_nums_utm"))
