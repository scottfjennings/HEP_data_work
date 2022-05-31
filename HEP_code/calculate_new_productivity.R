
# 1 packages ----
library(tidyverse)
library(RODBC)
library(here)
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
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% 
  right_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% 
  trim_hep_columns(stage = FALSE) %>% 
  cut_never_nested() %>% 
  mutate(site.name = as.character(site.name)) 

new_productivity <- hep %>% 
  select(year, species, code, contains("brd")) %>% 
  mutate(across(contains("brd"), ~replace_na(., 0)),
         total.brd.nests = brd1 + brd2 + brd3 + brd4 + brd5 + brd6) %>% 
  group_by(year, species, code) %>% 
  mutate(mean.chx.per.nest = mean(c(rep(1, brd1), rep(2, brd2), rep(3, brd3), rep(4, brd4), rep(5, brd5), rep(6, brd6))),
         sd.chx.per.nest = sd(c(rep(1, brd1), rep(2, brd2), rep(3, brd3), rep(4, brd4), rep(5, brd5), rep(6, brd6)))) %>% 
  ungroup() %>% 
  mutate(st.err.chx.per.nest = sqrt(sd.chx.per.nest/total.brd.nests))


saveRDS(new_productivity, here("HEP_data/hep_annual_new_productivity"))
