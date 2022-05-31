

# this code made for the following data request from Emi on 2/28/22
# Hi Scott,
# Would it be possible for you to pull a query for me from the HEP data that you have wrangled…?
# I need a list of all sites, the last year they were visited, the status on that visit, and if they are inactive, how many years have they been inactive since (and including) the most recent visit.
# This would be super helpful.
# Cheers,
# Emi



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


hep2020 <- readRDS("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/rawhep_to_HEPDATA/data/as_HEPDATA/HEPDATA_2020") %>% 
  mutate(across(c(JULSTGEDATE, DIST1DATE, DIST2DATE, DIST3DATE, DIST4DATE, DIST5DATE, DIST6DATE, DIST7DATE, DIST8DATE, DIST9DATE), ~ as.Date(.)),
         across(contains("RESULT"), ~ as.character(.)),
         across(contains("NESTING"), ~ as.character(.)))


hep <- hep_start %>% 
  mutate(across(contains("RESULT"), ~ as.character(.)),
         across(contains("NESTING"), ~ as.character(.))) %>% 
  bind_rows(., hep2020) %>% 
  select(year = YEAR, code =  CODE, species = SPECIES, peakactvnsts = PEAKACTVNSTS, numbervisits = NUMBERVISITS) %>% 
  left_join(., select(hep_sites, code, site.name)) 

most_recent_active <- hep %>% 
  cut_never_nested() %>% 
  filter(peakactvnsts > 0) %>% 
  group_by(site.name,code) %>% 
  summarise(most.recent.active = max(year)) %>% 
  arrange(site.name)

most_recent_visit <- hep %>% 
  cut_never_nested() %>% 
  ungroup() %>% 
  group_by(site.name, code) %>%  
  summarise(most.recent.visit = max(year)) %>% 
  arrange(site.name)


hep_colony_status <- full_join(most_recent_active, most_recent_visit) %>% 
  mutate(active.most.recent.visit = most.recent.visit == most.recent.active,
         innactive.survey.gap = ifelse(active.most.recent.visit == FALSE, most.recent.visit - most.recent.active, NA),
         years.since.last.visit = year(Sys.Date()) - most.recent.visit,
         years.since.last.active = year(Sys.Date()) - most.recent.active)



  write.csv(hep_colony_status, here("HEP_data/hep_colony_status.csv"), row.names = FALSE)

  

  