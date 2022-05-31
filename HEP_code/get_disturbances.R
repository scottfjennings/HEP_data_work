

# summarize disturbances to colonies

# 1 packages ----
library(tidyverse)
library(RColorBrewer)
library(RODBC)
library(rgdal)
library(sp)
library(here)
library(birdnames)
options(scipen = 999)

source(here("HEP_code/HEP_utility_functions.R"))
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")

# 2 data ----
zsppz <- c("BCNH", "GBHE", "GREG", "SNEG")

hepdata_location = here("HEP_data/HEPDATA.accdb")
# all these functions are in HEP_report_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location)

hep_disturbance <- hep_start %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% 
  cut_never_nested() %>% 
  select(code, species, year, contains("dist"), -disturbance)


dist_long <- hep_disturbance %>% 
  mutate(across(contains("dist"), ~as.character(.))) %>% 
  pivot_longer(cols = contains("dist"))  %>% 
  mutate(name = gsub('dist','', name),
         dist.num = str_sub(name, 1, 1),
         name = gsub('[[:digit:]]+','', name))


dist <- dist_long %>% 
  pivot_wider(id_cols = c(code, species, year, dist.num)) %>% 
  filter(!(is.na(type) & is.na(result))) %>% 
  full_join(., data.frame(type = c("A", "H", "W", "M", "O", "P", "U"),
                          type.label = c("Avian", "Human", "Weather", "Mammal", "ACR field observer", "Unknown Predator", "unknown"))) %>%
  full_join(data.frame(result = c("0", "1", "2", "3", "4"),
                       result.label = c("none", "behavioral response", "nest failure", "abandonment of colony", "pre-season disturbance")))

  