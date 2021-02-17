


# calculate the distances between all hep colonies


# 1 packages ----
library(tidyverse)
library(RColorBrewer)
library(RODBC)
library(sf)
options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")

# 2 data ----
zsppz <- c("BCNH", "GBHE", "GREG", "SNEG")

hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location) 

projcrs <- "+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

hep_site_sf <- hep_sites %>% 
  filter(!is.na(utmnorth), !is.na(utmeast))%>% 
  select(code, utmeast, utmnorth) %>% 
  st_as_sf(coords = c("utmeast", "utmnorth"),
                          crs = projcrs) 

hep_distances <- hep_site_sf %>% 
  st_distance() %>% 
  data.frame() %>% 
  rownames_to_column("col.ind")


hep_distances2 <- hep_site_sf %>% 
  data.frame() %>% 
  select(code) %>% 
  cbind(hep_distances)

hep_distances_long <- hep_distances2 %>% 
  pivot_longer(cols = contains("X")) %>% 
  mutate(name = gsub("X", "", name)) %>% 
  rename(code.from = code, distance.to.colony = value) %>% 
  full_join(., hep_distances2 %>%
              data.frame() %>% 
              select(code.to = code, name = col.ind)) %>% 
  select(code.from, code.to, distance.to.colony, -col.ind, -name) %>% 
  arrange(distance.to.colony, code.from)


 