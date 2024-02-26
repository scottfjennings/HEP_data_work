

# summarize disturbances to colonies

# 1 packages ----
library(tidyverse)
library(lubridate)
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
                       result.label = c("none", "behavioral response", "nest failure", "abandonment of colony", "pre-season disturbance"))) %>% 
  arrange(code, date, species, dist.num)


type_results <- dist %>% 
  filter(!is.na(date)) %>% 
  mutate(dist.result.type = paste(type, result, sep = "_")) %>% 
  pivot_wider(id_cols = c(code, date, dist.num), names_from = species, values_from = dist.result.type) %>% 
  data.frame()
  arrange(code, date) %>% 
  mutate_all(as.character()) 


write.csv(type_results, here("HEP_data/all_disturbance_types_results.csv"), row.names = FALSE)
type_results <- read.csv(here("HEP_data/all_disturbance_types_results.csv"))

  
dist %>% 
  filter(!is.na(date), result == 4) %>% 
  mutate(month = month(date),
         day = day(date)) %>% 
  arrange(month, day) %>% view()
  
dist %>% 
  filter(!is.na(date)) %>% 
  mutate(result = as.numeric(result)) %>% 
  group_by(code, date) %>% 
  summarise(zresult = mean(result, na.rm = TRUE)) %>% 
  filter(zresult != 0, zresult != 1, zresult != 2, zresult != 3, zresult != 4) %>% view()
  select(-zresult) %>% 
  left_join(dist) %>% 
  view()

  
  
# disturbance by year and subregion
  
  dist %>% 
    group_by(year, code) %>% 
    summarise(num.dist = n()) %>% 
    full_join(hep_sites %>%  select(code, subregion)) %>% 
    group_by(year, subregion) %>% 
    summarise(mean.dist.per.col = mean(num.dist)) %>%
    filter(year > 1994) %>% 
    ggplot() +
    geom_point(aes(x = year, y = mean.dist.per.col)) +
    stat_smooth(aes(x = year, y = mean.dist.per.col)) +
    facet_wrap(~subregion)

  
  dist %>% 
    full_join(hep_sites %>%  select(code, subregion)) %>% 
    filter(subregion == "CSF") %>% 
    arrange(year, code, species)%>% view()
  
  