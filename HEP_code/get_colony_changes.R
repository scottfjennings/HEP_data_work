


# visualize dynamics of each colony as a sequence of eventsL abandoned, colonized, abundance down, abundance up, abundance stable


# 1 packages ----
library(tidyverse)
library(lubridate)
library(RODBC)
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

hep <- hep_start %>%  
  append_as_hepdata() %>% 
  clean_hep() %>% 
  mutate(peakactvnsts = as.numeric(peakactvnsts)) %>% 
  filter(peakactvnsts >= 0) %>% 
  left_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% 
  trim_hep_columns(disturbance = TRUE, stage = FALSE, meta = TRUE) %>% 
  cut_never_nested() %>% 
  mutate(site.name = as.character(site.name))
  

# want to ID cases where a colony became abandoned, and 

colony_changes <- hep %>%
  left_join(., select(hep_sites, code, parent.code, site.name)) %>%
  select(code, parent.code, species, year, peakactvnsts) %>% 
  group_by(year, code, species) %>% 
  summarise(peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  arrange(code, species, year) %>% 
  group_by(code, species) %>% 
  mutate(consec.years = year-lag(year) == 1,
         prev.yr.nsts = ifelse(consec.years == TRUE, lag(peakactvnsts), NA),
         per.change = ifelse(consec.years == TRUE, 100 * ((peakactvnsts/lag(peakactvnsts)-1)), NA),
         colony.status = case_when(consec.years == TRUE & peakactvnsts == 0 & lag(peakactvnsts > 0) ~ "abandoned",
                                   consec.years == TRUE & peakactvnsts == 0 & lag(peakactvnsts == 0) ~ "stay.abandoned",
                                   consec.years == TRUE & peakactvnsts > 0 & lag(peakactvnsts > 0) ~ "persist",
                                   consec.years == TRUE & peakactvnsts > 0 & lag(peakactvnsts == 0) ~ "colonized"),
         change.dir = case_when(per.change < 0 ~ "down",
                                per.change > 0 ~ "up",
                                per.change == 0 ~ "stable"),
         change.run = data.table::rleid(change.dir)) %>% 
  ungroup() %>% 
  group_by(code, species, change.run) %>% 
  mutate(change.run.index = row_number(change.dir),
         change.run.length = max(change.run.index)) %>% 
  arrange(code, species, year) %>% 
  ungroup() %>% 
  mutate(colony.dynamic.label = ifelse(colony.status == "persist", change.dir, colony.status),
         colony.dynamic.label = gsub("stay.", "", colony.dynamic.label),
         colony.dynamic.label = case_when(is.na(colony.dynamic.label) & peakactvnsts == 0 ~ "abandoned",
                                          is.na(colony.dynamic.label) & peakactvnsts > 0 ~ "stable",
                                          TRUE ~ as.character(colony.dynamic.label)))

saveRDS(colony_changes, here("HEP_data/colony_changes_bycode"))

abandoned_colonies <- colony_changes %>%
  filter(colony.status == "abandoned") %>% 
  distinct(parent.code, species)


colony_changes %>% 
  right_join(., abandoned_colonies) %>% 
  filter(species == "GREG", year > 1990) %>% 
  left_join(., select(hep_sites, parent.code, parent.site.name)) %>% 
ggplot(group = code) +
  geom_point(aes(x = year, y = parent.site.name, color = colony.dynamic.label), size = 3) +
  facet_grid(~species) +
  theme_bw()








