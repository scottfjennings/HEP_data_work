



library(tidyverse)
library(devtools)
library(RColorBrewer)
library(here)
library(plotly)
library(sp)
library(gganimate)

options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/numbers2words.R")
source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")
# source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")


report.year = 2019




hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location) 
parent_sites <- hep_sites %>% 
  filter(!is.na(utmnorth), !is.na(utmeast), !is.na(parent.code)) %>% 
  group_by(parent.code, parent.site.name) %>%
  summarise(utmnorth = mean(utmnorth),
            utmeast = mean(utmeast))

parent_sites_utm <- SpatialPointsDataFrame(coords = cbind(parent_sites$utmeast, parent_sites$utmnorth), data = parent_sites, proj4string = CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
parent_sites_dd <- spTransform(parent_sites_utm, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
parent_sites_dd_df <- data.frame(parent_sites_dd) %>% 
  rename(long = coords.x1, lat = coords.x2)

hep_changes <- hep_start %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% # remove "no data" records
  #left_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% # add human readable colony names
  trim_hep_columns() %>% # remove set blocks of columns
  cut_never_nested() %>% # remove all records for colony X species that were never really active _ cut artifact of "complete" HEPDATA
  left_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion, utmnorth, utmeast))  %>% # add human readable colony names
  mutate(site.name = as.character(site.name)) %>% 
  hep_annual_changer() %>% # aggregates to parent code spatial scale
  filter(year >= 1990) %>% 
  bird_taxa_filter(drop_cols = c("species.number", "order", "family", "subfamily", "genus", "species")) %>% 
  rename(species = alpha.code, spp.name = common.name) %>% 
  mutate(per.change.1year = ifelse(zero2zero, 0, per.change.1year),
         per.change.1year = ifelse(zero2some, abs.change.1year * 100, per.change.1year)) %>% 
  distinct()


spp_cols <- hep_changes %>% 
  distinct(species, parent.code)

hep_years <- seq(1990, max(hep_changes$year))

greg_filled_years <- spp_cols %>% 
  filter(species == "GREG") %>% 
  slice(rep(1:n(), each = length(hep_years))) %>% 
  mutate(year = rep(hep_years, ))

col_plot <- hep_changes %>% 
  #filter(spp.name %in% core4spp) %>% 
  filter(species == "GREG") %>% 
  left_join(., parent_sites_dd_df) %>% 
  ggplot() +
  geom_point(aes(x = long, y = lat, size = peakactvnsts)) +
  facet_wrap(~spp.name)


col_plot +
  transition_time(year) +
  labs(title = "Year: {frame_time}")

anim_save("figures_output/colony_size_annimation.gif", width = 8)
