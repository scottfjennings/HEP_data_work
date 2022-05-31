
# 1 packages ----
library(tidyverse)
library(RColorBrewer)
library(RODBC)
library(rgdal)
library(sp)
library(birdnames)
options(scipen = 999)

source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")

# 2 data ----
zsppz <- c("BCNH", "GBHE", "GREG", "SNEG")

hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_report_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location)

#


# export shapefile for colony locations ----
hep_sites <- hep_sites %>% 
  filter(!is.na(utmnorth), !is.na(utmeast))
hep_sites_utm <- SpatialPointsDataFrame(cbind(hep_sites$utmeast, hep_sites$utmnorth), proj4string = CRS("+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"), data = hep_sites)
hep_sites_dd <- spTransform(hep_sites_utm, CRS("+init=epsg:4326"))
writeOGR(obj=hep_sites_dd, dsn="C:/Users/scott.jennings/Documents/Projects/HEP/HEP_spatial/data", layer="hep_sites", driver="ESRI Shapefile") # this is in geographical projection


hep <- hep_start %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% 
  left_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% 
  trim_hep_columns(disturbance = TRUE, stage = FALSE, meta = TRUE) %>% 
  cut_never_nested() %>% 
  mutate(site.name = as.character(site.name))
  

hep %>% 
  filter(species %in% zsppz, numbervisits < 10) %>% 
 ggplot() + geom_point(aes(x = (totalhours/numbervisits), y = peakactvnsts, color = numbervisits)) + facet_wrap(~species, scales = "free")




hep_start %>% 
  filter(SPECIES %in% zsppz) %>% 
 ggplot() + geom_point(aes(x = ((LTMARSTAGE1 + LTMARSTAGE2 + LTMARSTAGE3)/(MAYSTAGE4 + MAYSTAGE5)), y = (FOCFAILURE/FOCALNESTS))) + facet_wrap(~SPECIES, scales = "free")



### tomales bay colonies
tom_site_names <- hep_sites %>% 
  filter(code %in% c(32.0, 32.1, 50.0, 50.1, 50.2, 50.3, 83.0, 114.0, 119.0, 122.0, 143.0, 152.0, 160.0, 160.1)) %>% 
  distinct(code, parent.site.name, parent.code)


tom_greg_year <- filter(hep, code %in% c(32.0, 32.1, 50.0, 50.1, 50.2, 50.3, 83.0, 114.0, 119.0, 122.0, 143.0, 152.0, 160.0, 160.1), species == "GREG", peakactvnsts > 0, year > 2016) %>% 
  group_by(year) %>% 
  summarise(tot.peak.year = sum(peakactvnsts),
            num.col = n()) %>% 
  ungroup() 

tom_greg_year %>% 
  summarise(mean.peak = mean(tot.peak.year),
            num.yr = n(),
            se.peak = sd(tot.peak.year)/sqrt(num.yr))
  
filter(hep, code == 83.0, species == "GREG", peakactvnsts > 0, year > 2016) %>% 
  group_by(year) %>% 
  summarise(tot.peak.year = sum(peakactvnsts),
            num.col = n()) %>% 
  ungroup()  %>% 
  summarise(mean.peak = mean(tot.peak.year),
            num.yr = n(),
            se.peak = sd(tot.peak.year)/sqrt(num.yr))


filter(hep, code %in% c(32.0, 32.1, 50.0, 50.1, 50.2, 50.3, 83.0, 114.0, 119.0, 122.0, 143.0, 152.0, 160.0, 160.1), species == "GREG", !is.na(peakactvnsts)) %>%
  dplyr::select(code, peakactvnsts, year) %>% 
  left_join(., tom_site_names) %>% 
  group_by(parent.site.name, year) %>% 
  summarise(peakactvnsts = sum(peakactvnsts)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = peakactvnsts, color = as.factor(parent.site.name))) +
  geom_point(aes(x = year, y = peakactvnsts, color = as.factor(parent.site.name))) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ggtitle("Number of GREG nests at Tomales Bay colonies") +
  ylab("Number of nests")

ggsave("figures_output/tomales_bay_nest_abundance.png", width = 12, height = 8)
