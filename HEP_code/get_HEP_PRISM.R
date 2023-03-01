
library(tidyverse)
library(prism) # http://ropensci.github.io/prism/
library(raster)
library(rgdal)
library(sf)
library(rgeos)


options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

hepdata_location = "C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location)
 
hep_parent_sites <- hep_sites %>% 
  filter(!is.na(utmnorth), !is.na(utmeast), !is.na(parent.code)) %>% 
  group_by(parent.code) %>%
  summarise(utmnorth = mean(utmnorth),
            utmeast = mean(utmeast))

hep_parent_sites_utm <- SpatialPoints(cbind(hep_parent_sites$utmeast, hep_parent_sites$utmnorth), proj4string = CRS('+proj=utm +zone=10 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))

hep_parent_sites_dd <- spTransform(hep_parent_sites_utm, CRS("+init=epsg:4326"))

hep_parent_sites_dd_df <- hep_parent_sites_dd@coords %>% 
  data.frame() %>% 
  cbind(hep_parent_sites$parent.code)

write.csv(hep_parent_sites_dd_df, "HEP_data/hep_parent_sites_dd_df.csv", row.names = F)

read.csv("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/data/all_sfbbo_sites_subreg.csv") %>% 
  dplyr::select(Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         prism.index = row_number()) %>% 
  write.csv("HEP_data/sfbbo_sites_dd_df.csv", row.names = F)


# then bulk download data here: https://prism.oregonstate.edu/explorer/bulk.php
# webpage allows you to upload the list of colony coords created above, select which variables to download for those site, select dates, and download.
# NOTE, CSV CANNOT HAVE COLUMN HEADERS!!!!
# can download monthly values in 15 year chunks, so will end up with multiple files
# currently saving these to HEP_data/hep_prism_data


# next, consolidate those files

hep_prism_90_04 <- read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_199001_200412.csv", skip = 10)
hep_prism_05_19 <- read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_200501_201912.csv", skip = 10)
hep_prism_20_21 <- read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_tmean_provisional_4km_202001_202112.csv", skip = 10)

hep_prism <- bind_rows(read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_199001_200412.csv", skip = 10),
                   read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_200501_201912.csv", skip = 10),
                   read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_tmean_provisional_4km_202001_202112.csv", skip = 10)) %>% 
  separate(Date, c("year", "month"), sep = "-", remove = F) %>% 
  rename(rain.mm = ppt..mm.) %>% 
  select(parent.code = Name, year, month, rain.mm)



sfbbo_prism <- bind_rows(read.csv("HEP_data/hep_prism_data/sfbbo_PRISM_ppt_stable_4km_199501_200912.csv", skip = 10),
                         read.csv("HEP_data/hep_prism_data/sfbbo_PRISM_ppt_stable_4km_201001_201901.csv", skip = 10),
                         read.csv("HEP_data/hep_prism_data/sfbbo_PRISM_ppt_stable_4km_199001_199412.csv", skip = 10)) %>% 
  rename("prism.index" = Name) %>% 
  full_join(read.csv("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/data/all_sfbbo_sites_subreg.csv") %>%
              dplyr::select(site.name) %>%
              mutate(prism.index = row_number())) %>%
  separate(Date, c("year", "month"), sep = "-", remove = F) %>% 
  rename(rain.mm = ppt..mm.) %>% 
  select(site.name, year, month, rain.mm)

summary(hep_prism)

saveRDS(hep_prism, "HEP_data/hep_prism_data/hep_prism_combined")         

saveRDS(sfbbo_prism, "HEP_data/hep_prism_data/sfbbo_prism_combined")         

