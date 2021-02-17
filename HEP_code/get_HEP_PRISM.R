
library(tidyverse)
library(prism) # http://ropensci.github.io/prism/
library(raster)
library(rgdal)
library(sf)
library(rgeos)


options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
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

# then bulk download data here: https://prism.oregonstate.edu/explorer/bulk.php
# webpage allows you to upload the list of colony coords created above, select which variables to download for those site, select dates, and download.
# can download monthly values in 15 year chunks, so will end up with multiple files
# currently saving these to HEP_data/hep_prism_data






