
library(tidyverse)
library(prism) # http://ropensci.github.io/prism/
library(raster)
library(rgdal)
library(sf)
library(rgeos)


options(scipen = 999)
source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

hepdata_location = "C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access(hepdata_location)
 
hep_parent_sites <- hep_sites %>% 
  filter(!is.na(utmnorth), !is.na(utmeast), !is.na(parent.code)) %>% 
  group_by(parent.code) %>%
  summarise(utmnorth = mean(utmnorth),
            utmeast = mean(utmeast))

hep_parent_sites_utm <- st_as_sf(hep_parent_sites, coords = c("utmeast", "utmnorth"), crs = "EPSG:32610")

hep_parent_sites_dd <- st_transform(hep_parent_sites_utm, CRS("+init=epsg:4326"))

hep_parent_sites_dd_df <- hep_parent_sites_dd  %>% 
  dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                latitude = sf::st_coordinates(.)[,2]) %>% 
  data.frame() %>% 
  dplyr::select(-geometry) %>% 
  relocate(latitude, longitude, parent.code)

write.csv(hep_parent_sites_dd_df, "HEP_data/hep_parent_sites_dd_df.csv", row.names = F)

write.csv(hep_parent_sites_dd_df, "HEP_data/hep_parent_sites_dd_df_nohead.csv", row.names = F, col.names = FALSE)

read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/data/all_sfbbo_sites_subreg.csv") %>% 
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

hep_prism_85_89 <- read.csv("HEP_data/hep_prism_data/PRISM_ppt_stable_4km_198501_198912.csv", skip = 10)
hep_prism_90_04 <- read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_199001_200412.csv", skip = 10)
hep_prism_05_19 <- read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_200501_201912.csv", skip = 10)
hep_prism_20_21 <- read.csv("HEP_data/hep_prism_data/PRISM_ppt_provisional_4km_202001_202307.csv", skip = 10)

hep_prism <- bind_rows(read.csv("HEP_data/hep_prism_data/PRISM_ppt_stable_4km_198501_198912.csv", skip = 10),
                       read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_199001_200412.csv", skip = 10),
                       read.csv("HEP_data/hep_prism_data/hep_PRISM_ppt_stable_4km_200501_201912.csv", skip = 10),
                       read.csv("HEP_data/hep_prism_data/PRISM_ppt_provisional_4km_202001_202307.csv", skip = 10)) %>% 
  separate(Date, c("year", "month"), sep = "-", remove = F) %>% 
  rename(rain.mm = ppt..mm.) %>% 
  dplyr::select(parent.code = Name, year, month, rain.mm)



sfbbo_prism <- bind_rows(read.csv("HEP_data/hep_prism_data/sfbbo_PRISM_ppt_stable_4km_199501_200912.csv", skip = 10),
                         read.csv("HEP_data/hep_prism_data/sfbbo_PRISM_ppt_stable_4km_201001_201901.csv", skip = 10),
                         read.csv("HEP_data/hep_prism_data/sfbbo_PRISM_ppt_stable_4km_199001_199412.csv", skip = 10)) %>% 
  rename("prism.index" = Name) %>% 
  full_join(read.csv("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/data/all_sfbbo_sites_subreg.csv") %>%
              dplyr::select(site.name) %>%
              mutate(prism.index = row_number())) %>%
  separate(Date, c("year", "month"), sep = "-", remove = F) %>% 
  rename(rain.mm = ppt..mm.) %>% 
  select(site.name, year, month, rain.mm)

summary(hep_prism)

saveRDS(hep_prism, "HEP_data/hep_prism_data/hep_prism_combined")         

saveRDS(sfbbo_prism, "HEP_data/hep_prism_data/sfbbo_prism_combined")         


# summarize rain data by HEP subregion ----
# this copied and generalized from C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/hep_analyses/how_are_the_egrets_doing/code/ms_analysis/rain_varbs.R

start.year = 1989
end.year = 2023


source("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

# all these functions are in HEP_utility_functions.R
hep_sites <- hep_sites_from_access("C:/Users/scott.jennings/OneDrive - Audubon Canyon Ranch/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_data/HEPDATA.accdb") %>% 
  distinct(parent.code, parent.site.name, subregion)

# data/all_sfbbo_sites_subreg.csv has been hand edited in excel to have no missing subregions
sites_subreg <- read.csv(here("HEP_data/all_sfbbo_sites_subreg.csv")) %>% 
  dplyr::select(parent.site.name = site.name, subregion) %>% 
  bind_rows(hep_sites %>% dplyr::select(parent.site.name, subregion))


colony_rain_season <- readRDS("HEP_data/hep_prism_data/hep_prism_combined") %>% 
  left_join(hep_sites) %>% 
  bind_rows(readRDS("HEP_data/hep_prism_data/sfbbo_prism_combined") %>% 
              rename("parent.site.name" = site.name)) %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  mutate(birdyear = ifelse(month <= 9, year, year + 1),
         rain.season = case_when(between(month, 3, 6) ~ "spring", # Mar, Apr, May, Jun = spring
                                 between(month, 7, 10) ~ "dry", # Jul, Aug, Sep, Oct = dry season
                                 month > 10 | month < 3 ~ "winter")) %>% # Nov, Dec, Jan, Feb, Mar = winter 
  group_by(parent.site.name, birdyear, rain.season) %>% 
  summarise(season.year.rain = sum(rain.mm)) %>% 
  ungroup()


colony_rain <- readRDS("HEP_data/hep_prism_data/hep_prism_combined") %>% 
  left_join(hep_sites) %>% 
  bind_rows(readRDS("HEP_data/hep_prism_data/sfbbo_prism_combined") %>% 
              rename("parent.site.name" = site.name)) %>% 
  mutate(month = as.numeric(month),
         year = as.numeric(year)) %>% 
  mutate(birdyear = ifelse(month <= 6, year, year + 1)) %>% 
  group_by(parent.site.name, birdyear) %>% 
  summarise(colony.year.rain = sum(rain.mm)) %>% 
  ungroup() 

subreg_rain <- colony_rain %>% 
  left_join(sites_subreg) %>% 
  bind_rows(colony_rain %>% mutate(subregion = "All")) %>% 
  group_by(subregion, birdyear) %>% 
  summarise(subreg.rain = mean(colony.year.rain)) %>% 
  ungroup() %>% 
  group_by(subregion) %>% 
  mutate(mean.subreg.rain = mean(subreg.rain),
         rain.dev = subreg.rain - mean.subreg.rain,
         rain.dev.label = ifelse(rain.dev > 0, "Above average", "Below average")) %>% 
  full_join(subreg_key)

saveRDS(subreg_rain, here("HEP_data/subreg_rain"))



### summarize, visualize


subreg_rain %>% 
  filter(between(birdyear, start.year, end.year)) %>% 
  ggplot() +
  geom_col(aes(x = birdyear, y = subreg.rain)) +
  #scale_fill_manual(values = c("blue", "red")) +
  stat_smooth(aes(x = birdyear, y = subreg.rain), method = "lm", color = "black", se = FALSE, size = .5) +
  facet_wrap(~subreg.name, labeller = labeller(subreg.name = label_wrap_gen(30))) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), labels = seq(1995, 2020, by = 5)) +
  theme_bw() +
  labs(x = "Year",
       y = "Annual rainfall (mm)",
       fill = "") +
  theme(text = element_text(size=8))

ggsave(here("figures/subregion_rain_plot.png"), width = 7, height = 4, dpi = 300)


subreg_rain %>% 
  ggplot() +
  geom_col(aes(x = birdyear, y = rain.dev, fill = rain.dev.label)) +
  scale_fill_manual(values = c("blue", "red")) +
  stat_smooth(aes(x = birdyear, y = rain.dev), method = "lm", color = "black", se = FALSE, size = .5) +
  facet_wrap(~subreg.name, labeller = labeller(subreg.name = label_wrap_gen(30))) +
  scale_x_continuous(breaks = seq(1995, 2020, by = 5), labels = seq(1995, 2020, by = 5)) +
  theme_bw() +
  labs(x = "Year",
       y = "Deviation from average rainfall",
       fill = "") +
  theme(text = element_text(size=8))

ggsave(here("figures/rain_deviation.png"), width = 7, height = 4)







