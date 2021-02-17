

# calculate average phenology for ardeid nesting in north SF bay area (ACR HEP study area)

# for each species, make a model to predict what proportion of nests is expected to be in each stage on a particular date

# packages, source

library(tidyverse)
library(lubridate)
source("HEP_code/HEP_utility_functions.R")



hep <- hep_from_access() %>% 
  clean_hep() %>% 
  add_site_names() %>% 
  cut_never_nested() %>% 
  cut_leading_0s() %>% 
  trim_hep_columns(stage = F)


# function to count the number of nests in each stage on each of the standard survey periods
# this is needed to account for variation in survey date within each survey period when evelauating phenology
tally_stages=function(df) {
#df=hep  
stage_nests_long <- df %>% 
  dplyr::select(code, year, species, contains("stage")) %>% 
  pivot_longer(cols = contains("stage"), names_to = c("month", "stage"), names_sep = ("stage"), values_to = ("num.nests")) %>% 
  filter(!is.na(num.nests))
 
stage_date_long <- df %>% 
  dplyr::select(code, year, species, contains("stg")) %>% 
  pivot_longer(cols = contains("stg"), names_to = ("month"), values_to = ("stage.date")) %>% 
  mutate(month = gsub("stgedate", "", month),
         month = gsub("stgdate", "", month)) %>% 
  filter(!is.na(stage.date))
 
stage_brood_date <- left_join(stage_nests_long, stage_date_long)
return(stage_brood_date)
}


proportion_stages <- function(df) {
 
  df <- df %>% 
    group_by(code, stage.date, species) %>% 
    mutate(total.nests = sum(num.nests),
           prop.stage = num.nests/total.nests,
           jdate = yday(stage.date)) %>% 
    ungroup()
  
}


hep_prop_stages <- hep %>% 
  tally_stages() %>% 
  filter(!is.na(stage.date))%>% 
  proportion_stages()

zzz <- hep_prop_stages %>% 
  filter(!species %in% c("DCCO", "CAEG"), !is.na(stage), !is.na(stage.date)) 


%>% 
ggplot() +
  geom_smooth(aes(x = as.Date(jdate, origin = as.Date("2018-01-01")), y = prop.stage, color = stage), span = 0.8, method = "loess", se = FALSE) +
  facet_wrap(~species) +
  xlab("") +
  ylab("Proportion of nests") +
  scale_x_date(date_labels = "%b %d") +
  theme_bw() +
  ggtitle("How does the proportion of nests assigned to each stage change through the season?")

ggsave("figures_output/stage_proportions.png", width = 9, height = 6, dpi = 300, units = "in")
