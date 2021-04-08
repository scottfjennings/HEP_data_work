


library(tidyverse)

# check for multiple surveys at same site and same time
# check for time span between surveys
# check observers against expected site


s123_file = "HEP_2020_0"

s123 <- read.csv(
  paste("HEP_data/", s123_file, ".csv", sep = "")
  ) %>% 
  select(-starts_with(c("X.", "Tally.", "A.nest.is.active", "Re.locate.the")))


observers <- s123 %>% 
  select(ï..ObjectID, GlobalID, contains("Observer")) %>% 
  pivot_longer(cols = contains("Observer"), names_to = "role", values_to = "name") %>% 
  filter(!is.na(name), name != "")

survey_dates <- s123 %>% 
  select(ï..ObjectID, GlobalID, Select.Colony, contains("Time"))


predators  <- s123 %>% 
  select(ï..ObjectID, GlobalID, contains("redator"))


disturbance  <- s123 %>% 
  select(ï..ObjectID, GlobalID, contains("isturbance"))
 

birds <- s123 %>% 
  select(ï..ObjectID, GlobalID, contains(c("eron", "gret", "GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO")), -starts_with(c("You.have", "Please.fill"))) %>% 
  pivot_longer(contains(c("eron", "gret", "GREG", "GBHE", "SNEG", "BCNH", "CAEG", "DCCO"))) %>% 
  mutate(name = gsub("Great.Blue.Heron", "GBHE", name),
         name = gsub("Great.Egret", "GREG", name),
         name = gsub("Snowy.Egret", "SNEG", name),
         name = gsub("Black.Crowned.Night.Heron", "BCNH", name),
         name = gsub("Cattle.Egret", "CAEG", name),
         name = gsub("Double.Crested.Cormorant", "DCCO", name))
 
          