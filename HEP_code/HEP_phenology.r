

# calculate average phenology for ardeid nesting in north SF bay area (ACR HEP study area)

# for each species, make a model to predict what proportion of nests is expected to be in each stage on a particular date

# packages, source

library(tidyverse)

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
df_long <- df %>% 
  dplyr::select(code, year, species, contains("stge"), contains("stage")) %>% 
  pivot_longer(cols = contains("stage"), names_to = ("stage"), values_to = ("brood.size"))
  
mar <- select(df, code, year, species, marstgedate, marstage1, marstage2, marstage3, marstage4, marstage5) 
mar.long = gather(mar, key = stage, value = mar.num.nests, -code, -year, -species, -marstgedate)
mar.long$stage <- as.numeric(substr(mar.long$stage, 9, 9))
mar.long=filter(mar.long, mar.num.nests>0) 

apr <- select(df, CODE, YEAR, SPECIES, APRSTGEDATE, APRSTAGE1, APRSTAGE2, APRSTAGE3, APRSTAGE4, APRSTAGE5) 
apr.long = gather(apr, key = stage, value = apr.num.nests, -CODE, -YEAR, -SPECIES, -APRSTGEDATE)
apr.long$stage <- as.numeric(substr(apr.long$stage, 9, 9))
apr.long=filter(apr.long, apr.num.nests>0) 

may <- select(df, CODE, YEAR, SPECIES, MAYSTGEDATE, MAYSTAGE1, MAYSTAGE2, MAYSTAGE3, MAYSTAGE4, MAYSTAGE5) 
may.long = gather(may, key = stage, value = may.num.nests, -CODE, -YEAR, -SPECIES, -MAYSTGEDATE)
may.long$stage <- as.numeric(substr(may.long$stage, 9, 9))
may.long=filter(may.long, may.num.nests>0) 

jun <- select(df, CODE, YEAR, SPECIES, JUNSTGEDATE, JUNSTAGE1, JUNSTAGE2, JUNSTAGE3, JUNSTAGE4, JUNSTAGE5) 
jun.long = gather(jun, key = stage, value = jun.num.nests, -CODE, -YEAR, -SPECIES, -JUNSTGEDATE)
jun.long$stage <- as.numeric(substr(jun.long$stage, 9, 9))
jun.long=filter(jun.long, jun.num.nests>0) 

ltjun <- select(df, CODE, YEAR, SPECIES, LTJUNSTGDATE, LTJUNSTAGE1, LTJUNSTAGE2, LTJUNSTAGE3, LTJUNSTAGE4, LTJUNSTAGE5) 
ltjun.long = gather(ltjun, key = stage, value = ltjun.num.nests, -CODE, -YEAR, -SPECIES, -LTJUNSTGDATE)
ltjun.long$stage <- as.numeric(substr(ltjun.long$stage, 9, 9))
ltjun.long=filter(ltjun.long, ltjun.num.nests>0) 

jul <- select(df, CODE, YEAR, SPECIES, JULSTGEDATE, JULSTAGE1, JULSTAGE2, JULSTAGE3, JULSTAGE4, JULSTAGE5) 
jul.long = gather(jul, key = stage, value = jul.num.nests, -CODE, -YEAR, -SPECIES, -JULSTGEDATE)
jul.long$stage <- as.numeric(substr(jul.long$stage, 9, 9))
jul.long=filter(jul.long, jul.num.nests>0) 

stages <- join_all(list(mar.long, apr.long, may.long, jun.long, ltjun.long, jul.long), by=c("CODE", "YEAR", "SPECIES", "stage"), type='full')

stages <- select(stages, CODE, YEAR, SPECIES, stage, everything())

return(stages)
}

