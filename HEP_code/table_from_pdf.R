
# gather and visualze birds data and covariates

# 1 packages ----
library(tidyverse)
library(lubridate)
library(tabulizer)
library(fuzzyjoin)
source("C:/Users/scott.jennings/Documents/Projects/birds/bird_taxa_filter.R")
options(scipen = 999)


mi_tabs <- extract_tables("C:/Users/scott.jennings/Documents/Reports_shorts_etc/Reports/MarinIs/kelly_fischer_2018_wmi2018.pdf")


mi_nest_nums <- mi_tabs[2][[1]][3:41, 2:6]%>% 
   data.frame() %>% 
  rename(year = 1, GREG = 2, SNEG = 3, BCNH = 4, GBHE = 5)


write.csv(mi_nest_nums, "C:/Users/scott.jennings/Documents/Reports_shorts_etc/Reports/MarinIs/MarinIs_nest_nums.csv")
