library(plyr)
library(tidyverse)
library(lubridate)
library(xlsx)
library(stringi)

#
setwd("C:/Users/scott.jennings/Documents") 

#===========================================================

##importing from hep_raw access database
## run HEP_individual nests Query, export to xls

HEP_indiv_nests <- read_csv("HEP_screening/queried_from_access/HEP_individual nests Query.csv")
# capitalize first letter of column names 
names(HEP_indiv_nests)[] <- stri_trans_totitle(names(HEP_indiv_nests)[])


# some data management
HEP_indiv_nests <- HEP_indiv_nests %>% 
  mutate(Nest = as.character(Nest),
         # make date the correct format then add field for julian date and the day and month.
         Date = mdy(Date),
         # fill in missing data
         Stage = ifelse(Stage == "NA", 0, Stage)) %>% 
  select(Species = Speciescode, everything())
  
 

## make season summary file
make.seas.summ <- function(){
  first.last.survey <- HEP_indiv_nests %>% 
    group_by(Code, year(Date), Species) %>%
    summarize(first.survey = min(Date),
              last.survey = max(Date)) %>% 
    data.frame()
  
  first.last.act.nest <- HEP_indiv_nests %>% 
    filter(Status=="A") %>%
    group_by(Code, year(Date), Species) %>%
    summarize(first.act.nest = min(Date),
              last.act.nest = max(Date)) %>% 
    data.frame()
  
  num.survey.days <- HEP_indiv_nests %>% 
    group_by(Code, year(Date), Species) %>%
    distinct(Date) %>% 
    summarize(num.survey.days = n()) %>% 
    data.frame()
  
  seas.summ=join_all(list(first.last.survey, first.last.act.nest, num.survey.days), type = 'full') %>% 
    select(Code, Species, first.survey, first.act.nest, last.act.nest, last.survey) %>% 
    mutate(year = year(first.survey))
  
  return(seas.summ)
}
seas.summ=make.seas.summ()


write_csv(seas.summ, "C:/Users/scott.jennings/Documents/Projects/HEP/seas_summary.csv")


