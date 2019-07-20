

#takes imported and filtered HEP dataframe from HEP_code


library(readxl)
library(plyr)
library(tidyverse)
library(lubridate)
setwd("C:/Users/scott.jennings/Documents/") 





hep=read_csv("Projects/HEP/newer_hep.csv")

#hep$MARSTGEDATE <- as.Date(hep$MARSTGEDATE, format = "%d-%h-%y")



# and any data that aren't yet entered; this from ...ACR\HEP_screening\HEPscreen to HEP.r
hep2015=read_csv("Projects/HEP/hep_generated_2015.csv")
hep2016=read_csv("Projects/HEP/hep_generated_2016.csv")
hep2017=read_csv("Projects/HEP/hep_generated_2017_blanks.csv")



# then combine
new.hep=data.frame(bind_rows(hep, hep2017))
new.hep$MARSTGEDATE <- as.Date(as.character(new.hep$MARSTGEDATE), format = "%m/%d/%Y")
new.hep$APRSTGEDATE <- as.Date(as.character(new.hep$APRSTGEDATE), format = "%m/%d/%Y")
new.hep$MAYSTGEDATE <- as.Date(as.character(new.hep$MAYSTGEDATE), format = "%m/%d/%Y")
new.hep$JUNSTGEDATE <- as.Date(as.character(new.hep$JUNSTGEDATE), format = "%m/%d/%Y")
new.hep$LTJUNSTGDATE <- as.Date(as.character(new.hep$LTJUNSTGDATE), format = "%m/%d/%Y")






#new.hep$CODE <- as.factor(new.hep$CODE)

newer.hep <- data.frame(bind_rows(hep, new.hep))
#newer.hep$CODE <- as.factor(newer.hep$CODE)
newer.hep$SPECIES <- as.factor(newer.hep$SPECIES)
newer.hep[c("BRD1", "BRD2", "BRD3", "BRD4", "BRD5", "BRD6",
            "MARSTAGE1", "MARSTAGE2", "MARSTAGE3", "MARSTAGE4", "MARSTAGE5",
            "APRSTAGE1", "APRSTAGE2", "APRSTAGE3", "APRSTAGE4", "APRSTAGE5",
            "MAYSTAGE1", "MAYSTAGE2", "MAYSTAGE3", "MAYSTAGE4", "MAYSTAGE5",
            "JUNSTAGE1", "JUNSTAGE2", "JUNSTAGE3", "JUNSTAGE4", "JUNSTAGE5",
            "LTJUNSTAGE1", "LTJUNSTAGE2", "LTJUNSTAGE3", "LTJUNSTAGE4", "LTJUNSTAGE5",
            "JULSTAGE1", "JULSTAGE2", "JULSTAGE3", "JULSTAGE4", "JULSTAGE5"
            )][is.na(newer.hep[c("BRD1", "BRD2", "BRD3", "BRD4", "BRD5", "BRD6",
                                 "MARSTAGE1", "MARSTAGE2", "MARSTAGE3", "MARSTAGE4", "MARSTAGE5",
                                 "APRSTAGE1", "APRSTAGE2", "APRSTAGE3", "APRSTAGE4", "APRSTAGE5",
                                 "MAYSTAGE1", "MAYSTAGE2", "MAYSTAGE3", "MAYSTAGE4", "MAYSTAGE5",
                                 "JUNSTAGE1", "JUNSTAGE2", "JUNSTAGE3", "JUNSTAGE4", "JUNSTAGE5",
                                 "LTJUNSTAGE1", "LTJUNSTAGE2", "LTJUNSTAGE3", "LTJUNSTAGE4", "LTJUNSTAGE5",
                                 "JULSTAGE1", "JULSTAGE2", "JULSTAGE3", "JULSTAGE4", "JULSTAGE5")])] <- 0


## 

write_csv(new.hep, "projects/HEP/newer_hep.csv")



