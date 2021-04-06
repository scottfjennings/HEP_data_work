


## generate summaries/visualizations of HEP data for annual reporting

# 1 packages ----
library(tidyverse)
library(lubridate)
library(RODBC)
library(devtools)
library(plotly)

devtools::install_github("troyhill/VulnToolkit")

library(VulnToolkit)
options(scipen = 999)
source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")


# generate a list of relevant stations
d <- noaa.stations() 

# download data using noaa.stations() result
highlow <- noaa(station = 9415402, begindate = 20130101)  




hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_utility_functions.R
hep_start <- hep_from_access(hepdata_location)


bohannon <- filter(hep_start, CODE == 21) %>% 
  select(YEAR, contains("STGEDATE")) %>% 
  arrange(YEAR) %>% 
  distinct() %>% 
  pivot_longer(cols = contains("STGEDATE")) %>% 
  drop_na() %>% 
  mutate(date = as.character(value),
         date = gsub("-", "", date),
         station = 9415402)



single_day_tide_getter <- function(date, station) {
zzz <- paste("https://tidesandcurrents.noaa.gov/api/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=", date, "&end_date=", date, "&datum=MLLW&station=", station, "&time_zone=lst_ldt&units=english&interval=hilo&format=csv", sep = "")

inc <- try(read.csv(zzz, as.is=TRUE))

inc <- inc %>% 
  mutate(station = station)
}

suisun_survey_tides <- map2_df(bohannon$date, bohannon$station, single_day_tide_getter)

suisun_survey_tides2 <- suisun_survey_tides %>% 
  separate(Date.Time, into = c("date", "time"), sep = " ", remove = FALSE) %>% 
  group_by(date) %>% 
  mutate(tide.num = row_number(),
         tide.num = paste("tide", tide.num, sep = "."),
         tide.num = as.character(tide.num)) %>% 
  ungroup() %>% 
  mutate(hour = hour(Date.Time),
         year = year(Date.Time))


suisun_survey_tides2 %>% 
  #filter(year > 2010) %>% 
  filter(month(Date.Time) == 5) %>% 
ggplot() +
  geom_smooth(aes(x = hour, y = Prediction, color = as.character(year), group = as.character(year))) +
  scale_x_continuous(breaks = c(seq(1, 24)), labels = c(seq(1, 24))) +
  geom_vline(xintercept = 8) +
  geom_vline(xintercept = 14) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ggtitle("Tide on Suisun survey day") +
  ylab("tide height (ft)")

ggsave("figures_output/suisun_tides.png", width = 10)


### -------

# get potential survey days for current year


current_year_suisun_tide_getter <- function(day1 = 15, day2 = 31, station = 9415402) {
  date1 = paste(year(Sys.Date()), "05", day1, sep = "")
  date2 = paste(year(Sys.Date()), "05", day2, sep = "")
  
  
  zzz <- paste("https://tidesandcurrents.noaa.gov/api/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=", date1, "&end_date=", date2, "&datum=MLLW&station=", station, "&time_zone=lst_ldt&units=english&interval=hilo&format=csv", sep = "")

inc <- try(read.csv(zzz, as.is=TRUE))

inc <- inc %>% 
  mutate(station = station)
}


suisun_tides <- current_year_suisun_tide_getter(day1 = "05") %>% 
  separate(Date.Time, into = c("date", "time"), sep = " ", remove = FALSE) %>% 
  group_by(date) %>% 
  mutate(tide.num = row_number(),
         tide.num = paste("tide", tide.num, sep = "."),
         tide.num = as.character(tide.num)) %>% 
  ungroup() %>% 
  mutate(hour = hour(Date.Time),
         hour = as.numeric(hour))

survey_days <- suisun_tides%>%
  mutate(survey.day = ifelse(Type == "H" & hour >= 8 & hour < 12 & Prediction > 3, TRUE, FALSE)) %>% 
  distinct(date, survey.day) %>% 
  filter(survey.day == TRUE)

suisun_tides_plot <- suisun_tides %>% 
  full_join(., survey_days) %>% 
  filter(!wday(Date.Time, label = TRUE) %in% c("Sat", "Sun")) %>% 
ggplot() +
  geom_smooth(aes(x = hour, y = Prediction, color = survey.day, group = as.character(day(Date.Time)),
              text = paste(wday(Date.Time, label = TRUE), ", ", month(Date.Time, label = TRUE), day(Date.Time), sep = ""))) +
  scale_x_continuous(breaks = c(seq(1, 24)), labels = c(seq(1, 24))) +
  geom_vline(xintercept = 9) +
  geom_vline(xintercept = 12) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  ggtitle("Tide on Suisun survey day") +
  ylab("tide height (ft)")

ggplotly(suisun_tides_plot, tooltip = "text")


