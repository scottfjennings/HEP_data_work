

## generate summaries/visualizations of HEP data for annual reporting

library(tidyverse)


hep.start <- read.csv("HEP_data/newer_hep.csv")
names(hep.start) <- tolower(names(hep.start))
report.year = 2014

hep <- hep.start %>% 
  filter(peakactvnsts > -1) %>% 
  mutate(code = as.numeric(as.character(code))) 



hep.sites.start <- read.csv("HEP_data/tbl_HEPSites.csv")
names(hep.sites.start) <- tolower(names(hep.sites.start))
hep.sites <- hep.sites.start %>% 
  select(code, site_name, county, subregion, utmnorth, utmeast) %>% 
  filter(subregion != is.na(subregion) | subregion == "")

#--------------------------------------------
colony_summarizer <- function(){
first_last_years <- hep %>% 
  group_by(code, species) %>% 
  filter(peakactvnsts > 0) %>% 
  summarise(first.year = min(year),
            last.year = max(year)) %>% 
  full_join(hep)

size_last_year <- first_last_years %>% 
  group_by(code, species) %>% 
  filter(year == last.year) %>% 
  select(code, species, size.last.year = peakactvnsts)

size_first_year <- first_last_years %>% 
  group_by(code, species) %>% 
  filter(year == first.year) %>% 
  select(code, species, size.first.year = peakactvnsts)

col.summary <- hep %>% 
  group_by(code, species) %>% 
  filter(peakactvnsts > 0) %>% 
  summarise(first.year = min(year),
            last.year = max(year),
            peak.size = max(peakactvnsts),
            mean.size = round(mean(peakactvnsts), 0),
            num.years = n()) %>% 
  mutate(span.years = 1 + (last.year - first.year),
         actv.evry.year = ifelse(num.years == span.years, "Yes", "No")) %>% 
  full_join(., size_first_year) %>% 
  full_join(., size_last_year)



return(col.summary)
}
col.summary = colony_summarizer()



  col.lm <- lm(peakactvnsts ~ year + as.factor(code) + species, data = filter(hep, peakactvnsts > -1))


#---------------------------------------------
colony_trend_coefer <- function(spp, zcode){
  col.foo = filter(hep, code == as.numeric(zcode), species == spp)
  
  col.lm <- lm(peakactvnsts ~ year, data = col.foo)
  col.lm.year.coef <- coef(col.lm)[2]
  col.lm.year.lci <- confint(col.lm)[2,1]
  col.lm.year.uci <- confint(col.lm)[2,2]
  col.trend <- cbind(spp, zcode, col.lm.year.coef, col.lm.year.lci, col.lm.year.uci)
}

codes <- hep %>% 
  distinct(code) %>% 
  select(zcode = code)

spp <- hep %>%
  distinct(species)

col.trends <- map2_df(spp, codes, colony_trend_coefer())


foo <- colony_trend_coefer("GBHE", "1.000")
#---------------------------------------------
subreg_summarizer <- function(){
  
  hep.w.sites <- hep %>% 
    select(code, year, species, peakactvnsts) %>% 
    full_join(hep.sites)
  
  subreg_yr.spp.peak <- hep.w.sites %>% 
  filter(peakactvnsts > 0) %>% 
    group_by(subregion, year, species) %>% 
    summarise(tot.nests = sum(peakactvnsts))
  
subreg_first_last_years <- subreg_yr.spp.peak %>% 
  group_by(subregion, species) %>% 
  summarise(first.year = min(year),
            last.year = max(year)) %>% 
  full_join(subreg_yr.spp.peak)

size_last_year <- subreg_first_last_years %>% 
  group_by(subregion, species) %>% 
  filter(year == last.year) %>% 
  select(subregion, species, size.last.year = tot.nests)

size_first_year <- subreg_first_last_years %>% 
  group_by(subregion, species) %>% 
  filter(year == first.year) %>% 
  select(subregion, species, size.first.year = tot.nests)

subreg.summary <- subreg_first_last_years %>% 
  group_by(subregion, species) %>% 
  summarise(first.year = min(year),
            last.year = max(year),
            peak.size = max(tot.nests),
            mean.size = round(mean(tot.nests), 0),
            num.years = n()) %>% 
  mutate(span.years = 1 + (last.year - first.year),
         actv.evry.year = ifelse(num.years == span.years, "Yes", "No")) %>% 
  full_join(., size_first_year) %>% 
  full_join(., size_last_year)

return(subreg.summary)
}
subreg.summary = subreg_summarizer()



num.colonies <- hep %>% 
  filter(year == report.year, numbervisits > 0) %>% 
  distinct(code) %>% 
  summarise(num.cols.surveyed = n())

num.colonies.occ <- hep %>% 
  filter(year == report.year, peakactvnsts > 0) %>% 
  distinct(code) %>% 
  summarise(num.cols.occupied = n())


num.colonies.spp <- hep %>% 
  filter(year == report.year, peakactvnsts > 0) %>% 
  group_by(species) %>% 
  distinct(code) %>% 
  summarise(num.cols.per.sp = n())

      

colony.peak.changer <- function() {
  foo <- hep %>% 
    select(year, code, species, peakactvnsts)
  
  foo.report.yr <- foo %>% 
    filter(year == report.year) %>% 
    rename(report.year.peak = peakactvnsts) %>% 
    select(-year)
           
  foo.prev.yr <- foo %>% 
    filter(year == report.year - 1) %>% 
    rename(prev.year.peak = peakactvnsts) %>% 
    select(-year)
  
  foo.all.prev.yr <- foo %>% 
    filter(year < report.year) %>% 
    group_by(code, species) %>% 
    summarise(mean.peak = mean(peakactvnsts)) %>% 
    mutate(mean.peak = round(mean.peak, 0))
  
  
  
  foo2 <- full_join(foo.report.yr, foo.prev.yr, by = c("code", "species")) %>% 
    full_join(., foo.all.prev.yr, by = c("code", "species")) %>% 
    mutate(cut = ifelse((prev.year.peak == 0 & report.year.peak == 0 & mean.peak == 0), 1, 0)) %>%
    filter(cut == 0) %>% 
    mutate(per.of.1yr = (report.year.peak/prev.year.peak)*100,
           per.of.1yr = round(per.of.1yr, 0),
           per.change.1yr = per.of.1yr - 100,
           per.of.mean = (report.year.peak/mean.peak)*100,
           per.of.mean = round(per.of.mean, 0),
           per.change.mean = per.of.mean - 100,
           per.of.1yr = ifelse(is.nan(per.of.1yr), 0, per.of.1yr),
           per.change.1yr = ifelse(is.nan(per.change.1yr), 0, per.change.1yr)) %>% 
    select(-cut)
  
return(foo2)
}
colony.changes <- colony.peak.changer()





subreg.peak.changer <- function() {
  foo <- hep %>% 
    select(year, code, species, peakactvnsts) %>% 
    full_join(., hep.sites) %>% 
    group_by(subregion, year, species) %>%
    filter(peakactvnsts > 0) %>% 
    summarize(tot.nests = sum(peakactvnsts))
  
  foo.report.yr <- foo %>% 
    ungroup() %>% 
    filter(year == report.year) %>% 
    rename(report.year.peak = tot.nests) %>% 
    select(-year)
           
  foo.prev.yr <- foo %>% 
    ungroup() %>% 
    filter(year == report.year - 1) %>% 
    rename(prev.year.peak = tot.nests) %>% 
    select(-year)
  
  foo.all.prev.yr <- foo %>% 
    ungroup() %>% 
    filter(year < report.year) %>% 
    group_by(subregion, species) %>% 
    summarise(mean.peak = mean(tot.nests)) %>% 
    mutate(mean.peak = round(mean.peak, 0))

  foo2 <- full_join(foo.report.yr, foo.prev.yr, by = c("subregion", "species")) %>% 
    full_join(., foo.all.prev.yr, by = c("subregion", "species")) %>% 
    mutate(cut = ifelse((prev.year.peak == 0 & report.year.peak == 0 & mean.peak == 0), 1, 0)) %>%
    filter(cut == 0) %>% 
    mutate(per.of.1yr = (report.year.peak/prev.year.peak)*100,
           per.of.1yr = round(per.of.1yr, 0),
           per.change.1yr = per.of.1yr - 100,
           per.of.mean = (report.year.peak/mean.peak)*100,
           per.of.mean = round(per.of.mean, 0),
           per.change.mean = per.of.mean - 100,
           per.of.1yr = ifelse(is.nan(per.of.1yr), 0, per.of.1yr),
           per.change.1yr = ifelse(is.nan(per.change.1yr), 0, per.change.1yr)) %>% 
    select(-cut) %>% 
    mutate(site_name = "Entire subregion",
           code = "") %>% 
    
  
return(foo2)
}

subreg.changes <- subreg.peak.changer()


subreg.col.changes <- colony.changes %>% 
  data.frame() %>% 
  left_join(., select(hep.sites, code, subregion, site_name)) %>% 
  mutate(site_name = as.character(site_name)) %>% 
  rbind(., subreg.changes) %>% 
  mutate() %>% 
  mutate_if(is.numeric, list(~na_if(., Inf)))


spp.subreg.forester <- function(spp, subreg){

  spp.names <- data.frame(species = c("GBHE", "GREG", "SNEG", "BCNH", "DCCO", "CAEG"),
                        sp.name = c("Great Blue Heron", "Great Egret", "Snowy Egret", "Black-crowned Night-heron", "Double-crested Cormorant", "Cattle Egret"))
  
  sub.regions <- data.frame(subregion = c("CSF", "IEB", "NNC", "OUC", "PNM", "RUR", "SUS"),
                            subreg.names = c("Central SF Bay", "Interior and East Bay", "Napa County", "Outer Coast", "Petaluma and Napa Marshes", "Russion River Watershed", "Suisun Bay, Marsh"))

sp.name <- filter(spp.names, species == spp)
subreg.name <- filter(sub.regions, subregion == subreg)
ztitle <- paste("1-year change in", sp.name$sp.name, "colony size, ", subreg.name$subreg.names)

  
  spp.changes <- subreg.col.changes %>% 
  filter(species == spp, subregion == subreg, report.year.peak > 0) %>% 
  arrange(per.of.1yr) %>% 
  mutate(change = "none",
         change = ifelse(per.change.1yr > 0, "increase", change),
         change = ifelse(per.change.1yr < 0, "decrease", change))


ggplot(data = spp.changes) +
  geom_bar(aes(x = reorder(site_name, per.change.1yr), y = per.change.1yr, color = change, fill = change), stat = "identity") +
  coord_flip() + 
  #scale_fill_manual(values = c("increase" = "green", "decrease" = "red", "none" = "blue")) +
  #ylim(-100, NA) +
  ylab("% change from previous year") +
  xlab("") +
  ggtitle(ztitle)

}


spp.subreg.forester("GREG", "SUS")


ggplot(data = gbhe.changes) +
  geom_point(aes(x = reorder(site_name, per.change.1yr), y = report.year.peak), color = "green") +
  geom_point(aes(x = reorder(site_name, per.change.1yr), y = prev.year.peak), color = "blue") +
  coord_flip() +
  geom_abline()
+
  facet_wrap(~subregion, ncol = 3)
  geom_point(aes(x = site_name, y = per.of.mean), color = "green")+
  geom_hline(yintercept = 100) 




