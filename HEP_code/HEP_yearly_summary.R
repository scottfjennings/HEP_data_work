


## generate summaries/visualizations of HEP data for annual reporting

# 1 packages ----
library(tidyverse)
library(RColorBrewer)
library(RODBC)
library(devtools)
options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/R_general/utility_functions/bird_utility_functions.R")
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")
source_url("https://raw.githubusercontent.com/scottfjennings/HEP_data_work/master/HEP_code/HEP_utility_functions.R")

# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")

# 2 data ----
zsppz <- c("BCNH", "GBHE", "GREG", "SNEG")

hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# all these functions are in HEP_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location)

hep <- hep_start %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% # remove "no data" records
  left_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% # add human readable colony names
  trim_hep_columns() %>% # remove set blocks of columns
  cut_never_nested() %>% # remove all records for colony X species that were never really active _ cut artifact of "complete" HEPDATA
  mutate(site.name = as.character(site.name))
  

#--

### run to here for start


# data checking ----
foo <- data.frame(table(hep$site_name, hep$year, hep$species)) %>% 
  rename(site = 1, year = 2, species = 3) %>% 
  mutate(year = as.numeric(as.character(year)))
foo_wide <- foo %>% 
  spread(year, Freq) 

#write.csv(foo_wide, "colony_year_spp.csv", row.names = F)

# number of years each colony had >0 nests
years_active <- hep %>% 
  filter(peakactvnsts > 0) %>% 
  group_by(species, parent.site.name) %>% 
  summarise(num.yrs.active = n()) %>% 
  arrange(species, -num.yrs.active)
#
# number of colonies surveyed each year ----
make_num_colonies_per_year <- function() {
num_colonies_sp_yr <- hep %>% 
  filter(species != "DCCO", peakactvnsts > 0)  %>%  
  select(year, parent.code, species) %>%  
  distinct() %>% 
  arrange(year) %>% 
  group_by(year, species) %>% 
  summarise(num.colonies.spp = n()) %>% 
  arrange(year, species)

num_colonies_wide <- num_colonies_sp_yr %>% 
  ungroup() %>% 
  spread(species, num.colonies.spp) 

num_colonies_yr <- hep %>% 
  filter(species != "DCCO", peakactvnsts >= 0) %>%  
  select(year, parent.code) %>% 
  distinct() %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  summarise(total.colonies = n()) %>% 
  arrange(year)

active_colonies_yr <- hep %>% 
  filter(species != "DCCO", peakactvnsts > 0) %>%  
  select(year, parent.code) %>% 
  distinct() %>% 
  arrange(year) %>% 
  group_by(year) %>% 
  summarise(active.colonies.yr = n()) %>% 
  arrange(year)

num_colonies2 <- full_join(num_colonies_wide, num_colonies_yr) %>% 
  full_join(., active_colonies_yr)
}
num_colonies_sp_yr <- make_num_colonies_per_year()
# plot number_of_colonies_surveyed per year ----
ggplot(data = num_colonies_sp_yr) + 
  geom_point(aes(x = year, y = num.colonies.spp, color = species)) +
  geom_line(aes(x = year, y = num.colonies.spp, color = species)) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  ylab("Number of colonies surveyed") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("")
ggsave("figures_output/number_of_colonies_surveyed.jpg", width = 6, height = 4, units = "in")


# number of nests per species per year, all colonies combined -----
make_total_num_nests <- function(){
  num_nests_yr_spp <- hep %>% 
  filter(species != "DCCO") %>% 
  group_by(species, year) %>% 
  summarise(tot.peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup()

num_nests_yr_spp_wide <- num_nests_yr_spp %>% 
  ungroup() %>% 
  spread(species, tot.peakactvnsts) %>% 
  mutate(all.spp = rowSums(.[, 2:6], na.rm = T))
                             
num_nests_yr_spp %>% 
  filter(species %in% zsppz) %>% 
  ggplot() +
  geom_point(aes(x = year, y = tot.peakactvnsts)) +
  geom_smooth(aes(x = year, y = tot.peakactvnsts)) +
  facet_wrap(~species)
}
num_nests_yr_spp <- make_total_num_nests()

# number of nests, scaled by number of colonies surveyed (mean # nests per colony) ----

nests_colonies <- full_join(num_nests_yr_spp, num_colonies_sp_yr) %>% 
  mutate(scaled.num.nests = tot.peakactvnsts/num.colonies.spp)

nests_colonies %>% 
  filter(species %in% zsppz) %>% 
  ggplot() +
  geom_point(aes(x = year, y = scaled.num.nests)) +
  geom_smooth(aes(x = year, y = scaled.num.nests)) +
  facet_wrap(~species, scales = "free")

nests_colonies %>% 
  filter(species %in% c("GBHE", "GREG", "SNEG")) %>% 
  pivot_longer(cols = c(scaled.num.nests, tot.peakactvnsts, num.colonies.spp)) %>% 
  mutate(name = factor(name, levels = c('tot.peakactvnsts', 'num.colonies.spp', "scaled.num.nests"), labels = c("total nests", "total colonies", "scaled number of nests")),
         species = factor(species, levels = c("GBHE", "GREG", "SNEG"), labels = c("Great Blue Heron", "Great Egret", "Snowy Egret"))) %>% 
  ggplot() +
  geom_point(aes(x = year, y = value)) +
  geom_smooth(aes(x = year, y = value)) +
  facet_wrap(species ~ name, scales = "free", ncol = 3) +
  theme_bw() +
  ylab("") +
  xlab("Year") +
  ggtitle("ACR colony monitoring data") +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))

#ggsave("figures_output/GBHE_GREG_SNEG_trends.png", width = 10, height = 6)


# nest survival ----
hep  %>% 
  select(parent.site.name, species, code, site.name, parent.code, county, subregion, year, peakactvnsts, focalnests, focfailure, brd1, brd2, brd3, brd4, brd5, brd6) %>% 
  filter(species != "DCCO", species != "CAEG", focalnests > 0, !is.na(focfailure)) %>% 
  mutate(nest.surv = 1 - (focfailure/focalnests)) %>% 
  group_by(species, year) %>%
  mutate(mean.nest.surv = mean(nest.surv),
         sd.nest.surv = sd(nest.surv),
         nest.surv.resid = nest.surv-mean.nest.surv) %>% 
  ungroup() %>% 
  ggplot(aes(x = year, y = nest.surv.resid)) +
  geom_point() +
  facet_wrap(~species)

# 1-year change in number of nests per colony, 1-year percent change in number of nests per colony ----
# calculations

hep_changes <- hep %>% 
  hep_annual_changer() %>% 
  filter(!is.na(prev.yr.nsts), zero2zero == 0, zero2some == 0, year >= 1990) %>% 
  bird_taxa_filter(drop_cols = c("species.number", "order", "family", "subfamily", "genus", "species")) %>% 
  rename(species = alpha.code, spp.name = common.name)

hep_changes %>% 
  full_join(years_active) %>% 
  filter(species %in% zsppz, subregion == "OUC", num.yrs.active > 10) %>% 
ggplot() +
  geom_point(aes(x = year, y = per.change.1year, color = parent.site.name)) +
  geom_line(aes(x = year, y = per.change.1year, color = parent.site.name)) +
  stat_smooth(aes(x = year, y = per.change.1year)) +
  facet_wrap(~species, scales = "free")


change_resids <- hep_changes %>% 
  group_by(species, year) %>% 
  mutate(mean.per.change = mean(per.change.1year),
         change.resid = mean.per.change - per.change.1year,
         num.cases = n()) %>% 
  ungroup() %>% 
  arrange(subregion, year, species)


ggplot(data = filter(change_resids, species != "CAEG", species != "DCCO")) +
  geom_point(aes(x = year, y = mean.per.change)) +
  facet_wrap(~species, scales = "free")




mean_change_resids <- change_resids %>%
  filter(num.cases > 3) %>% 
  group_by(subregion, parent.site.name, species) %>% 
  summarise(num.years = n(),
            mean.change.resid = mean(change.resid),
            sd.change.resid = sd(change.resid),
            max.change.resid = max(change.resid),
            min.change.resid = min(change.resid),
            abs.mean.change.resid = abs(mean.change.resid)) %>% 
  ungroup() %>% 
  arrange(subregion, species, abs.mean.change.resid) 
  
# plot annual changes in number of nests and colonies
annual_colony_plotter <- function(zspecies) {
  zcolor = case_when(zspecies == "BCNH" ~ brewer.pal(8, "Dark2")[1],
                   zspecies == "CAEG" ~ brewer.pal(8, "Dark2")[2],
                   zspecies == "GBHE" ~ brewer.pal(8, "Dark2")[3],
                   zspecies == "GREG" ~ brewer.pal(8, "Dark2")[4],
                   zspecies == "SNEG" ~ brewer.pal(8, "Dark2")[5],
                   zspecies == "All" ~ brewer.pal(8, "Dark2")[6])
  

ann_nests_colonies <- annual_nests_colonies  %>% 
  filter(species == zspecies)
  
ztitle <- paste("Number of", distinct(ann_nests_colonies, spp.name)$spp.name, "nests and colonies.")

  
ann_colony_plot <- ann_nests_colonies %>% 
  mutate(data.type = ifelse(grepl("colonies", name), "Total colonies", "Total nests")) %>% 
ggplot() +
  geom_point(aes(year, value,
                 text = paste(year, "\n",
                              data.type, ": ", value, sep = ""), color = zcolor)) +
  geom_line(aes(year, value), color = zcolor) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("") +
  ggtitle(ztitle) +
  theme(plot.margin = margin(t = 15)) + 
  facet_wrap(~data.type, scales = "free")

ggplotly(ann_colony_plot, tooltip = "text")
}

annual_colony_plotter("GREG")  
ggsave(here("figures_output/greg_nests_colonies.png"))
# plotting percent_change_colony_size -----
per_change_plotter <- function(zspp, zgroup, save.plot = TRUE, spp.color = T) {
 # zspp = c("All", "BCNH", "CAEG", "GBHE", "GREG", "SNEG")
 # zgroup = c("year", "subreg.name")
  if(zspp == "All") {
  per_change1 <- hep_changes %>% 
    filter(zero2some == 0)
} else {
  per_change1 <- hep_changes %>% 
  filter(species == zspp, zero2some == 0)
  }
#--   
per_change <- per_change1 %>% 
  group_by(.dots = zgroup[1:length(zgroup)]) %>% 
  summarise(mean.per.change = mean(per.change.1year, na.rm = T),
            mean.per.change = round(mean.per.change, 1),
            sd.per.change = sd(per.change.1year, na.rm = T),
            num.colonies.spp = n(),
            se.per.change = sd.per.change/sqrt(num.colonies.spp),
            sd.per.change = round(sd.per.change, 1),
            se.per.change = round(se.per.change, 1)) %>% 
  ungroup() 
#-- 
zcolor = ifelse(spp.color == F, "black",
                ifelse(zspp == "BCNH", brewer.pal(8, "Dark2")[1],
                ifelse(zspp == "CAEG", brewer.pal(8, "Dark2")[2],
                       ifelse(zspp == "GBHE", brewer.pal(8, "Dark2")[3],
                              ifelse(zspp == "GREG", brewer.pal(8, "Dark2")[4],
                                     ifelse(zspp == "SNEG", brewer.pal(8, "Dark2")[5],
                                            ifelse(zspp == "All", brewer.pal(8, "Dark2")[6], NA)))))))




#--                                     
zspec.name = ifelse(zspp == "All", "All species combined", distinct(per_change1, spp.name))


panel_ylim <- per_change %>% 
  mutate(bottomz = ifelse(is.na(se.per.change), mean.per.change, mean.per.change - se.per.change)) %>% 
  filter(!is.na(bottomz))

nloc = min(panel_ylim$bottomz) * 2
#-- 
zplot = ggplot(data = per_change, aes(x = year, y = mean.per.change))  +
  geom_point(aes(x = year, y = mean.per.change), color = zcolor) +
  geom_line(aes(x = year, y = mean.per.change), color = zcolor) +
  geom_hline(yintercept = 0) +
  stat_smooth(aes(x = year, y = mean.per.change), color = zcolor, span = .75, se = F) +
  geom_errorbar(aes(ymin = mean.per.change - se.per.change, ymax = mean.per.change + se.per.change), width=.2, position=position_dodge(.9), color = zcolor)  +
  #theme(legend.title=element_blank(),
        #panel.background = element_blank(),
        #plot.title = element_text(colour = zcolor),
        #panel.border = element_rect(colour = "black", fill = "transparent", size=0.5),
        #panel.grid.major = element_blank(), 
        #panel.grid.minor = element_blank()) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) +
  ylab("Mean 1-year % change in number of nests (± SE)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("")
  #ggtitle(zspec.name) +
  #geom_text(aes(x = year, y = nloc, label = num.colonies.spp), size = 2)
#-- 
if("subreg.name" %in% zgroup == TRUE){
  zplot = zplot +
  facet_wrap(~subreg.name) 
} else {
  zplot = zplot
}
#-- 
if("subreg.name" %in% zgroup == TRUE){
zreg = "subreg" 
} else {
zreg = "NorthBay"
}
#-- 
 if(save.plot == TRUE) {
ggsave(paste("figures_output/colony_size/percent_change_colony_size/HEP_1yearPerChangeColonySize_", zreg, "_", zspp, "_transp.png", sep = ""), zplot, width = 6, height = 4, units = "in", bg = "transparent")
} else {
  return(zplot)
} 

}


zsppz = c("All", "BCNH", "CAEG", "GBHE", "GREG", "SNEG")
zgroupz <- c(list(c("year")), list(c("year", "subreg.name")))
zspp_zgroup <- expand.grid(zspp = zsppz, zgroup = zgroupz)


zplot = per_change_plotter(zspp = "GREG", zgroup = c("year"), save.plot = F, spp.color = T)

zplot = zplot +
  ylab("") +
  ggtitle("Average % change\n in colony size")+
  theme(text = element_text(size=20, face= "bold", colour= "black"))

ggsave("figures_output/colony_size/percent_change_colony_size/HEP_1yearPerChangeColonySize_NorthBay_GREG_transp_title_black.png", zplot, width = 6, height = 4, units = "in", bg = "transparent")

map2(zspp_zgroup$zspp, zspp_zgroup$zgroup, per_change_plotter)

# plot absolute colony change ----
abs_change_plotter <- function(zspp, zgroup, save.plot = TRUE) {
 # zspp = c("All", "BCNH", "CAEG", "GBHE", "GREG", "SNEG")
 # zgroup = c("year", "subreg.name")
  if(zspp == "All") {
  abs_change1 <- hep_changes 
} else {
  abs_change1 <- hep_changes %>% 
  filter(species == zspp)
  }
#--   
abs_change <- abs_change1 %>% 
  group_by(.dots = zgroup[1:length(zgroup)]) %>% 
  summarise(mean.abs.change = mean(abs.change.1year, na.rm = T),
            mean.abs.change = round(mean.abs.change, 1),
            sd.abs.change = sd(abs.change.1year, na.rm = T),
            num.colonies.spp = n(),
            se.abs.change = sd.abs.change/sqrt(num.colonies.spp),
            sd.abs.change = round(sd.abs.change, 1),
            se.abs.change = round(se.abs.change, 1)) %>% 
  ungroup() 
#-- 
zcolor = ifelse(zspp == "BCNH", brewer.pal(8, "Dark2")[1],
                ifelse(zspp == "CAEG", brewer.pal(8, "Dark2")[2],
                       ifelse(zspp == "GBHE", brewer.pal(8, "Dark2")[3],
                              ifelse(zspp == "GREG", brewer.pal(8, "Dark2")[4],
                                     ifelse(zspp == "SNEG", brewer.pal(8, "Dark2")[5],
                                            ifelse(zspp == "All", brewer.pal(8, "Dark2")[6], NA))))))
#--                                     
zspec.name = ifelse(zspp == "All", "All species combined", distinct(abs_change1, spp.name))


panel_ylim <- abs_change %>% 
  mutate(bottomz = ifelse(is.na(se.abs.change), mean.abs.change, mean.abs.change - se.abs.change)) %>% 
  filter(!is.na(bottomz))

nloc = min(panel_ylim$bottomz) * 2
#-- 
zplot = ggplot(data = abs_change, aes(x = year, y = mean.abs.change))  +
  geom_point(aes(x = year, y = mean.abs.change), color = zcolor) +
  geom_line(aes(x = year, y = mean.abs.change), color = zcolor) +
  geom_hline(yintercept = 0) +
  stat_smooth(aes(x = year, y = mean.abs.change), color = zcolor, span = .75, se = F) +
  geom_errorbar(aes(ymin = mean.abs.change - se.abs.change, ymax = mean.abs.change + se.abs.change), width=.2, position=position_dodge(.9), color = zcolor)  +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(colour = zcolor)) +
  ylab("Mean 1-year change in number of nests (± SE)") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("")+
  ggtitle(zspec.name) +
  geom_text(aes(x = year, y = nloc, label = num.colonies.spp), size = 2)
#-- 
if("subreg.name" %in% zgroup == TRUE){
  zplot = zplot +
  facet_wrap(~subreg.name) 
} else {
  zplot = zplot
}
#-- 
if("subreg.name" %in% zgroup == TRUE){
zreg = "subreg" 
} else {
zreg = "NorthBay"
}
#-- 
 if(save.plot == TRUE) {
ggsave(paste("figures_output/colony_size/absolute_change_colony_size/HEP_1yearAbsChangeColonySize_", zreg, "_", zspp, ".jpg", sep = ""), width = 10, height = 4, units = "in")
} else {
  return(zplot)
  } 
}


zsppz = c("All", "BCNH", "CAEG", "GBHE", "GREG", "SNEG")
zgroupz <- c(list(c("year")), list(c("year", "subreg.name")))
zspp_zgroup <- expand.grid(zspp = zsppz, zgroup = zgroupz)


abs_change_plotter(zspp = "GBHE", zgroup = c("year"), save.plot = FALSE)

map2(zspp_zgroup$zspp, zspp_zgroup$zgroup, abs_change_plotter)


# mean colony size by year ----
# 
mean_col_size_yr_plotter <- function(save.plot = TRUE) {
mean_col_size_yr <- hep %>% 
  filter(species != "DCCO") %>% 
  group_by(year) %>% 
  summarise(mean.peakactvnsts = mean(peakactvnsts),
            mean.peakactvnsts = round(mean.peakactvnsts, 1),
            sd.peakactvnsts = sd(peakactvnsts),
            num.colonies.spp = n(),
            se.peakactvnsts = sd.peakactvnsts/sqrt(num.colonies.spp),
            sd.peakactvnsts = round(sd.peakactvnsts, 1),
            se.peakactvnsts = round(se.peakactvnsts, 1)) %>% 
  ungroup() %>% 
  full_join(., num_colonies)

# lump all species, plot entire region

panel.ylim <- max(mean_col_size_yr$mean.peakactvnsts) / -10

zplot <- ggplot(data = mean_col_size_yr, aes(x = year, y = mean.peakactvnsts))  +
  geom_point(aes(x = year, y = mean.peakactvnsts), color = "#E6AB02") +
  stat_smooth(aes(x = year, y = mean.peakactvnsts), color = "#E6AB02", span = .75, se = FALSE) +
  geom_errorbar(aes(ymin = mean.peakactvnsts - se.peakactvnsts, ymax = mean.peakactvnsts + se.peakactvnsts), width=.2, position=position_dodge(.9), color = "#E6AB02") +
  ylab("Mean number of nests per colony (± SE)") +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(colour = "#E6AB02"))+
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("")  +
  geom_text((aes(x = year, y = panel.ylim, label = num.colonies.spp)), size = 2) +
  ggtitle("All species combined")

if(save.plot == TRUE) {
ggsave("figures_output/colony_size/HEP_colonySize_NorthBay_allSppCombined.jpg", width = 6, height = 4, units = "in")
} else {
  return(zplot)
}
}
mean_col_size_yr_plotter(save.plot = FALSE)
mean_col_size_yr_plotter()
# mean colony size by species, year, subregion -----
# 

mean_col_size_yr_spp_plotter <- function(zspp, save.plot = TRUE) {

  mean_col_size_yr_sp <- hep %>% 
  filter(species == zspp) %>% 
  group_by(year, species) %>% 
  summarise(mean.peakactvnsts = mean(peakactvnsts),
            mean.peakactvnsts = round(mean.peakactvnsts, 1),
            sd.peakactvnsts = sd(peakactvnsts),
            num.colonies.spp = n(),
            se.peakactvnsts = sd.peakactvnsts/sqrt(num.colonies.spp),
            sd.peakactvnsts = round(sd.peakactvnsts, 1),
            se.peakactvnsts = round(se.peakactvnsts, 1)) %>% 
  ungroup() %>% 
  left_join(., num_colonies) %>% 
  left_join(., spp.name) 
  
zcolor = ifelse(zspp == "BCNH", brewer.pal(8, "Dark2")[1],
                ifelse(zspp == "CAEG", brewer.pal(8, "Dark2")[2],
                       ifelse(zspp == "GBHE", brewer.pal(8, "Dark2")[3],
                              ifelse(zspp == "GREG", brewer.pal(8, "Dark2")[4],
                                     brewer.pal(8, "Dark2")[5]))))


zspec.name = distinct(mean_col_size_yr_sp, spp.name)
panel.ylim <- max(mean_col_size_yr_sp$mean.peakactvnsts) / -10

zplot = ggplot(data = mean_col_size_yr_sp, aes(x = year, y = mean.peakactvnsts))  +
  geom_point(aes(x = year, y = mean.peakactvnsts), color = zcolor) +
  stat_smooth(aes(x = year, y = mean.peakactvnsts), color = zcolor, span = .75, se = F) +
  geom_errorbar(aes(ymin = mean.peakactvnsts - se.peakactvnsts, ymax = mean.peakactvnsts + se.peakactvnsts), width=.2, position=position_dodge(.9), color = zcolor)  +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(colour = zcolor)) +
  ylab("Mean number of nests per colony (± SE)") +
  scale_x_continuous(limits = c(1987, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") +
  ggtitle(zspec.name) +
  geom_text(aes(x = year, y = panel.ylim, label = num.colonies.spp), size = 3) +
  annotate("text", x = 1987, y = panel.ylim, label = "# colonies", size = 3)

 if(save.plot == TRUE) {
ggsave(paste("figures_output/colony_size/HEP_colonySize_NorthBay_", zspp, ".jpg", sep = ""), width = 8, height = 4, units = "in")
} else {
  return(zplot)
} 

}
mean_col_size_yr_spp_plotter("GREG", save.plot = FALSE)

map(zsppz, mean_col_size_yr_spp_plotter)

# entire N Bay, by species -----
# 

mean_col_size_spp_subreg_plotter <- function(zspp, save.plot = TRUE) {

  mean_col_size_yr_sp_subreg <- hep %>% 
  filter(species == zspp) %>% 
  group_by(year, subregion, species) %>% 
  summarise(mean.peakactvnsts = mean(peakactvnsts),
            mean.peakactvnsts = round(mean.peakactvnsts, 1),
            sd.peakactvnsts = sd(peakactvnsts),
            num.colonies.spp = n(),
            se.peakactvnsts = sd.peakactvnsts/sqrt(num.colonies.spp),
            sd.peakactvnsts = round(sd.peakactvnsts, 1),
            se.peakactvnsts = round(se.peakactvnsts, 1)) %>% 
  ungroup() %>% 
  left_join(., num_colonies) %>% 
  left_join(., spp.name) %>% 
  left_join(., hep.subreg.key)
  
zcolor = ifelse(zspp == "BCNH", brewer.pal(8, "Dark2")[1],
                ifelse(zspp == "CAEG", brewer.pal(8, "Dark2")[2],
                       ifelse(zspp == "GBHE", brewer.pal(8, "Dark2")[3],
                              ifelse(zspp == "GREG", brewer.pal(8, "Dark2")[4],
                                     brewer.pal(8, "Dark2")[5]))))


zspec.name = distinct(mean_col_size_yr_sp_subreg, spp.name)
panel.ylim <- max(mean_col_size_yr_sp_subreg$mean.peakactvnsts) / -10

zplot = ggplot(data = mean_col_size_yr_sp_subreg, aes(x = year, y = mean.peakactvnsts))  +
  geom_point(aes(x = year, y = mean.peakactvnsts), color = zcolor) +
  stat_smooth(aes(x = year, y = mean.peakactvnsts), color = zcolor, span = .75, se = F) +
  geom_errorbar(aes(ymin = mean.peakactvnsts - se.peakactvnsts, ymax = mean.peakactvnsts + se.peakactvnsts), width=.2, position=position_dodge(.9), color = zcolor)  +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        plot.title = element_text(colour = zcolor)) +
  ylab("Mean number of nests per colony (± SE)") +
  scale_x_continuous(limits = c(1989, 2020), breaks = c(1990, 2000, 2010, 20)) +
  xlab("") +
  facet_wrap(~subreg.name) +
  ggtitle(zspec.name) +
  geom_text(aes(x = year, y = panel.ylim, label = num.colonies.spp), size = 2)

 if(save.plot == TRUE) {
ggsave(paste("figures_output/colony_size/HEP_colonySize_subreg_", zspp, ".jpg", sep = ""), width = 10, height = 4, units = "in")
} else {
  return(zplot)
} 

}
mean_col_size_spp_subreg_plotter("GBHE", save.plot = FALSE)

map(zsppz, mean_col_size_spp_subreg_plotter)

# mean colony size -----
mean_colony_sizer <- function(){
# add all nests from child sites into parent site sum
  par_site_spp <- hep %>% 
  filter(species != "DCCO") %>% 
  group_by(parent.site.name, species, year) %>% 
  summarise(tot.peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup() 
#--- mean by species, site across all years
par_site_spp_mean <- par_site_spp %>% 
  group_by(parent.site.name, species) %>% 
  summarise(MeanPeakActiveNests = mean(tot.peakactvnsts)) %>% 
  ungroup()
  
site_spp_mean_wide <- par_site_spp_mean %>% 
  ungroup() %>% 
  mutate(species = paste(species, "MeanPeakActiveNests", sep = "")) %>% 
  spread(species, MeanPeakActiveNests)
#---
par_site_spp_sd <- par_site_spp %>% 
  group_by(parent.site.name, species) %>%
  summarise(StDevPeakActiveNests = sd(tot.peakactvnsts))

site_spp_sd_wide <- par_site_spp_sd %>% 
  ungroup() %>% 
  mutate(species = paste(species, "StDevPeakActiveNests", sep = "")) %>% 
  spread(species, StDevPeakActiveNests)
#---
sitez_codez <- hep %>% 
  select(parent.site.name, parent.code, subregion) %>% 
  distinct()

site_spp_summ <- full_join(site_spp_mean_wide, site_spp_sd_wide) %>% 
  full_join(., sitez_codez) %>% 
  select(parent.site.name, parent.code, subregion, contains("GREG"), contains("GBHE"), contains("SNEG"), contains("BCNH"), contains("CAEG"))

return(site_spp_summ)
}
site_spp_summ <- mean_colony_sizer()
write.csv(site_spp_summ, "colony_species_mean_sd.csv", row.names = F)


# total nests by subregion, species, year -----

subreg_year_peakactvizer <- function(){
par_site_spp <- hep %>% 
  filter(species != "DCCO") %>% 
  group_by(parent.site.name, species, year) %>% 
  summarise(tot.peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  full_join(., select(hep, parent.site.name, parent.code, subregion) %>% 
              distinct())

subreg_year_summary <- par_site_spp %>% 
  group_by(subregion, species, year) %>% 
  summarise(subreg.total.nests = sum(tot.peakactvnsts)) %>% 
  arrange(subregion, species, year) %>% 
  filter(!is.na(subreg.total.nests)) %>% 
  ungroup()
  
}
subreg_year_peakactvnsts = subreg_year_peakactvizer()

subreg_year_peakactvnsts <- subreg_year_peakactvnsts %>% 
  full_join(., hep.subreg.key) %>% 
  filter(!is.na(subregion), subregion != "") %>% 
  filter(species %in% c("GREG", "GBHE", "SNEG", "BCNH", "CAEG"))

# fill in 0s for missing data
subreg_year_peakactvnsts_all0s <- subreg_year_peakactvnsts %>% 
  spread(year, subreg.total.nests) %>% 
  replace(is.na(.), 0) %>% 
  gather(year, subreg.total.nests, -subregion, -species, -subreg.name) %>% 
  mutate(year = as.numeric(year))

# filter subregion and species ----
hep_sub_spp_filter <- function(zdf, zsub, zspp) {
  subreg_dat = subreg_year_peakactvnsts_all0s
  if(missing(zsub)) {
    subreg_dat = subreg_dat
  } else {
   subreg_dat <- filter(subreg_dat, subregion == zsub)
  }
  
  if(missing(zspp)) {
    subreg_dat = subreg_dat
  } else {
   subreg_dat <- filter(subreg_dat, species == zspp)
  }
}
# name file for output by subregion and species ----
hep_sub_spp_file_namer <- function(zsub, zspp) {
  
  if(missing(zsub)) {
    zsub.name = "allSubreg"
  } else {
   zsub.name =  filter(hep.subreg.key, subregion == zsub)[[2]]
  }
  
  if(missing(zspp)) {
    zspp.name = "allSpp"
  } else {
   zspp.name = zspp 
  }
  zfile.name = paste("figures_output/HEP_trends_", zsub.name, "_", zspp.name, ".jpg", sep = "")
return(zfile.name)
}
parms = list(
  zsub = "OUC"
  )

# subregion abbreviation list ----
sub_regz <- c("OUC", "RUR", "NNC", "PNM", "CSF", "SUS", "IEB")

# plot each subregion, all spp together ----
subreg_plotter <- function(zsub) {

subreg_dat <- subreg_year_peakactvnsts %>% 
  filter(subregion == zsub) %>% 
  left_join(., spp.name)
zsub.name =  filter(hep.subreg.key, subregion == zsub)[[2]]

ggplot(data = subreg_dat, aes(x = year, y = subreg.total.nests, color = spp.name))  +
  geom_point(aes(x = year, y = subreg.total.nests)) +
  stat_smooth(aes(x = year, y = subreg.total.nests), span = .75) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_color_brewer(palette = "Dark2") +
  ylab("Number of nests") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") 
  
ggsave(paste("figures_output/colony_size/HEP_colonySize_", zsub.name, "_allSpp.jpg", sep = ""), width = 6, height = 4, units = "in")
}
subreg_plotter("OUC")

map(sub_regz, subreg_plotter)

# individual species by subregion -----


zsppz <- c("BCNH", "CAEG", "GBHE", "GREG", "SNEG")
zsppz2 <- rep(zsppz, length(sub_regz))
sub_regz2 <- rep(sub_regz, length(zsppz))

subreg_spp_plotter <- function(zsub, zspp) {
zspp = "BCNH"
zcolor = ifelse(zspp == "BCNH", brewer.pal(8, "Dark2")[1],
                ifelse(zspp == "CAEG", brewer.pal(8, "Dark2")[2],
                       ifelse(zspp == "GBHE", brewer.pal(8, "Dark2")[3],
                              ifelse(zspp == "GREG", brewer.pal(8, "Dark2")[4],
                                     brewer.pal(8, "Dark2")[5]))))

subreg_dat <- subreg_year_peakactvnsts %>% 
  filter(subregion == zsub, species == zspp) %>% 
  left_join(., spp.name)
zsub.name =  filter(hep.subreg.key, subregion == zsub)[[2]]

ggplot(data = subreg_dat, aes(x = year, y = subreg.total.nests))  +
  geom_point(aes(x = year, y = subreg.total.nests), color = zcolor) +
  stat_smooth(aes(x = year, y = subreg.total.nests), color = zcolor, span = .75) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  ylab("Number of nests") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") 
  
ggsave(paste("figures_output/colony_size/HEP_colonySize_", zsub.name, "_", zspp, ".jpg", sep = ""), width = 6, height = 4, units = "in")
}
subreg_spp_plotter("OUC", "GBHE")

map2(sub_regz2, zsppz2, subreg_spp_plotter)

# lump all species, plot entire region -----

nbay_year_peakactvnsts_sppCombined <- subreg_year_peakactvnsts %>% 
  group_by(year) %>% 
  summarise(total.nests = sum(subreg.total.nests))

ggplot(data = nbay_year_peakactvnsts_sppCombined, aes(x = year, y = total.nests))  +
  geom_point(aes(x = year, y = total.nests), color = "#E6AB02") +
  stat_smooth(aes(x = year, y = total.nests), color = "#E6AB02", span = .75) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_color_brewer(palette = "Dark2") +
  ylab("Number of nests") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") 
  
ggsave("figures_output/colony_size/HEP_colonySize_NorthBay_allSppCombined.jpg", width = 6, height = 4, units = "in")

# plot entire region, all spp together -----
nbay_year_peakactvnsts <- subreg_year_peakactvnsts %>% 
  group_by(species, year) %>% 
  summarise(total.nests = sum(subreg.total.nests))

allhep_plotter <- function() {

subreg_dat <- nbay_year_peakactvnsts %>% 
  left_join(., spp.name)

ggplot(data = subreg_dat, aes(x = year, y = total.nests, color = spp.name))  +
  geom_point(aes(x = year, y = total.nests)) +
  stat_smooth(aes(x = year, y = total.nests), span = .75) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_color_brewer(palette = "Dark2") +
  ylab("Number of nests") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") 
  
ggsave("figures_output/colony_size/HEP_colonySize_NorthBay_allSpp.jpg", width = 6, height = 4, units = "in")
}
allhep_plotter()

# entire N Bay, by species -----

zsppz <- c("BCNH", "CAEG", "GBHE", "GREG", "SNEG")

spp_plotter <- function(zspp) {

zcolor = ifelse(zspp == "BCNH", brewer.pal(8, "Dark2")[1],
                ifelse(zspp == "CAEG", brewer.pal(8, "Dark2")[2],
                       ifelse(zspp == "GBHE", brewer.pal(8, "Dark2")[3],
                              ifelse(zspp == "GREG", brewer.pal(8, "Dark2")[4],
                                     brewer.pal(8, "Dark2")[5]))))

subreg_dat <- nbay_year_peakactvnsts %>% 
  filter(species == zspp) %>% 
  left_join(., spp.name)

ggplot(data = subreg_dat, aes(x = year, y = total.nests))  +
  geom_point(aes(x = year, y = total.nests), color = zcolor) +
  stat_smooth(aes(x = year, y = total.nests), color = zcolor, span = .75) +
  theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  ylab("Number of nests") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") 
  
ggsave(paste("figures_output/colony_size/HEP_colonySize_NorthBay_", zspp, ".jpg", sep = ""), width = 6, height = 4, units = "in")
}
spp_plotter("GBHE")

map(zsppz, spp_plotter)

# set brewer colors for each species ----

zspp = c("BCNH", "CAEG", "GBHE", "GREG", "SNEG")
zcols = c(brewer.pal(8, "Dark2")[1],
          brewer.pal(8, "Dark2")[2],
          brewer.pal(8, "Dark2")[3],
          brewer.pal(8, "Dark2")[4],
          brewer.pal(8, "Dark2")[5])
spp.cols <- cbind(zspp, zcols)
 






# plot Changes in number of nesting herons and egrets by subregion -----

ggplot(data = subreg_year_peakactvnsts, aes(x = year, y = subreg.total.nests, color = species))  +
  geom_point(aes(x = year, y = subreg.total.nests, color = species)) +
  stat_smooth(aes(x = year, y = subreg.total.nests, color = species)) + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  ylab("Number of nests") +
  scale_x_continuous(limits = c(1990, 2020), breaks = c(1990, 2000, 2010, 2020)) +
  xlab("") +
  ggtitle("Changes in number of nesting herons and egrets by subregion")

ggsave("figures_output/HEP_nest_numbers_20190926_2col_noleg.jpg", width = 6, height = 6, units = "in")
# basic summary for each colony -----

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



# fit lm to peak active nests for a colony, get coefficients ----
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
# summarize effort for each subregion ----
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


# more basic summaries ----
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

      
# compare current year to previous year and all previous year, for single colony ----
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




# compare current year to previous year and all previous year, for subregion ----
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

# changes in colony size by subregion, with forest plot ----
subreg.col.changes <- colony.changes %>% 
  data.frame() %>% 
  left_join(., select(hep.sites, code, subregion, site_name)) %>% 
  mutate(site_name = as.character(site_name)) %>% 
  rbind(., subreg.changes) %>% 
  mutate() %>% 
  mutate_if(is.numeric, list(~na_if(., Inf)))


spp.subreg.forest_plotter <- function(spp, subreg){

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







