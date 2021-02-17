
library(tidyverse)
library(RODBC)
library(RColorBrewer)

source("HEP_code/HEP_utility_functions.r")
core.ardeids <- c("GBHE", "GREG", "SNEG", "BCNH")

hep <- hep_from_access()  %>% 
  rename_all(tolower) %>% 
  #filter(peakactvnsts > -1, year > 1989) %>% 
  #mutate(code = as.numeric(as.character(code))) %>% 
  add_site_names() %>% # changes _ to . in field names
  cut_never_nested()



# number of colonies -----
nba.num.co.per.year.spp <- hep %>%
  select(code, year, species, peakactvnsts) %>% 
  group_by(year, species) %>% 
  summarise(num.col = n(),
            mean.num.nests = mean(peakactvnsts)) %>% 
  ungroup()

nba.num.co.per.year.spp %>% 
  filter(species %in% core.ardeids) %>% 
ggplot() +
  geom_point(aes(x = year, y = num.col, color = species)) +
  geom_line(aes(x = year, y = mean.num.nests, color = species)) +
  facet_wrap(~species) +
  theme(legend.position = "none") +
  ylab("# Colonies surveyed")
#--
nba.num.co.per.year <- hep %>%
  filter(peakactvnsts > 0) %>% 
  select(code, year, species, peakactvnsts) %>% 
  distinct(year, code) %>% 
  group_by(year) %>% 
  summarise(num.col = n())
#--
nba.breed.pop <- hep %>%
  select(code, year, species, peakactvnsts) %>% 
  group_by(year, species) %>% 
  summarise(yr.spp.tot.pop = sum(peakactvnsts))


ggplot(data = filter(nba.breed.pop, species %in% core.ardeids), aes(x = year, y = yr.spp.tot.pop, color = species)) +
  geom_point() +
  facet_wrap(~species) +
  theme(legend.position = "none") +
  ylab("# Breeding pairs")


# colony size ----


# single colony trend ----

zcode = 1

hep <- hep %>%
  filter(species %in% core.ardeids) %>% 
  define_species_colors() %>% 
  cut_leading_0s() %>% 
  trim_hep_columns() %>% 
  calc_mean_brood_size()


  hep_colony_size_plotter(colony.code = zcode, save.plot = F)

  
  ggplot(aes(x = year, y = yr.spp.tot.pop, color = species)) +
  geom_point() +
  stat_smooth(aes(x = year, y = yr.spp.tot.pop, color = species)) +
  ylab("# Breeding pairs") +
  ggtitle(col_name[1])

col_name <- distinct(col_breed_pop, site.name)
col_breed_pop %>% 
  define_species_colors() 



# subregional breeding population trend ----
subreg.breed.pop <- hep_w_sites %>%
  select(code, year, species, peakactvnsts, subregion) %>% 
  group_by(subregion, year, species) %>% 
  summarise(yr.spp.tot.pop = sum(peakactvnsts))


ggplot(data = filter(subreg.breed.pop, species %in% core.ardeids), aes(x = year, y = yr.spp.tot.pop, color = species)) +
  geom_point() +
  facet_wrap(~ subregion) +
  ylab("# Breeding pairs")

# colony productivity trend

hep_brood_size_plotter(colony.code = zcode, save.plot = F)

# entire region productivity trends ---------------

nba.productivity <- hep %>%
  select(code, year, species, peakactvnsts, contains("brd"), focalnests, focfailure) %>%
  mutate(num.chx = (brd1 * 1) + (brd2 * 2) + (brd3 * 3) + (brd4 * 4) + (brd5 * 5) + (brd6 * 6),
         nests4brd = (brd1 + brd2 + brd3 + brd4 + brd5 + brd6),
         #focfledge.match = ifelse(focalnests - focfailure == nests4brd, T, F),
         chx.per.fledg.nest = round(num.chx/nests4brd, 1),
         nest.succ = round((focalnests - focfailure)/focalnests, 2))

mean.chx.per.fledg.nest <- nba.productivity %>% 
  select(year, species, chx.per.fledg.nest) %>% 
  drop_na() %>% 
  group_by(year, species) %>% 
  summarise(mean.chx.per.fledg.nest = round(mean(chx.per.fledg.nest, rm.nan = T), 1)) %>% 
  arrange(year, species)

mean.nest.succ <- nba.productivity %>% 
  select(year, species, nest.succ) %>% 
  drop_na() %>% 
  group_by(year, species) %>% 
  summarise(mean.nest.succ = round(mean(nest.succ, rm.nan = T), 2)) %>% 
  arrange(year, species)

nba.productivity.summ <- full_join(mean.chx.per.fledg.nest, mean.nest.succ) %>% 
  full_join(., nba.breed.pop) %>% 
  mutate(chx.per.nest = mean.chx.per.fledg.nest * mean.nest.succ)

ggplot(data = filter(nba.productivity.summ, species %in% core.ardeids), aes(x = year, y = chx.per.nest, color = species)) +
  geom_point() +
  facet_wrap(~ species) +
  theme(legend.position = "none") +
  ylab("# Chicks produced per nest") +
  stat_smooth(method = "lm")

# subregional trends in productivity ---------------

subreg.productivity <- hep_w_sites %>%
  select(code, year, species, subregion, peakactvnsts, contains("brd"), focalnests, focfailure) %>%
  mutate(num.chx = (brd1 * 1) + (brd2 * 2) + (brd3 * 3) + (brd4 * 4) + (brd5 * 5) + (brd6 * 6),
         nests4brd = (brd1 + brd2 + brd3 + brd4 + brd5 + brd6),
         #focfledge.match = ifelse(focalnests - focfailure == nests4brd, T, F),
         chx.per.fledg.nest = round(num.chx/nests4brd, 1),
         nest.succ = round((focalnests - focfailure)/focalnests, 2))

mean.chx.per.fledg.nest <- subreg.productivity %>% 
  select(year, species, chx.per.fledg.nest, subregion) %>% 
  drop_na() %>% 
  group_by(subregion, year, species) %>% 
  summarise(mean.chx.per.fledg.nest = round(mean(chx.per.fledg.nest, rm.nan = T), 1)) %>% 
  arrange(year, species)

mean.nest.succ <- subreg.productivity %>% 
  select(subregion, year, species, nest.succ) %>% 
  drop_na() %>% 
  group_by(subregion, year, species) %>% 
  summarise(mean.nest.succ = round(mean(nest.succ, rm.nan = T), 2)) %>% 
  arrange(year, species)

subreg.productivity.summ <- full_join(mean.chx.per.fledg.nest, mean.nest.succ) %>% 
  full_join(., subreg.breed.pop) %>% 
  mutate(chx.per.nest = mean.chx.per.fledg.nest * mean.nest.succ)

ggplot(data = filter(subreg.productivity.summ, species %in% core.ardeids), aes(x = year, y = chx.per.nest, color = species)) +
  geom_point() +
  facet_wrap(~ subregion) +
  ylab("# Chicks produced per nest") +
  stat_smooth(method = "lm")


