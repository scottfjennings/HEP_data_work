
library(tidyverse)

core.ardeids <- c("GBHE", "GREG", "SNEG", "BCNH")

hep.start <- read.csv("HEP_data/newer_hep.csv")
names(hep.start) <- tolower(names(hep.start))

hep <- hep.start %>% 
  filter(peakactvnsts > -1, year > 1989) %>% 
  mutate(code = as.numeric(as.character(code))) 



#---------------
nba.num.co.per.year <- hep %>%
  select(code, year, species, peakactvnsts) %>% 
  group_by(year, species) %>% 
  summarise(num.col = n())

ggplot(data = filter(nba.num.co.per.year, species %in% core.ardeids), aes(x = year, y = num.col, color = species)) +
  geom_point() +
  facet_wrap(~species) +
  theme(legend.position = "none") +
  ylab("# Colonies surveyed")


#---------------
nba.breed.pop <- hep %>%
  select(code, year, species, peakactvnsts) %>% 
  group_by(year, species) %>% 
  summarise(yr.spp.tot.pop = sum(peakactvnsts))


ggplot(data = filter(nba.breed.pop, species %in% core.ardeids), aes(x = year, y = yr.spp.tot.pop, color = species)) +
  geom_point() +
  facet_wrap(~species) +
  theme(legend.position = "none") +
  ylab("# Breeding pairs")


#---------------

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


