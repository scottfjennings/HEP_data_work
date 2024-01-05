


# 1 packages ----
library(tidyverse)
library(here)
library(lubridate)
library(birdnames)

options(scipen = 999)
source(here("HEP_code/HEP_utility_functions.R"))

hep <- readRDS("HEP_data/hep_annual_nest_abundance")
sites <- readRDS(here("HEP_data/HEP_site_names_nums_utm"))

spp_color = data.frame(alpha.code = c("BCNH", "CAEG", "GBHE", "GREG", "SNEG", "All", "DCCO"),
                    spp.color = c(brewer.pal(8, "Dark2")[1], brewer.pal(8, "Dark2")[2], brewer.pal(8, "Dark2")[3], brewer.pal(8, "Dark2")[4], brewer.pal(8, "Dark2")[5], brewer.pal(8, "Dark2")[6], brewer.pal(8, "Dark2")[7]))


tomales_colonies <- filter(sites, code %in% c(32.000, 32.100,
                                              50.000, 50.200, 50.999, 50.300, 50.100,
                                              83.000,
                                              113.000, 113.100,
                                              114.000,
                                              119.000,
                                              122.000,
                                              143.000,
                                              152.000,
                                              160.000, 160.100)) %>% 
                             mutate(marin.region = "Tomales Bay")

bolinas_colonies <- filter(sites, code %in% c(1, 1.1, 53, 53.1)) %>% 
                             mutate(marin.region = "Bolinas Lagoon")

ptreyes_colonies <- filter(sites, code %in% c(17, 25, 94)) %>% 
                             mutate(marin.region = "Point Reyes")

tomales_trends <- right_join(hep, tomales_colonies) %>% 
  rename(alpha.code = species) %>% 
  mutate(species = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         species = factor(species, levels = c("Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron", "Cattle Egret", "Double-crested Cormorant"))) 


tomales_trends %>% 
  group_by(species, alpha.code, year) %>% 
  summarise(total.nests = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  left_join(spp_color) %>% 
  filter(alpha.code %in% c("GBHE", "GREG")) %>% 
  ggplot() +
  geom_point(aes(x = year, y = total.nests, color = spp.color)) +
  stat_smooth(aes(x = year, y = total.nests, color = spp.color)) +
#  scale_color_brewer(palette = "Dark2", guide="none") +
  facet_wrap(~species, scales = "free_y", ncol = 2) +
  labs(title = "All Tomales colonies combined",
       y = "Total # nests",
       x = "Year",
       color = "") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(here("figures_output/Tomales_Bay_heronry_trends_GREG_GBHE.png"))



west_marin_trends <- bind_rows(tomales_colonies, bolinas_colonies, ptreyes_colonies) %>% 
  left_join(hep) %>% 
  rename(alpha.code = species) %>% 
  mutate(species = translate_bird_names(alpha.code, "alpha.code", "common.name"),
         species = factor(species, levels = c("Great Egret", "Great Blue Heron", "Snowy Egret", "Black-crowned Night-Heron", "Cattle Egret", "Double-crested Cormorant"))) 


west_marin_trends %>% 
  filter(peakactvnsts > 0) %>% 
  group_by(marin.region, species, alpha.code, year) %>% 
  summarise(total.nests = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  left_join(spp_color) %>% 
  filter(alpha.code %in% c("GBHE")) %>% 
  droplevels() %>% 
  ggplot() +
  geom_point(aes(x = year, y = total.nests), color = "#7570B3") +
  stat_smooth(aes(x = year, y = total.nests), color = "#7570B3") +
  facet_grid(marin.region~species, scales = "free_y") +
  labs(title = "",
       y = "Total # nests",
       x = "Year",
       color = "") +
  theme_bw() +
  theme(legend.position = "none")


ggsave(here("figures_output/West_Marin_heronry_trends_GBHE.png"))

