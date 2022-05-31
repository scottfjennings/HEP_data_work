



# 1 packages ----
library(tidyverse)
library(RColorBrewer)
library(RODBC)
library(lubridate)
library(here)
library(birdnames)

options(scipen = 999)
source(here("HEP_code/HEP_utility_functions.R"))
# source("C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_code/HEP_data_summary_functions.R")


hepdata_location = here("HEP_data/HEPDATA.accdb")
# all these functions are in HEP_utility_functions.R
hep_start <- hep_from_access(hepdata_location)
hep_sites <- hep_sites_from_access(hepdata_location) 


hep <- hep_start %>% 
  clean_hep() %>% 
  filter(peakactvnsts >= 0) %>% 
  right_join(., dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion))  %>% 
  trim_hep_columns(stage = FALSE) %>% 
  cut_never_nested() %>% 
  mutate(site.name = as.character(site.name)) 

# ----



stage4_chicks <- hep %>% 
  select(year, species, code, contains("brd")) %>% 
  pivot_longer(contains("brd"), names_to = "brood.size", values_to = "num.nests") %>% 
  filter(num.nests > 0) %>% 
  mutate(brood.size = gsub("brd", "", brood.size),
         brood.size = as.numeric(brood.size)) %>% 
  filter(!is.na(num.nests)) %>% 
  group_by(year, code, species) %>% 
  summarise(total.stage4.chx = sum(brood.size * num.nests),
            stage4.nests = sum(num.nests))

num_nests_chicks <- hep %>% 
  select(species, year, code, peakactvnsts, numbervisits, contains("foc")) %>% 
  right_join(stage4_chicks) %>% 
  mutate(chx.per.attempted.nest = total.stage4.chx/peakactvnsts,
         chx.per.fledged.nest = total.stage4.chx/stage4.nests,
         survival.new = chx.per.attempted.nest/chx.per.fledged.nest,
         prop.stage4.nests = stage4.nests/peakactvnsts,
         nest.surv = (focalnests - focfailure)/focalnests,
         old.productivity = nest.surv * chx.per.fledged.nest)

num_nests_chicks %>% 
  filter(species %in% c("GREG", "GBHE", "SNEG", "BCNH")) %>% 
  ggplot() +
  geom_point(aes(x = old.productivity, y = chx.per.attempted.nest)) +
  facet_wrap(~species) +
  geom_abline() +
  ylab("New productivity estimate") +
  xlab("Old productivity estimate")

ggsave("figures_output/compare_productivity.png", height = 6, width = 6)

## can nest survival of focal nests be predicted by ratios of nests in each stage at different times? ----

stages_long <- hep %>% 
  select(year, code, species, contains(c("stage"))) %>% 
  pivot_longer(contains("stage"), names_to = "mon.stage", values_to = "num.nests") %>% 
  separate(mon.stage, 
           into = c("rop", "stage"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
  mutate(stage = as.numeric(stage),
         guarded = ifelse(stage > 3, "guarded", "unguarded"),
         rop = gsub("stage", "", rop))

rop_dates_long <- hep %>% 
  select(year, code, species, contains(c("stg"))) %>% 
  pivot_longer(contains("stg"), names_to = "rop", values_to = "date") %>% 
  mutate(rop = gsub("date", "", rop),
         rop = gsub("stge", "", rop),
         rop = gsub("stg", "", rop)) %>% 
  filter(!is.na(date))


stages_dates_long <- full_join(rop_dates_long, stages_long) %>% 
  mutate(date = as.Date(date),
         yday = yday(date))


stages_dates_long %>% 
  filter(!species %in% c("DCCO", "CAEG"), code == 1, stage %in% c(1, 4)) %>% 
  mutate(stage = factor(stage)) %>% 
  ggplot() +
  geom_point(aes(x = yday, y = num.nests, group = stage, color = stage)) +
  stat_smooth(aes(x = yday, y = num.nests, group = stage, color = stage), se = FALSE, span = 1) +
  facet_wrap(~species, scales = "free_y")

peak_stage1 <- stages_dates_long %>%
  filter(stage == 1, num.nests > 0) %>% 
  group_by(year, code, species) %>% 
  filter(num.nests == max(num.nests)) %>% 
  ungroup() %>% 
  select(year, code, species, peak.stage1 = num.nests) 

peak_stage4 <- stages_dates_long %>%
  filter(stage == 4, num.nests > 0) %>% 
  group_by(year, code, species) %>% 
  filter(num.nests == max(num.nests)) %>% 
  ungroup() %>% 
  select(year, code, species, peak.stage4 = num.nests)

peak_stage1_4 <- full_join(peak_stage1, peak_stage4)

ggplot(peak_stage1_4)+
  geom_point(aes(peak.stage1, peak.stage4))+
  geom_abline()



guard_unguard <- stages_long %>% 
  group_by(code, species, year, rop, guarded) %>% 
  summarise(num.nests = sum(num.nests)) %>% 
  ungroup() %>% 
  filter(!is.na(num.nests)) 

guard_unguard_ratios <- guard_unguard %>% 
  pivot_wider(id_cols = c("code", "species", "year", "rop"), values_from = num.nests, names_from = guarded) %>% 
  mutate(guarded = ifelse(is.na(guarded), 0, guarded),
         unguarded = ifelse(is.na(unguarded), 0, unguarded),
         tot.stage.nests = (guarded + unguarded),
         guard.unguard.ratio = guarded / tot.stage.nests)


guard_unguard_ratios %>% 
  filter(!species %in% c("DCCO", "CAEG")) %>% 
  mutate(rop = factor(rop, levels = c("mar", "ltmar", "apr", "may", "jun", "ltjun", "jul"))) %>% 
ggplot() +
  geom_point(aes(x = rop, y = tot.stage.nests)) +
  facet_wrap(~species)



nest_survival <- hep %>% 
  select(code, year, species, peakactvnsts, contains("foc")) %>% 
  mutate(nest.survival = (focalnests - focfailure)/focalnests)


nest_surv_guard_ratio <- guard_unguard_ratios %>% 
  pivot_wider(id_cols = c("code", "species", "year"), names_from = rop, values_from = c("tot.stage.nests", "guard.unguard.ratio")) %>% 
  full_join(nest_survival) %>% 
  select(year, code, species, peakactvnsts, contains("foc"), nest.survival, contains("mar"), contains("ltmar"), contains("apr"), contains("may"), contains("jun"), contains("ltjun"), contains("jul"))

nest_surv_guard_ratio %>% 
  filter(!species %in% c("DCCO", "CAEG")) %>% 
ggplot() +
  geom_point(aes(x = (guard.unguard.ratio_jun * tot.stage.nests_jun)/peakactvnsts, y = nest.survival)) +
  facet_wrap(~species) +
  geom_abline()



peak_stage1_4 %>% 
  mutate(ratio.1.4 = peak.stage1/peak.stage4) %>% 
  full_join(nest_survival)%>% 
  filter(!species %in% c("DCCO", "CAEG")) %>% 
ggplot() +
  geom_point(aes(x = ratio.1.4, y = nest.survival)) +
  facet_wrap(~species) +
  geom_abline()
