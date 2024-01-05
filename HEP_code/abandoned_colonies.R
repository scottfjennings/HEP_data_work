


library(tidyverse)
library(zoo)
library(here)
library(RColorBrewer)
source(here("HEP_code/HEP_utility_functions.R"))


hep<- readRDS("HEP_data/hep_annual_nest_abundance")


hep_all <- hep_from_access(here("HEP_data/HEPDATA.accdb")) 

names(hep_all) <- tolower(names(hep_all))


abandoned_by_spp <- hep %>% 
  filter(!is.na(peakactvnsts)) %>% 
  arrange(code, species, year) %>% 
  mutate(active = peakactvnsts > 0,
         active.last.3 = rollmean(active, 3, fill = NA, align = "right")) %>% 
  group_by(code, species) %>% 
  mutate(abandoned.spp = ifelse(peakactvnsts == 0 & lag(peakactvnsts) > 0 & lag(active.last.3) >= 1, TRUE, FALSE)) %>% 
  ungroup()

new_colonies <- hep %>% 
  filter(peakactvnsts > 0) %>% 
  group_by(code) %>% 
  filter(year == min(year)) %>% 
  ungroup() %>% 
  distinct(code, year) %>% 
  mutate(first.year = TRUE)


active_spp <- hep %>% 
  filter(!is.na(peakactvnsts), peakactvnsts > 0) %>% 
  group_by(code, year) %>% 
  summarise(species = paste(species, collapse = ", ")) %>% 
  ungroup()

abandoned <- hep %>% 
  filter(!is.na(peakactvnsts), peakactvnsts >= 0) %>% 
  group_by(code, year) %>% 
  summarise(peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  arrange(code, year) %>% 
  group_by(code) %>% 
  mutate(active = peakactvnsts > 0,
         last.3.mean = rollmean(active, 3, fill = NA, align = "right"),
         next.3.mean = rollmean(active, 3, fill = NA, align = "left"),
         #active.last.3 = lag(last.3.mean) >= 1,
         #empty.next.3 = next.3.mean <= 1/3
         ) %>% 
  group_by(code) %>% 
  mutate(abandoned = ifelse(peakactvnsts == 0 & 
                              lag(peakactvnsts) > 0 & 
                              lag(last.3.mean) >= 1 &
                              next.3.mean <= 1/3, TRUE, FALSE),
         recolonized = ifelse(peakactvnsts > 0 & 
                              lag(peakactvnsts) == 0  & 
                                lag(last.3.mean) <= 1/3 &
                                next.3.mean >= 1, TRUE, FALSE)) %>% 
  ungroup() %>% 
  full_join(active_spp)

change_status <- abandoned %>% 
  filter(abandoned == TRUE | recolonized == TRUE) %>% 
  distinct(code, year, abandoned, recolonized) %>% 
  full_join(new_colonies) %>% 
  pivot_longer(cols = c(abandoned, recolonized, first.year), names_to = "change.status") %>% 
  filter(value == TRUE) %>% 
  select(-value) %>% 
  arrange(code, year) %>% 
  group_by(code) 


run_years <- change_status %>% 
  group_by(code) %>% 
  mutate(run.years = year - lag(year))
  

colony_changes <- abandoned %>% 
  select(code, year, peakactvnsts) %>% 
  full_join(change_status) %>% 
  left_join(hep %>% distinct(code, site.name)) %>%
  left_join(active_spp) %>% 
  arrange(code, year) %>% 
  group_by(code) %>% 
  mutate(status = change.status) %>% 
  fill(status, .direction = "down") %>% 
  ungroup() %>% 
  mutate(status = case_when(status %in% c("first.year", "recolonized") ~ "active",
                            status == "abandoned" & is.na(change.status) ~ "inactive",
                            TRUE ~ status))






colony_changes %>%
  filter(year > 2000) %>% 
  distinct(code) %>% 
  nrow()   


colony_changes_summary <- colony_changes %>% 
  filter(!is.na(change.status)) %>% 
  group_by(year, change.status) %>% 
  summarise(num.cols = n()) %>% 
  filter(year > 1992) %>% 
  pivot_wider(id_cols = year, names_from = change.status, values_from = num.cols) %>% 
  pivot_longer(cols = c(abandoned, recolonized, first.year), names_to = "change.status", values_to = "num.cols") %>% 
  mutate(num.cols = ifelse(is.na(num.cols), 0, num.cols))




colony_changes_summary %>%
  filter(year > 2000) %>% 
  group_by(change.status) %>% 
  summarise(num.cols = sum(num.cols))

colony_changes_summary %>%
  filter(year > 2000) %>% 
  mutate(change.status = ifelse(change.status == "first.year", "new", change.status)) %>% 
  ggplot() +
  geom_col(aes(x = year, y = num.cols, fill = change.status), stat="identity", width=1, position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Year",
       y = "Number of colonies",
       fill = "",
       title = "Number of abandoned, recolonized, and\nnew colonies each year since 2000")+
    theme(text = element_text(size=20))


ggsave(here("figures_output/change_in_status.png"), width = 8)


colony_changes_summary %>% 
  pivot_wider(id_cols = year, names_from = change.status, values_from = num.cols) %>% 
  ggplot() +
  geom_point(aes(x = first.year, y = lag(abandoned)))


run_years %>% 
  filter(!is.na(run.years), year > 2000) %>% 
  group_by(change.status) %>% 
  mutate(mean.years = mean(run.years),
            sd.years = sd(run.years)) %>% 
  ungroup() %>% 
  mutate(change.status = ifelse(change.status == "abandoned", "Abandoned after x\nyears active",
                                "Recolonized after x\nyears inactive")) %>% 
  ggplot() +
  geom_point(aes(x = change.status, y = mean.years), size = 6) +
  geom_errorbar(aes(x = change.status, ymin = mean.years - sd.years, ymax = mean.years + sd.years), width = 0.25) + 
  geom_jitter(aes(x = change.status, y = run.years), color = "#E41A1C", width = 0.1, guide = FALSE) +
  ylim(0, 30) +
  labs(x = "",
       y = "Number of years",
       title = "Abandonment and recolonization\nevents since 2000") +
  theme_bw()+
    theme(text = element_text(size=20))


ggsave(here("figures_output/years_to_change_in_status.png"), width = 8)


# is there a relationship between number of disturbances and abandonment? ----


# Disturbance types: A = Avian, H = Human, W = Weather, M = Mammal, O = ACR field observer, P = Unknown Predator, U = unknown

# Disturbance results: 0 = none, 1 = behavioral response, 2 = nest failure, 3 = abandonment of colony, 4 = pre‐season disturbance



disturbance <- hep_all %>% 
  select(code, year, species, contains("dist"), -disturbance) %>% 
  filter(!is.na(dist1date)) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = contains("dist")) %>% 
  filter(!is.na(value)) %>%
  mutate(name = gsub("dist", "", name)) %>% 
  separate(name, into = c("dist.num", "dat"), sep = "(?<=[0-9])") %>% 
  pivot_wider(id_cols = c(code, year, species, dist.num), values_from = value, names_from = dat) %>% 
  select(-species, -dist.num) %>% 
  distinct() %>% 
  arrange(code, date) %>% 
  filter(!is.na(type)) %>% 
  mutate(code = as.numeric(code),
         year = as.numeric(year))

dist_per_year <- disturbance %>% 
  group_by(code, year) %>% 
  summarise(dist.per.year = n())


active_abandoned <- colony_changes %>% 
  filter(status %in% c("active", "abandoned")) %>% 
  left_join(dist_per_year) %>% 
  arrange(code, year) %>% 
  group_by(code) %>% 
  mutate(dist.per.year = replace_na(dist.per.year, 0),
         sum.dist.2 = dist.per.year + lag(dist.per.year),
         sum.dist.3 = dist.per.year + lag(dist.per.year) + lag(dist.per.year, 2))

active_abandoned %>% 
  select(code, year, status, contains("dist")) %>% 
  pivot_longer(cols = contains("dist")) %>% 
  mutate(name = case_when(name == "dist.per.year" ~ "this year",
                          name == "sum.dist.2" ~ "last 2 years",
                          name == "sum.dist.3" ~ "last 3 years"),
         name = factor(name, levels = c("this year", "last 2 years", "last 3 years")), 
         status = gsub("active", "stay\nactive", status),
         status = factor(status, levels = c("stay\nactive", "abandoned"))) %>% 
  group_by(status, name) %>% 
  mutate(mean.dist = mean(value, na.rm = TRUE),
            sd.dist = sd(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_jitter(aes(x = status, y = value), color = "#E41A1C", width = 0.1, guide = FALSE) +
  geom_point(aes(x = status, y = mean.dist), size = 4) +
  geom_errorbar(aes(x = status, ymin = mean.dist - sd.dist, ymax = mean.dist + sd.dist), width = 0.25) + 
  facet_wrap(~name) +
  theme_bw() +
  labs(y = "Number of disturbance events",
       x = "")+
    theme(text = element_text(size=20))

  
ggsave(here("figures_output/disturbance_abandoned.png"), width = 8)
