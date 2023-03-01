

library(tidyverse)
library(devtools)
library(RColorBrewer)
library(colorspace)
library(here)
library(plotly)
library(birdnames)

options(scipen = 999)
source("C:/Users/scott.jennings/Documents/Projects/core_monitoring_research/HEP/HEP_data_work/HEP_code/HEP_utility_functions.R")

# data
hep_sites <- readRDS(here("HEP_data/HEP_site_names_nums_utm"))



# function to make the figure
hep_colony_plotter <- function(zcolony) {

# zcolony = c(27, 27.1)  
  
abundance <- readRDS(here("HEP_data/hep_annual_nest_abundance")) %>% 
  filter(code %in% zcolony) %>% 
  cut_never_nested() %>% 
  filter(peakactvnsts >= 0)
  

productivity <- readRDS(here("HEP_data/hep_annual_new_productivity")) %>% 
  filter(code %in% zcolony)

colony_changes <- readRDS(here("HEP_data/colony_changes_bycode")) %>% 
  filter(code %in% zcolony)
  
  
hep_stats <- abundance %>% 
  full_join(productivity) %>% 
  full_join(colony_changes) %>% 
  select(year, site.name, code, species, peakactvnsts, mean.chx.per.nest, per.change) %>% 
  pivot_longer(cols = c(peakactvnsts, mean.chx.per.nest, per.change)) %>% 
  full_join(., data.frame(name = c("peakactvnsts", "mean.chx.per.nest", "per.change"),
                          varb.label = c("Total nests", "Mean chicks per nest", "% change # nests"),
                          facet.label = c("Colony size", "Productivity", "Change in colony size"))) %>% 
  left_join(spp_color_name %>% filter(spp.name != "All"))


colony_plot <- hep_stats %>%
ggplot() +
  geom_point(aes(year, value, color = spp.name,
                 text = paste(year,  "\n",
                              spp.name,  "\n",
                              "Value = ", round(value, 1)))) +
  #stat_smooth(aes(year, value, color = spp.name), se = FALSE) +
  scale_color_manual(values = spp_color_name$spp.color) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  labs(y = "",
       x = "Year",
       color = "") +
  facet_wrap(vars(site.name, varb.label), ncol = length(zcolony), scales = "free_y", strip.position="right")

ggplotly(colony_plot, tooltip = "text")
}


hep_colony_plotter(53)
hep_colony_plotter(c(27, 27.1))


ggsave(here("figures_output/Bolinas_colony_viewer.png"))
