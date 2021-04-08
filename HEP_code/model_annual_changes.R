
fit_mods <- function(zspecies) {
  
hep_changes_rain  <- hep_changes_rain %>% 
  filter(species == zspecies, !zero2some) %>% 
  mutate(parent.code = as.character(parent.code))
# year only
year <- hep_changes_rain %>% 
  lm(per.change.1year ~ year + parent.code, data = .)

# rain only
rain <- hep_changes_rain %>% 
  lm(per.change.1year ~ rain + parent.code, data = .)

# rain and year
year_rain <- hep_changes_rain %>% 
  lm(per.change.1year ~ rain + year + parent.code, data = .)
# rain^2 and year
year_rain2 <- hep_changes_rain %>% 
  lm(per.change.1year ~ I(rain^2) + year + parent.code, data = .)

mods <- list(year, rain, year_rain, year_rain2)
return(mods)
}


greg_mods <- fit_mods("GREG")
gbhe_mods <- fit_mods("GBHE")
bcnh_mods <- fit_mods("BCNH")
sneg_mods <- fit_mods("SNEG")

mod_list_order <- c("year", "rain", "year_rain", "year_rain2")


(greg_aic <- aictab(greg_mods, mod_list_order))
(gbhe_aic <- aictab(gbhe_mods, mod_list_order))
(bcnh_aic <- aictab(bcnh_mods, mod_list_order))
(sneg_aic <- aictab(sneg_mods, mod_list_order))


greg_best <- greg_mods[which(as.list(mod_list_order) == filter(greg_aic, Delta_AICc == 0)$Modnames)[[1]]][[1]]
gbhe_best <- gbhe_mods[which(as.list(mod_list_order) == filter(gbhe_aic, Delta_AICc == 0)$Modnames)[[1]]][[1]]
bcnh_best <- bcnh_mods[which(as.list(mod_list_order) == filter(bcnh_aic, Delta_AICc == 0)$Modnames)[[1]]][[1]]
sneg_best <- sneg_mods[which(as.list(mod_list_order) == filter(sneg_aic, Delta_AICc == 0)$Modnames)[[1]]][[1]]


filter(hep_changes_rain, species == "GREG", !zero2some) %>% 
  cbind(fitted(greg_best) %>% data.frame() %>% rename(fitted = 1)) %>% 
  pivot_longer(cols = c(per.change.1year, fitted)) %>% 
  ggplot() +
  geom_point(aes(year, value, color = name))
# temporal autocor AR1, all study area combined - this doesn't work with per.change.1year b/c missing values break autcorr

fit_mods_ac <- function(zspecies) {
# year only
year_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ year, data = ., correlation = corAR1(form =~ year), method = "ML")

# rain only, but still with year AR1
rain_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ mean.rain, data = ., correlation = corAR1(form =~ year), method = "ML")

# rain and year, but still with year AR1
year_rain_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ mean.rain + year, data = ., correlation = corAR1(form =~ year), method = "ML")
# rain^2 and year, but still with year AR1
year_rain2_ar1 <- spp_rain_year %>% 
  filter(species == zspecies) %>% 
  gls(nest.abund ~ I(mean.rain^2) + year, data = ., correlation = corAR1(form =~ year), method = "ML")

mods <- list(year_ar1, rain_ar1, year_rain_ar1, year_rain2_ar1)
return(mods)
}


greg_mods_ac <- fit_mods_ac("GREG")
gbhe_mods_ac <- fit_mods_ac("GBHE")


aictab(greg_mods_ac, c("year_ar1", "rain_ar1", "year_rain_ar1", "year_rain2_ar1"))
aictab(gbhe_mods_ac, c("year_ar1", "rain_ar1", "year_rain_ar1", "year_rain2_ar1"))



