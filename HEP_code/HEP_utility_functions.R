

# a basic starting workflow for HEP data looks like this

#hep <- hep_from_access() %>% 
#  clean_hep() %>% 
#  add_site_names() %>% 
#  cut_never_nested() %>% 
#  cut_leading_0s() %>% 
#  trim_hep_columns()


# data reading ----

hep_from_access <- function(zdata.source = "HEP data", ztable = "tbl_HEPDATA") {
library(RODBC)
hep <- odbcConnect(dsn = zdata.source)
hep_data <- sqlFetch(hep, ztable)
close(hep)
rm(hep)
hep_data
}

#hep_start <- hep_from_access()

# 2 data management functions ----

# clean_hep(); fix a few data problems ----
clean_hep <- function(hep) { 
  # this function needs to be run first !!!!!
  # should not change number of rows or columns
  names(hep) <- tolower(names(hep))
  names(hep) <- gsub("_", ".", names(hep))

  hep_cleaned <- hep %>% 
  mutate(code = as.numeric(as.character(code)),
         species = as.character(species),
         species = ifelse(species == "GHBE", "GBHE", species),
         species = ifelse(species == "GBH", "GBHE", species)) %>% 
  arrange(code, year) %>% 
    ungroup()
}

# species order
hep_species_order <- data.frame(species = c("GREG", "GBHE", "BCNH", "SNEG", "CAEG", "DCCO"),
                                sp.order = seq(1:6)) 

# define_species_colors(); define standard species colors for plotting ----
define_species_colors <- function(hep) {
  # input df must be the outpout of clean_hep()
  # should not change number of rows, should add 2 columns
  spp_color_name = data.frame(species = c("BCNH", "CAEG", "GBHE", "GREG", "SNEG", "All", "DCCO"),
                    spp.color = c(brewer.pal(8, "Dark2")[1], brewer.pal(8, "Dark2")[2], brewer.pal(8, "Dark2")[3], brewer.pal(8, "Dark2")[4], brewer.pal(8, "Dark2")[5], brewer.pal(8, "Dark2")[6], brewer.pal(8, "Dark2")[7]),
                    spp.name = c("Black-crowned Night-heron", "Cattle Egret", "Great Blue Heron", "Great Egret", "Snowy Egret", "All", "Double-crested Cormorant")) 

hep <- left_join(hep, spp_color_name)  %>% 
  mutate(spp.color = as.character(spp.color)) %>% 
    ungroup()
}

# add_site_names(); combine with site names ----
add_site_names <- function(hep) { 
  # input df must be the outpout of clean_hep()
  # requires tbl_HEPSites.csv to be saved in HEP_data/
  # should not change number of rows, should add 5 columns
hep_sites <- hep_from_access(ztable = 'tbl_HEPSITES')
names(hep_sites) <- tolower(names(hep_sites))
names(hep_sites) <- gsub("_", ".", names(hep_sites))
hep <- left_join(hep, dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion)) %>% 
  mutate(code = as.character(code),
         parent.code = as.character(parent.code),
         parent.site.name = as.character(parent.site.name),
         parent.site.name = ifelse(parent.site.name == "" & parent.code == 160, 'Cypress Road', parent.site.name)) %>% 
  dplyr::select(code, parent.code, site.name, parent.site.name, everything(), -site)  %>% 
    ungroup()
}

# cut_never_nested(); exclude child colony X species combos where peakactvnsts = 0 in all years ----
cut_never_nested <- function(hep){
  # input df must be the outpout of clean_hep()
  # should not change number of columns, should cut number of rows
hep_no_never_nested <- hep %>% 
  group_by(code, species) %>% 
  summarise(mean.peakactvnsts = mean(peakactvnsts)) %>% 
  filter(mean.peakactvnsts > 0) %>% 
  mutate(actv.gr.eq.1yr = 1) %>% 
  full_join(., hep, by = c("code", "species")) %>% 
  filter(actv.gr.eq.1yr == 1) %>% 
  arrange(parent.site.name, species, year) %>% 
  ungroup() %>% 
  select(-mean.peakactvnsts, -actv.gr.eq.1yr)  %>% 
    ungroup()
}

# cut_leading_0s(); exclude 0s that exist in database before a species began nesting at a colony ----
cut_leading_0s <- function(hep) {
  # input df must be the outpout of clean_hep()
  # should not change number of columns, should cut number of rows
  hep_no_lead0 <- hep %>%
    filter(peakactvnsts > 0) %>% 
    group_by(code, species) %>% 
    summarise(first.year = min(year)) %>% 
    full_join(hep) %>% 
    mutate(b4.1st.year = ifelse(year < first.year, 1, 0)) %>% 
    filter(b4.1st.year == 0) %>% 
    select(-first.year, -b4.1st.year) %>% 
    ungroup()
}

# trim_hep_columns(); trim extra columns ----
trim_hep_columns <- function(hep, disturbance = TRUE, stage = TRUE, meta = TRUE) {
  if(disturbance == TRUE) {
    hep <- select(hep, -contains("dist"), -contains("roosting"), -contains("nesting"))
  }
    if(stage == TRUE) {
    hep <- select(hep, -contains("stage"), -contains("stge"), -contains("stg"))
    }
      if(meta == TRUE) {
    hep <- select(hep, -contains("notes"), -contains("source"), -contains("entry"), -contains("entered"), -contains("dataid"))
  }
}


# 3 calculation functions
# calc_mean_brood_size(); calculate mean brood size by year, site, species ----
calc_mean_brood_size <- function(hep) {
  # input df must be the outpout of clean_hep()
  # should not change number of rows, should cut number of columns
  # brd1-brd6 should be replaced by mean.brd.size and sd.brd.size
  brd_long <- hep  %>% 
  dplyr::select(year, code, species, brd1, brd2, brd3, brd4, brd5, brd6) %>% 
    gather(brdsz, num.nests, -year, -code, -species) %>% 
    filter(!(is.na(num.nests))) %>% 
    mutate(brdsz = gsub("brd", "", brdsz)) %>% 
    arrange(year, code, species, brdsz)
  # make long again so there's a row for each nest of each brood size
  brd_longer <- brd_long[rep(row.names(brd_long), brd_long$num.nests), 1:4]	%>% 
    mutate(brdsz = as.numeric(brdsz)) %>% 
    ungroup()
  
  mean_brd_size <- brd_longer %>% 
    group_by(year, code, species) %>% 
    summarise(mean.brd.size = mean(brdsz),
              sd.brd.size = sd(brdsz),
              brd.n.nests = n()) %>% 
    ungroup()
  new_hep <- hep %>% 
    select(-brd1, -brd2, -brd3, -brd4, -brd5, -brd6) %>% 
    full_join(mean_brd_size) %>% 
    ungroup()
}


# function to calculate mean and sd for colony-level productivity parameters: brood size, proportion of nests successfull, colony productivity
calc.repro=function(df) {
##-- first peel off brood size columns to calculate average brood size and se
brds <- select(df, CODE, YEAR, SPECIES, BRD1, BRD2, BRD3, BRD4, BRD5, BRD6)
brds.long=melt(brds, id.vars = c("CODE", "YEAR", "SPECIES"), variable_name="br.size")
brds.long$br.size <- as.numeric(substr(brds.long$br.size, 4, 4))
brds.longer <- brds.long[rep(row.names(brds.long), brds.long$value), 1:4]
brds.longer1=filter(brds.longer, br.size>0)
brds.prod <- brds.longer %>%
		group_by(CODE, YEAR, SPECIES) %>%
		summarize(mean.brs=mean(br.size),
				  se.brs=var(br.size)/sqrt(length(br.size)))
##-- then add mean and se brood size back to original df				  
df1=merge(df, brds.prod, by.x=c("CODE", "YEAR", "SPECIES"), by.y=c("CODE", "YEAR", "SPECIES"), all.x=T)
##-- and finally calculate proportion and se of focal nests that were successful, and colony productivity (average number of chicks per nest attempt)
df1$prop.successful=1-(df1$FOCFAILURE/df1$FOCALNESTS)
df1$se.successfull=sqrt((df1$prop.successful*(1-df1$prop.successful))/df1$FOCALNESTS)
df1$prod=df1$prop.successful*df1$mean.brs
return(df1)
}
#hep=calc.repro(hep)
	




# 4 plotting funtions ----
# hep_colony_size_plotter(); plot colony size by year, all species on same plot ----
hep_colony_size_plotter <- function(colony.code, save.plot = TRUE) {

  hep4plot <- hep %>% 
    filter(code == colony.code)
  
col.name <- dplyr::distinct(hep4plot, site.name)[[1]] %>% 
  droplevels()

year.lims = c(min(hep4plot$year), max(hep4plot$year))
year.breaks = seq(year.lims[1], year.lims[2], by = round((year.lims[2]-year.lims[1])/3, 0))

#-- 
zcolor <- hep4plot %>% 
  distinct(species, spp.color) %>% 
  droplevels() %>% 
  arrange(species)

plot.cols <- zcolor[[2]]

  zplot = ggplot(data = hep4plot) +
    geom_point(aes(x = year, y = peakactvnsts, color = spp.name)) +
    stat_smooth(aes(x = year, y = peakactvnsts, color = spp.name), se = F) +
    ggtitle(paste(col.name, " - Colony size", sep = "")) +
    theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    ylab("Peak colony size") +
    xlab("") +
    scale_x_continuous(limits = year.lims, breaks = year.breaks) + 
    scale_color_manual(values= plot.cols)
  if(save.plot == TRUE) {
    ggsave(paste("figures_output/HEP_by_colony_plots/", col.name, "_colonysize_", year.lims[1], "_", year.lims[2], ".jpg", sep = ""), width = 9, height = 6)
  }  else {
    zplot
  }
  
}
# hep_brood_size_plotter(); plot brood size by year, all species on same plot ----
hep_brood_size_plotter <- function(colony.code, save.plot = TRUE) {

  hep4plot <- hep %>% 
    filter(code == colony.code)
  
col.name <- dplyr::distinct(hep4plot, site.name)[[1]] %>% 
  droplevels()

year.lims = c(min(hep4plot$year), max(hep4plot$year))
year.breaks = seq(year.lims[1], year.lims[2], by = round((year.lims[2]-year.lims[1])/3, 0))

#-- 
zcolor <- hep4plot %>% 
  distinct(species, spp.color) %>% 
  droplevels() %>% 
  arrange(species)

plot.cols <- zcolor[[2]]

  zplot = ggplot(data = hep4plot) +
    geom_point(aes(x = year, y = mean.brd.size, color = spp.name)) +
    stat_smooth(aes(x = year, y = mean.brd.size, color = spp.name), se = F) +
    ggtitle(paste(col.name, " - Brood size", sep = "")) +
    theme(legend.title=element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
    ylab("Average brood size") +
    xlab("") +
    scale_x_continuous(limits = year.lims, breaks = year.breaks) + 
    scale_color_manual(values= plot.cols)
  if(save.plot == TRUE) { 
    ggsave(paste("figures_output/HEP_by_colony_plots/", col.name, "_broodsize_", year.lims[1], "_", year.lims[2], ".jpg", sep = ""), width = 9, height = 6)
  }  else {
    zplot
  }
}
