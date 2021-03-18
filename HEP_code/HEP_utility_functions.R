
# a basic starting workflow for HEP data looks like this

library(RODBC)


#hep <- hep_from_access() %>% 
#  clean_hep() %>% 
#  add_site_names() %>% 
#  cut_never_nested() %>% 
#  cut_leading_0s() %>% 
#  trim_hep_columns()


# default location, can be overwritten later
#hepdata_location = "S:/Databases/HEP_data/HEPDATA.accdb"
# hepdata_location = "C:/Users/scott.jennings/Documents/Projects/HEP/HEP_data_work/HEP_data/HEPDATA.accdb"
# data reading ----
hep_from_access <- function(hepdata_location = hepdata_location){
  if(is.na(hepdata_location)){
    print("Please assign location of HEPDATA access file to hepdata_location")
  } else {
    db <- hepdata_location
  }
 
 con2 <- odbcConnectAccess2007(db)

out_table <- sqlFetch(con2, "tbl_HEPDATA") 
 
close(con2)

return(out_table)
}

hep_sites_from_access <- function(hepdata_location = hepdata_location){
  if(is.na(hepdata_location)){
    print("Please assign location of HEPDATA access file to hepdata_location")
  } else {
    db <- hepdata_location
  }
 
 con2 <- odbcConnectAccess2007(db)

out_table <- sqlFetch(con2, "tbl_HEPSITES") 
 
close(con2)

names(out_table) <- tolower(names(out_table))
names(out_table) <- gsub("_", ".", names(out_table))
out_table <- out_table  # %>% 
  #mutate(code = as.character(code),
  #       parent.code = as.character(parent.code),
  #       site.name = as.character(site.name),
  #       parent.site.name = as.character(parent.site.name))
return(out_table)
}

# hep_start <- hep_from_access(hepdata_location)

# hep_sites <- hep_sites_from_access(hepdata_location)

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
hep_sites <- hep_sites_from_access(hepdata_location)
names(hep_sites) <- tolower(names(hep_sites))
names(hep_sites) <- gsub("_", ".", names(hep_sites))
hep <- left_join(hep, dplyr::select(hep_sites, code, parent.code, site.name, parent.site.name, county, subregion)) %>% 
  mutate(code = as.character(code),
         parent.code = as.character(parent.code),
         parent.site.name = as.character(parent.site.name),
         parent.site.name = ifelse(parent.site.name == "" & parent.code == 160, 'Cypress Road', parent.site.name)) %>% 
  #dplyr::select(code, parent.code, site.name, parent.site.name, everything(), -site)  %>% 
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
  if(disturbance) {
    hep <- select(hep, -contains("dist"), -contains("roosting"), -contains("nesting"))
  }
    if(stage) {
    hep <- select(hep, -contains("stage"), -contains("stg"))
    }
      if(meta) {
    hep <- select(hep, -contains("notes"), -contains("source"), -contains("entry"), -contains("entered"), -contains("dataid"))
  }
}


# 3 data summary functions ----
hep_by_parent_site <- function(hep) {
  hep_par_site <- hep %>% 
    group_by(parent.code, year, species) %>% 
    summarise(peakactvnsts = sum(peakactvnsts),
              focalnests = sum(focalnests),
              focfailure = sum(focfailure),
              brd1 = sum(brd1),
              brd2 = sum(brd2),
              brd3 = sum(brd3),
              brd4 = sum(brd4),
              brd5 = sum(brd5),
              brd6 = sum(brd6)) %>% 
    ungroup() %>% 
    mutate(parent.code = as.numeric(parent.code)) %>% 
    add_site_names()
  
  
}


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


# calc_mean_brood_size(); calculate mean brood size by species, lumping any year and colonies in hep ----
calc_mean_brood_size_lump_yr_col <- function(hep) {
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
    group_by(species) %>% 
    summarise(mean.brd.size = mean(brdsz),
              sd.brd.size = sd(brdsz),
              brd.n.nests = n()) %>% 
    ungroup()
  
  new_hep <- hep %>% 
    select(-brd1, -brd2, -brd3, -brd4, -brd5, -brd6) %>% 
    full_join(mean_brd_size) %>% 
    ungroup()
}

# annual percent change in colony size ----
hep_annual_changer <- function(hep) {
hep_changes <- hep %>% 
  group_by(parent.site.name, species, year, subregion) %>% 
  summarise(peakactvnsts = sum(peakactvnsts)) %>% 
  ungroup() %>% 
  group_by(parent.site.name, species) %>% 
  summarise(peakactvnsts = sum(peakactvnsts)) %>% 
  dplyr::arrange(parent.site.name, species, year) %>%
  mutate(consec.yrs = year - lag(year) == 1,
         prev.yr.nsts = ifelse(consec.yrs == T, lag(peakactvnsts), NA),
         abs.change.1year = peakactvnsts - prev.yr.nsts,
         per.change.1year = ((peakactvnsts - prev.yr.nsts)/prev.yr.nsts)*100,
         zero2zero = ifelse(consec.yrs, peakactvnsts == 0 & prev.yr.nsts == 0, NA),
         zero2some = ifelse(consec.yrs, prev.yr.nsts == 0 & peakactvnsts > 0, NA)) %>% 
  #mutate(per.change.1year = ifelse(per.change.1year == Inf, (100 * abs.change.1year), per.change.1year))%>% 
  #left_join(., spp.name) %>% 
  #left_join(., hep.subreg.key) %>% 
  ungroup()  
}

# function to calculate mean and sd for colony-level productivity parameters: brood size, proportion of nests successfull, colony productivity
calc_repro=function(df) {
##-- first peel off brood size columns to calculate average brood size and se
brds <- df %>% 
  select(code, year, species, brd1, brd2, brd3, brd4, brd5, brd6) %>% 
  pivot_longer(cols = contains("brd"), names_to = "br.size", values_to = "num.nests") %>% 
  mutate(br.size = gsub("brd", "", br.size)) %>% 
  filter(!is.na(br.size), !is.na(num.nests), num.nests != 0)

brds.expanded <- brds[rep(row.names(brds), brds$num.nests), ]



brds.prod <- brds.expanded %>%
  mutate(br.size = as.numeric(br.size)) %>% 
		group_by(code, year, species) %>%
		summarize(mean.brs=mean(br.size),
				  se.brs=var(br.size)/sqrt(length(br.size)))
##-- then add mean and se brood size back to original df				  
df1 <- full_join(df, brds.prod, by = c("code", "year", "species"))
##-- and finally calculate proportion and se of focal nests that were successful, and colony productivity (average number of chicks per nest attempt)


df1 <- df1 %>% 
  mutate(prop.successful = 1-(focfailure/focalnests),
         se.successfull = sqrt((prop.successful*(1-prop.successful))/focalnests),
         prod = prop.successful*mean.brs)
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

year.lims = c(floor(min(hep4plot$year)/5)*5, ceiling(max(hep4plot$year)/5)*5)
year.breaks = seq(year.lims[1], year.lims[2], by = 5)
#year.breaks = seq(year.lims[1], year.lims[2], by = round((year.lims[2]-year.lims[1])/3, 0))

#-- 
zcolor <- hep4plot %>% 
  distinct(species, spp.color) %>% 
  droplevels() %>% 
  arrange(species)

plot.cols <- zcolor[[2]]

  zplot = ggplot(data = hep4plot) +
    geom_point(aes(x = year, y = peakactvnsts, color = spp.name)) +
    stat_smooth(aes(x = year, y = peakactvnsts, color = spp.name), se = F) +
    #ggtitle(paste(col.name, " - Colony size", sep = "")) +
    ggtitle(col.name) +
    theme_classic() +
    theme(legend.title=element_blank()) +
    ylab("Number of nesting pairs") +
    xlab("") +
    scale_x_continuous(limits = year.lims, breaks = year.breaks) + 
    scale_color_manual(values= plot.cols) +
    ylim(0, max(hep4plot$peakactvnsts)*1.1)
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
    ggtitle(col.name) +
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
