

#takes imported and filtered HEP dataframe from HEP_code


library(xlsx)
library(dplyr)
library(reshape2)
library(ggplot2)
setwd("C:/Users/scott.jennings/Dropbox/ACR/") # ACR
#setwd("C:/Users/Scott/Dropbox/ACR/")	#personal

		
newer.hep= read.csv("projects/HEP/newer_hep.csv")

## can double check that the SITE you want is in this .csv for a given year
filter(newer.hep, YEAR=="2016") %>% distinct(SITE) %>% select(SITE)


## FILL IN COLONY AND YEAR SPECIFIC INFO HERE
year <- "2016"
site <- 		"SanPabloDamWN"   ## this is the HEP_screening file name
par.code <- "51"  ## this is the prefix for the HEP site CODE
## CAN GET BOTH OF THESE FROM C:/Users/scott.jennings/Dropbox/ACR/HEP_screening/sub_sites.csv


# subset to site - parent.code is easiest way to do this
hep.site.a <- droplevels(subset(newer.hep, parent.code==par.code))
hep.site <- droplevels(filter(hep.site.a, PEAKACTVNSTS>0))

# make a table showing which species were in the colony in each year
select(hep.site, YEAR, SPECIES, PEAKACTVNSTS) %>%  dcast(YEAR~SPECIES, value.var='PEAKACTVNSTS')

# make a table showing which sub sites were occupied in each year
select(hep.site, YEAR, SPECIES, child.code) %>%  dcast(YEAR~SPECIES, value.var='child.code')




# data management
hep.site$SPECIES=as.factor(hep.site$SPECIES)
hep.site[c("FOCALNESTS", "FOCFAILURE")][is.na(hep.site[c("FOCALNESTS", "FOCFAILURE")])] <- 0


# calculate overall survival rate and se for each species, lumping across all years prior to the year specificed in filter()
col.surv.prev.yrs= hep.site %>%
  filter(YEAR<year)%>%
  group_by(SPECIES) %>%
  summarize(
    FOCAL.sum=sum(FOCALNESTS),
    FAILURE.sum=sum(FOCFAILURE),
    colony.perc.successful=100*(1-(FAILURE.sum/FOCAL.sum)),
    colony.se.perc.successfull=(sqrt((colony.perc.successful*(100-colony.perc.successful))/FOCAL.sum)))


col.surv.cur.yr= hep.site %>%
  filter(YEAR==year) %>%
  group_by(SPECIES) %>%
  summarize(
    colony.perc.successful=100*(1-(FOCFAILURE/FOCALNESTS)),
    colony.se.perc.successfull=(sqrt((colony.perc.successful*(100-colony.perc.successful))/FOCALNESTS)))

col.surv.each.yr= hep.site %>%
  group_by(YEAR, SPECIES) %>%
  summarize(
    FOCFAILURE = sum(FOCFAILURE),
    FOCALNESTS = sum(FOCALNESTS),
    colony.perc.successful=100*(1-(FOCFAILURE/FOCALNESTS)),
    colony.se.perc.successfull=(sqrt((colony.perc.successful*(100-colony.perc.successful))/FOCALNESTS)))


# calculate overall brood size and se for each species, lumping across all years prior to the year specificed in filter()
colony.brds = select(hep.site, CODE, YEAR, SPECIES, BRD1, BRD2, BRD3, BRD4, BRD5, BRD6) 
colony.brds.long=melt(colony.brds, id.vars = c("CODE", "YEAR", "SPECIES"), variable_name="br.size")
colony.brds.long$br.size <- as.numeric(substr(colony.brds.long$br.size, 4, 4))	# extract just the number from old column names (e.g. BRD2 -> 2)
colony.brds.longer <- colony.brds.long[rep(row.names(colony.brds.long), colony.brds.long$value), 1:4]	# make long again so there's a row for each nest of each brood size
colony.brds.longer1=filter(colony.brds.longer, br.size>0)
colony.brds.prod.prev.yrs <- colony.brds.longer %>%
  filter(YEAR<year) %>%
  group_by(SPECIES) %>%
  summarize(mean.brs=mean(br.size),
            se.brs=var(br.size)/sqrt(length(br.size)))				



colony.brds.prod.cur.yr <- colony.brds.longer %>%
  filter(YEAR==year) %>%
  group_by(SPECIES) %>%
  summarize(mean.brs=mean(br.size),
            se.brs=var(br.size)/sqrt(length(br.size)))				

colony.brds.prod.each.yr <- colony.brds.longer %>%
  group_by(YEAR,SPECIES) %>%
  summarize(mean.brs=mean(br.size),
            se.brs=var(br.size)/sqrt(length(br.size)))	 

col.surv.prev.yrs
col.surv.cur.yr
col.surv.each.yr
colony.brds.prod.prev.yrs
colony.brds.prod.cur.yr
colony.brds.prod.each.yr




#summarize data	for a specified year or all years		  
summary(subset(hep.site, YEAR=="2016" & SPECIES=="GBHE"))				
summary(subset(hep.site, SPECIES=="GBHE"))				


# head() for dplyr dfs					
#print(tbl_df(a), n=40)	


##	if some years were not resolved to subsite (child.code= 999) 
no.999 <- sub.hep1 %>%
		group_by(YEAR, SPECIES, parent.code)%>%
		filter(child.code!=999)%>%
		summarise(
			tot.nests.no999 = as.numeric(sum(PEAKACTVNSTS)),
			foc.nests.no999 = as.numeric(sum(FOCALNESTS)),
			foc.fail.no999 = as.numeric(sum(FOCFAILURE)),
			stg4_brood.no999 = as.numeric(mean(stg4_brood)))


just.999 <- sub.hep1 %>%
		group_by(YEAR, SPECIES, parent.code)%>%
		filter(child.code==999)%>%
		summarise(
			tot.nests.just999 = as.numeric(sum(PEAKACTVNSTS)),
			foc.nests.just999 = as.numeric(sum(FOCALNESTS)),
			foc.fail.just999 = as.numeric(sum(FOCFAILURE)),
			stg4_brood.just999 = as.numeric(mean(stg4_brood)))
			
 c=merge(no.999, just.999, all=T) 


c=within(c, {
tot.nests = ifelse(!is.na(tot.nests.just999), tot.nests.just999, tot.nests.no999)
foc.nests = ifelse(!is.na(foc.nests.just999), foc.nests.just999, foc.nests.no999)
foc.fail  = ifelse(!is.na(foc.fail.just999), foc.fail.just999, foc.fail.no999)
stg4_brood  = ifelse(!is.na(stg4_brood.just999), stg4_brood.just999, stg4_brood.no999)

})

d=subset(c, select=c("YEAR", "SPECIES", "tot.nests", "foc.nests", "foc.fail", "stg4_brood"), parent.code==51 )
	

dat3=rbind(d, dat)

hep.site$surv=1-(hep.site$FOCFAILURE/hep.site$FOCALNESTS)


				
plot.hep.report=function(df, sp)	{	

sp.name <- ifelse(sp=="GBHE", "Great Blue Heron",
			ifelse(sp=="GREG", "Great Egret",
			ifelse(sp=="SNEG", "Snowy Egret",
			ifelse(sp=="BCNH", "Black-crowned Night-heron",
			ifelse(sp=="CAEG", "Cattle Egret",
			ifelse(sp=="DCCO", "Double-crested Cormorant", "multiple species"))))))		
				
ggplot(data=subset(df, SPECIES==sp), aes(x=YEAR, y=PEAKACTVNSTS))+
		#geom_point(size=5)+
		geom_bar(stat="identity", alpha=.5, width=.25)+
		ggtitle(sp.name)+   
		geom_hline(aes(yintercept=mean(PEAKACTVNSTS, na.rm=T)), color="blue", linetype="dashed", size=1.5)+
		ylab("Colony size")+
		xlab("")+
		scale_x_continuous(breaks=seq(min(df$YEAR), max(df$YEAR), 1))+ 
		theme_bw()+
				theme(plot.background = element_blank(),
					  panel.grid.minor = element_blank(),
					  panel.grid.major.x=element_blank(),
					  axis.text.x = element_text(angle = 45, hjust = 1))	
				
}				
				


plot.hep.report(df=hep.site,
				sp="GBHE")



		