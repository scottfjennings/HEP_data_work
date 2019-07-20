
####  several code snippets for doing various things with the HEP data



library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape)

options(scipen=999)




hep=read.csv("HEP/tbl_HEPDATA.csv")
hep$PEAKACTVNSTS[hep$PEAKACTVNSTS=="-999"]<-NA
hep$CODE=as.factor(hep$CODE)



## check which colonies have BCNH
bcnh <- hep %>% filter(SPECIES=="BCNH" & PEAKACTVNSTS>0) %>% distinct(CODE)


## some data cleaning
# replace empty fields for number of nests with each brood size with 0
hep[c("BRD1", "BRD2", "BRD3", "BRD4", "BRD5", "BRD6")][is.na(hep[c("BRD1", "BRD2", "BRD3", "BRD4", "BRD5", "BRD6")])] <- 0

# function to calculate the proportion of unguarded to total nests for the MAY and JUNE survey periods
#calc.time.index=function(df) {


hep=within(hep, {
	may.time.index=(MAYSTAGE4 + MAYSTAGE5)/(MAYSTAGE1 + MAYSTAGE2 + MAYSTAGE3 + MAYSTAGE4 + MAYSTAGE5)
	jun.time.index=(JUNSTAGE4 + JUNSTAGE5)/(JUNSTAGE1 + JUNSTAGE2 + JUNSTAGE3 + JUNSTAGE4 + JUNSTAGE5)
})


#return(df)
#}
#hep=calc.time.index(hep)

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
hep=calc.repro(hep)
		  
##
## function to separate CODE field into 2 parts for parent sites and satelite (child) sites
parse.CODE=function(df) {

	df$CODE=as.character(df$CODE)
	df2 <- df %>% separate(CODE, c("parent.code", "child.code"), remove = FALSE, sep = "\\.")
	df2$parent.code=as.factor(df2$parent.code)
	df2$child.code=as.character(df2$child.code)
	df2$child.code[is.na(df2$child.code)] <- 0
	df2$child.code=as.factor(df2$child.code)
return(df2)
}
hep=parse.CODE(hep)


# function to count the number of nests in each stage on each of the standard survey periods
# this is needed to account for variation in survey date within each survey period when evelauating phenology
tally.stages=function(df) {
#df=hep  
mar <- select(df, CODE, YEAR, SPECIES, MARSTGEDATE, MARSTAGE1, MARSTAGE2, MARSTAGE3, MARSTAGE4, MARSTAGE5) 
mar.long = gather(mar, key = stage, value = mar.num.nests, -CODE, -YEAR, -SPECIES, -MARSTGEDATE)
mar.long$stage <- as.numeric(substr(mar.long$stage, 9, 9))
mar.long=filter(mar.long, mar.num.nests>0) 

apr <- select(df, CODE, YEAR, SPECIES, APRSTGEDATE, APRSTAGE1, APRSTAGE2, APRSTAGE3, APRSTAGE4, APRSTAGE5) 
apr.long = gather(apr, key = stage, value = apr.num.nests, -CODE, -YEAR, -SPECIES, -APRSTGEDATE)
apr.long$stage <- as.numeric(substr(apr.long$stage, 9, 9))
apr.long=filter(apr.long, apr.num.nests>0) 

may <- select(df, CODE, YEAR, SPECIES, MAYSTGEDATE, MAYSTAGE1, MAYSTAGE2, MAYSTAGE3, MAYSTAGE4, MAYSTAGE5) 
may.long = gather(may, key = stage, value = may.num.nests, -CODE, -YEAR, -SPECIES, -MAYSTGEDATE)
may.long$stage <- as.numeric(substr(may.long$stage, 9, 9))
may.long=filter(may.long, may.num.nests>0) 

jun <- select(df, CODE, YEAR, SPECIES, JUNSTGEDATE, JUNSTAGE1, JUNSTAGE2, JUNSTAGE3, JUNSTAGE4, JUNSTAGE5) 
jun.long = gather(jun, key = stage, value = jun.num.nests, -CODE, -YEAR, -SPECIES, -JUNSTGEDATE)
jun.long$stage <- as.numeric(substr(jun.long$stage, 9, 9))
jun.long=filter(jun.long, jun.num.nests>0) 

ltjun <- select(df, CODE, YEAR, SPECIES, LTJUNSTGDATE, LTJUNSTAGE1, LTJUNSTAGE2, LTJUNSTAGE3, LTJUNSTAGE4, LTJUNSTAGE5) 
ltjun.long = gather(ltjun, key = stage, value = ltjun.num.nests, -CODE, -YEAR, -SPECIES, -LTJUNSTGDATE)
ltjun.long$stage <- as.numeric(substr(ltjun.long$stage, 9, 9))
ltjun.long=filter(ltjun.long, ltjun.num.nests>0) 

jul <- select(df, CODE, YEAR, SPECIES, JULSTGEDATE, JULSTAGE1, JULSTAGE2, JULSTAGE3, JULSTAGE4, JULSTAGE5) 
jul.long = gather(jul, key = stage, value = jul.num.nests, -CODE, -YEAR, -SPECIES, -JULSTGEDATE)
jul.long$stage <- as.numeric(substr(jul.long$stage, 9, 9))
jul.long=filter(jul.long, jul.num.nests>0) 

stages <- join_all(list(mar.long, apr.long, may.long, jun.long, ltjun.long, jul.long), by=c("CODE", "YEAR", "SPECIES", "stage"), type='full')

stages <- select(stages, CODE, YEAR, SPECIES, stage, everything())

return(stages)
}



# then load some functions for additional filtering
## filter one or multiple species
filt.spp=function(df, sp, multiple.spp) {
		if(missing(multiple.spp)) {
        x=filter(df, SPECIES==sp)
		} else {
        x=filter(df, SPECIES %in% sp)
    }

x=droplevels(x)
return(x)
}
# e.g for single species:
# gbhe=filt.spp(hep, "GBHE")
# e.g for multiple species:
#gbhe_greg=filt.spp(hep, c('GBHE', 'GREG'), y)

## filter to a study area sub region
filt.subreg=function(df, subreg) {
x=filter(df, SubRegion==subreg)
x=droplevels(x)
return(x)
}



sub.hep=select(hep, CODE, YEAR, SITE, SPECIES, PEAKACTVNSTS, FOCALNESTS, FOCFAILURE, MAYSTAGE4, JUNSTAGE4, 
				may.time.index, jun.time.index, prod) #using dplyr

#-=------------------


hep.sites=read.csv("HEP/tbl_HEPSites.csv")

sub.sites=select(hep.sites, Code, Site_Name, SubRegion, UTMNORTH, UTMEAST) #using dplyr

sub.sites=select(hep.sites, Code, Site_Name) #using dplyr

write.csv(sub.sites, "sub_sites.csv", row.names=F)
 
#summary(filter(sub.hep, YEAR == 2015))
#b<-aggregate(sub.hep$PEAKACTVNSTS,list(sub.hep$YEAR, sub.hep$SPECIES),sum)



hep.sites.data=merge(sub.hep, sub.sites, by.x="CODE", by.y="Code")




#-=------------------


gbhe=filt.spp(hep.sites.data, "GBHE")
gbhe_greg=filt.spp(hep.sites.data, c('GBHE', 'GREG'), y)


gbhe_greg.sus=filt.subreg(gbhe_greg, "SUS")

gbhe_greg.sus.prod=subset(gbhe_greg, select=c("YEAR","prod"))

gbhe_greg.sus.long=melt(gbhe_greg.sus.prod, id.vars = "YEAR", variable.name = 'repro.type', value.name = 'repro.val')



ggplot(data=gbhe_greg.sus.long, aes(x=YEAR, y=value, color=variable))+
		geom_point()+
		geom_smooth(method="lm")+
		ylab("")+
		theme_bw()+
				theme(plot.background = element_blank(),
					  panel.grid.minor = element_blank())	
 





#ggplot(data=hep.sites.dat1, aes(x=UTMEAST, y=UTMNORTH))+
#	geom_point(aes(size=PEAKACTVNSTS))+
#	facet_grid(~SPECIES)
	
	



ggplot(data=gbhe.sus2, aes(x=YEAR, y=prop.fail))+
		geom_point()



PropFailByYear=function(df) {		#calculate the proportion of all focal nests that failed for each year, averaged across all colonies
	x <- df %>% 
		group_by(YEAR) %>%	
	summarise(mean(prop.fail, na.rm = TRUE))
	colnames(x)[2]="mean.prop.fail"
return(x)
}

gbhe.sus3=PropFailByYear(gbhe.sus2)



## group data by year, parent site and species
grouped=group_by(hep.sites.data, YEAR, parent.code, SPECIES)

## calculate the total number of active nests for each parent site (summing across child sites)
a=summarise(grouped, sum.active=sum(PEAKACTVNSTS))

## group data by parent site only
grouped.b=group_by(hep.sites.dat2, parent.code)
# calculate mean UTMs to generate a single approximate location for all child sites
b=summarise(grouped.b, mean.utmn=mean(UTMNORTH, na.rm=TRUE), mean.utme=mean(UTMEAST, na.rm=TRUE))



c=left_join(a,b)


#c.gbhe=subset(c, SPECIES == "GBHE", select=c("YEAR", "sum.active"))

#gbhe.acf=acf(c.gbhe$sum.active, na.action = na.pass, plot=FALSE)
#all.acf=acf(c$sum.active, na.action = na.pass, plot=FALSE)



## join summed active nest data with the UTMs
c=c[!is.na(c$sum.active),]


ggplot(data=c, aes(x=YEAR, y=sum.active, color=parent.code))+
	geom_line()

## add a new column for percent change in size from previous year, and rename df to d
d=c %>% group_by(SPECIES, parent.code) %>% mutate(per.chng=(100*(sum.active/lag(sum.active)))-100)

## take care of issues associated with 0 active nests in certain years
d$per.chng=as.numeric(d$per.chng)
d$per.chng[d$per.chng=="Inf"]<-"100"
d$per.chng=as.numeric(d$per.chng)


e=filter(d, sum.active !=0)
e$per.chng=as.numeric(e$per.chng)
e.sub=e[!is.na(e$per.chng),]

filter(d, parent.code=="1", SPECIES=="GBHE")


## calculate mean, sd and cv of peak active nests
mean.size=summarise(d,
  mean.size = mean(sum.active, na.rm = TRUE))
sd.size=summarise(d,
  sd.size = sd(sum.active, na.rm = TRUE))

mean.sd=left_join(mean.size, sd.size)
mean.sd.coords=data.frame(left_join(mean.sd,b))
mean.sd.coords=mean.sd.coords[!is.na(mean.sd.coords$sd.size),]

mean.sd.coords.more0=filter(mean.sd.coords, mean.size>0)
mean.sd.coords.more0$cv.size=(mean.sd.coords.more0$sd.size/mean.sd.coords.more0$mean.size)


	
	ggplot(data=new.coastCA3.b, aes(x=V1, y=V2)) + 
	geom_path()+
	geom_point(data=mean.sd.coords.more0, aes(x=mean.utme, y=mean.utmn, size=cv.size, color=cv.size))+
	xlim(460000,  620000)+
	ylim(4175000, 4300000)+
	xlab("UTMEAST")+
	ylab("UTMNORTH")+
	ggtitle("variability (coefficient of variation) in colony size") +
	facet_grid(~SPECIES)+
	theme_bw()
	
	





filter(d, parent.code == 1, SPECIES == "GREG")
	


ggplot(data=filter(hep.sites.dat2, CODE == 1), aes(x=YEAR, y=PEAKACTVNSTS))+
	geom_point(size=0)+
	facet_grid(~SPECIES)+
	geom_line(aes(x=YEAR, y=PEAKACTVNSTS, color="blue"))+
	geom_line(aes(x=YEAR, y=abs.chng, color="red"))


pica.greg=filter(hep.sites.dat2, CODE == 1, SPECIES == "GREG")

pacf(pica.greg$PEAKACTVNSTS, pica.greg$YEAR, na.action = na.pass)

	m=gls(sum.active~YEAR, data=a,
	correlation = corAR1(form = ~ YEAR|site.spp))

	
B.lm<-lm(per.chng~YEAR*SPECIES, data=e)
summary(B.lm)

E<-data.frame(rstandard(B.lm))
