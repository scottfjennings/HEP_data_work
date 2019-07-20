

## takes objects from HEP_code.r



library(gstat)
library(maps)
library(maptools)
library(sp)
library(rgdal)
#mydata<-data.frame(E, c$mean.utmn, c$mean.utme)
mydata<-data.frame(E, e.sub)

sp::coordinates(mean.sd.coords.greg)<- c("mean.utme", "mean.utmn")

sp::bubble(mean.sd.coords.greg,"sd.size",col=c("black","grey"),
       main="GREG colony size SD",xlab="X-coordinates",
       ylab="Y-coordinates")
	   

sp::coordinates(mydata)<- c("mean.utme", "mean.utmn")
sp::bubble(mydata,"rstandard.B.lm.",col=c("black","grey"),
       main="Residuals",xlab="X-coordinates",
       ylab="Y-coordinates")
	   
	Vario1 = variogram(rstandard.B.lm. ~ 1, mydata)
plot(Vario1, main="All Spp")
 
mydata.gbhe=subset(mydata, SPECIES=="GBHE")
sp::bubble(mydata.gbhe,"rstandard.B.lm.",col=c("black","grey"),
       main="Residuals-GBHE",xlab="X-coordinates",
       ylab="Y-coordinates")	     
Vario.gbhe = variogram(rstandard.B.lm. ~ 1, mydata.gbhe)
plot(Vario.gbhe, main="gbhe")
  	   
mydata.greg=subset(mydata, SPECIES=="GREG")	     
sp::bubble(mydata.greg,"rstandard.B.lm.",col=c("black","grey"),
       main="Residuals-GREG",xlab="X-coordinates",
       ylab="Y-coordinates")	
Vario.greg = variogram(rstandard.B.lm. ~ 1, mydata.greg)
plot(Vario.greg, main="greg")
  	   	   
	   
z=expand.grid(
			YEAR=mydata$YEAR,
			SPECIES=mydata$SPECIES,
			parent.code=unique(mydata$parent.code)
			)	   
	   
x=data.frame(predict(B.lm, z))  
y=data.frame(z, x)

ggplot(data=mydata, aes(x=mean.utme, y=mean.utmn))+
	geom_point(size=E, color=E)+
	facet_grid(~SPECIES)+ 
	scale_fill_gradient2(midpoint = 0)
	
	
	+
	geom_line(aes(x=YEAR, y=PEAKACTVNSTS, color="blue"))+
	geom_line(aes(x=YEAR, y=abs.chng, color="red"))

	
	
	d <- qplot(mean.utme, mean.utmn, data=mydata, colour=E, size=abs(E))
	
	d + scale_colour_gradient2()+
	facet_grid(~SPECIES
