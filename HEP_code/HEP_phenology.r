# quicky phenology test


summary(lm(jun.time.index~YEAR, data=filter(hep, SPECIES=="GREG")))
confint(lm(jun.time.index~YEAR, data=filter(hep, SPECIES=="GREG")))


ggplot(data=filter(hep, SPECIES=="GREG"), aes(x=YEAR, y=jun.time.index), main="GREG")+
	geom_point()+ 
	ggtitle("GREG")+
	xlim(1990, 2015)+
	geom_smooth(method="lm") + 
	annotate("text", x = 1997, y = .8, label = "linear model YEAR coef.= -0.008 \n 95% CI = -0.009 to 0.0006 \n P= 0.06", size=5)+ 
	theme_bw()+
				theme(
					plot.background = element_blank()
					,panel.grid.minor = element_blank()
					,panel.grid.major = element_blank()
					)		
					