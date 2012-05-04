## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
## Figure 1 p.315

# create country-year mean PSE over commodities
for(i in 1:length(PJ$country)){
  years <- unique(PJ$year[PJ$country == i])
  for(j in 1:length(years)){
    PJ$cyMeanPSE[PJ$country == i & PJ$year == years[j]] <- 
      mean(PJ$PSE[PJ$country == i & PJ$year == years[j]])
    coms <- unique(PJ$commodity[PJ$country == i & PJ$year == years[j]])
    for(k in 1:length(coms)){
      PJ$comcount[PJ$country == i & PJ$year == years[j] & PJ$commodity == coms[k]] <- k
    }
  }
}

# country codes vector for plot
PJcy <- PJ[PJ$comcount == 1,c(1,2,4,32)]
PJcy$cyMeanPSE <- PJcy$cyMeanPSE/100
ccodes <- c("AUS","CAN","ICE","JPN","KOR","MEX","NZL","NOR","SWZ","TUR","USA")
for(i in 1:11){
  PJcy$ccode[PJcy$country == i]  <- ccodes[i]
}

# create tikz file with plot
tikz("Graphics/figure1.tikz", width = 5.5, height = 3.5, standAlone =T)

plot(PJcy$rawyear[PJcy$country == 11],PJcy$cyMeanPSE[PJcy$country == 11],
     ylim=c(0,.8),xlim=c(1986,2000),pch=1,
     xlab="Time", ylab ="Producer Support Estimate")
text(1987,PJcy$cyMeanPSE[PJcy$country == 11 & PJcy$rawyear == 1987]
     ,PJcy$ccode[PJcy$country == 11 & PJcy$rawyear == 1987], cex =.8, pos=3)
lines(PJcy$rawyear[PJcy$country == 11],PJcy$cyMeanPSE[PJcy$country == 11],lty=11)
p <- c(3,1,3,1,1,3,3,1,1,3)
for(i in 1:10){
  points(PJcy$rawyear[PJcy$country == i],PJcy$cyMeanPSE[PJcy$country == i],pch=i)
  text(1987,PJcy$cyMeanPSE[PJcy$country == i & PJcy$rawyear == 1987]
       ,unique(PJcy$ccode[PJcy$country == i]), cex =.8, pos=p[i])
  lines(PJcy$rawyear[PJcy$country == i],PJcy$cyMeanPSE[PJcy$country == i],lty=i)
}
text(1997,PJcy$cyMeanPSE[PJcy$country == 9 & PJcy$rawyear == 1997]
     ,unique(PJcy$ccode[PJcy$country == 9]), cex =.8, pos=3)

dev.off()

# make a nicer graph for the poster
PJcy$ccode  <- as.character(PJcy$ccode)
PJcy$countryname  <- as.character(PJcy$countryname)
PJcy$countryname[PJcy$ccode == "NZL"] <- "New Zealand"
PJcy$countryname[PJcy$ccode == "USA"] <- "United States"

timeline <- ggplot(PJcy,aes(rawyear,cyMeanPSE,colour=as.factor(countryname)))
timeline <- timeline + geom_line(aes(linetype=as.factor(countryname))) + theme_bw()
timeline <- timeline + xlab("Year") + ylab("Producer Support Estimate")
timeline <- timeline + scale_colour_discrete(name = "Country") 
            + scale_linetype_discrete(name = "Country")
timeline <- timeline + opts(panel.grid.minor=theme_blank()
                            ,panel.grid.major=theme_blank())

tikz("Graphics/timeline.tikz",width=5.5,height=3.5,standAlone=T)
timeline
dev.off()