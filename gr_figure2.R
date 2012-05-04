## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
## Figure 2 p.319

library(tikzDevice)

# create country means of PSE and Cox threshold over country-years
for(i in 1:length(PJ$country)){
  PJ$cMeanPSE[PJ$country == i] <- mean(PJ$PSE[PJ$country == i])
  PJ$cMeanCox[PJ$country == i] <- mean(PJ$coxenep[PJ$country == i])
}

PJmeans <- PJ[PJ$year==12 & PJ$comcount==1,c(1,34:35)]

# create tikz file with plot
tikz("Graphics/figure2.tikz", width = 5.5, height = 5.5, standAlone =T)

plot(PJmeans$cMeanCox,PJmeans$cMeanPSE, xlim = c(0.15,.5),
     ylim =c(0,.8), pch=16, ylab="Producer Support Estimate",
     xlab="Cox Threshold")
PJmeans$countryname <- as.character(PJmeans$countryname)
PJmeans$countryname[7] <- "New Zealand"
PJmeans$countryname[11] <- "US"
text(PJmeans$cMeanCox,PJmeans$cMeanPSE,PJmeans$countryname, cex =.6,
     pos = c(2,2,4,2,1,2,2,2,4,2,2))
dev.off()

# create nice figure for poster
PJmeans$countryname[11] <- "United States"
PJmeans$cMeanPSE <- PJmeans$cMeanPSE/10
PJmeans$pos.y <- PJmeans$cMeanPSE
PJmeans$pos.x <- PJmeans$cMeanCox
# adjust positions
PJmeans$pos.x[PJmeans$countryname=="Australia"] <- 
  PJmeans$pos.x[PJmeans$countryname=="Australia"] - 0.025

PJmeans$pos.x[PJmeans$countryname=="United States"] <- 
  PJmeans$pos.x[PJmeans$countryname=="United States"] - 0.025

PJmeans$pos.x[PJmeans$countryname=="Japan"] <- 
  PJmeans$pos.x[PJmeans$countryname=="Japan"] - 0.01

PJmeans$pos.x[PJmeans$countryname=="Norway"] <- 
  PJmeans$pos.x[PJmeans$countryname=="Norway"] - 0.005

PJmeans$pos.y[PJmeans$countryname=="Norway"] <- 
  PJmeans$pos.y[PJmeans$countryname=="Norway"] - 0.001



psecox <- ggplot(PJmeans,aes(cMeanCox,cMeanPSE))
psecox <- psecox + geom_point(color=NA) + theme_bw()
psecox <- psecox + opts(panel.grid.major=theme_blank(),panel.grid.minor=theme_blank())
psecox <- psecox + geom_smooth(method="lm",color=hublue,fil=hugrayblue
                               , fullrange=T)
psecox <- psecox + geom_text(aes(x=pos.x,y=pos.y,label=countryname)
                             ,size=4,vjust=-1,hjust=0.2)
psecox <- psecox + geom_point() #+ ylim(c(-.2,.85))
psecox <- psecox + ylab("Producer Support Estimate \n(Average 1986 - 2000)")
psecox <- psecox + xlab("Cox Threshold \n(Average 1986 - 2000)") 

tikz("Graphics/psecoxP.tikz",width=5.5,height=5.5,standAlone=T)
psecox
dev.off()