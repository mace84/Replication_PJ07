## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/15/12
## coefficient plots for main models

library(ggplot2)
library(tikzDevice)
# define colors of HU corporate design
source("/Users/mo/Documents/RFunctions/HUDesignColors.R")

# get results from model 1
model1.mcmc <- as.mcmc(m1)
res1 <- summary(model1.mcmc,quantiles=c(.025,.975))
means <- res1$statistics[-1,1]
int95 <- res1$quantiles[-1,]
res12 <- summary(model1.mcmc,quantiles=c(.05,.95))
int90 <- res12$quantiles[-1,]
estdat <- cbind(means,int90,int95,"Unconditional")

# get results from model 2
model2.mcmc <- as.mcmc(m2)
res2 <- summary(model2.mcmc,quantiles=c(.025,.975))
means <- res2$statistics[-c(11,12),1]
int95 <- res2$quantiles[-c(11,12),]
res22 <- summary(model2.mcmc,quantiles=c(.05,.95))
int90 <- res22$quantiles[-c(11,12),]
estdat2 <- cbind(means,int90,int95,"Conditional")
estdat2 <- rbind(estdat2[1:10,],rep(NA,ncol(estdat2)), estdat2[-c(1:10),])
estdat <- rbind(estdat2,estdat)
names <- EST$var

# make data frame
EST <- as.data.frame(estdat)
names(EST) <- c("Mean","lower90","upper90","lower95","upper95","Model")
rownames(EST) <- c(1:nrow(EST))
EST$var <- c("19Production","18Consumption","17Agricultural Share","16GDP"
             , "15Land per Labor","14Rural Population","13Personal Vote"
             , "12\\textbf{Cox Threshold}","11Political Constraints", "10Year"
             #, "09Variance Components"
             , "02$\\sigma_{commodity}$", "04$\\sigma_{country}$"
             , "06$\\sigma_{country-year}$","08$\\sigma_{\\epsilon}$"
             , "01", "03"
             , "05","07"
             )
EST$level <- c(rep("01Base \nLevel",2),rep("02Country-Year Level",8)
               ,rep("03Variance Components",8))
EST$Mean <- as.numeric(as.character(EST$Mean))
EST$lower90 <- as.numeric(as.character(EST$lower90))
EST$upper90 <- as.numeric(as.character(EST$upper90))
EST$lower95 <- as.numeric(as.character(EST$lower95))
EST$upper95 <- as.numeric(as.character(EST$upper95))

# make coeficient plot
coefp <- ggplot(EST,aes(Mean,var))
#coefp <- coefp + geom_point(colour=NA)
#coefp <- coefp + geom_vline(aes(xintercept=-2),size=.2,colour="grey90")
#coefp <- coefp + geom_vline(aes(xintercept=c(-1.5,1.5)),size=.2,colour="grey90")
#coefp <- coefp + geom_vline(aes(xintercept=c(-1,1)),size=.2,colour="grey90")
#coefp <- coefp + geom_vline(aes(xintercept=c(-.5,.5)),size=.2,colour="grey90")
coefp <- coefp + geom_vline(aes(xintercept=0), linetype=2)
coefp <- coefp + geom_errorbarh(aes(xmin=lower95,xmax=upper95, colour = Model)
                                , alpha = .7, height = 0)
coefp <- coefp + geom_errorbarh(aes(xmin=lower90,xmax=upper90, colour = Model)
                                , size = 1.3, height = 0, alpha = .5)
coefp <- coefp + geom_point(aes(color=Model),size = 2.5)
coefp <- coefp + scale_colour_manual(values = c(hublue,hured))
coefp <- coefp + xlab("Estimate") + ylab("")+ theme_bw()
coefp <- coefp + facet_grid(level~., scales="free",space="free")
coefp <- coefp + opts(legend.position = "top", axis.ticks = theme_blank()
                      ,panel.grid.major=theme_blank())
coefp <- coefp + scale_x_continuous(breaks=c(-1,0,1),labels=c(-1,0,1))



# export as tikz (numbers in labes have to be manually removed)
tikz("Graphics/coefplotP.tikz", width=5.5,height=5.5,standAlone=T)
coefp
dev.off()