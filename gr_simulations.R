## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/19/12
## plotting model simulations for model 2
library(ggplot2)
library(tikzDevice)

# plot within country differences
SIMS$minmax <- SIMS$cox
SIMS$minmax[SIMS$scen=="cscen" & SIMS$minmax == 0.147492625 
                  | SIMS$scen=="cscen" & SIMS$minmax == 0.458715596] <- "Minimum"
SIMS$minmax[SIMS$scen=="cscen" & SIMS$minmax == 0.350877193
                  | SIMS$scen=="cscen" & SIMS$minmax == 0.497512438] <- "Maximum"

simplot1 <- ggplot(SIMS[SIMS$scen=="cscen",],aes(expval))
simplot1 <- simplot1 + geom_density(aes(fill = as.factor(minmax)),alpha=.5)                                    
simplot1 <- simplot1 + scale_fill_manual(name = "Cox \nThreshold"  
                                           ,label=c("Maximum","Minimum")
                                           ,values = c(hured,hublue))
simplot1 <- simplot1 + facet_grid(.~ country)+ theme_bw()
simplot1 <- simplot1 + xlab("Expected Producer Support \n(Normalized)") + ylab("Density") + opts(legend.position = "top")
simplot1 <- simplot1 + opts(panel.grid.major=theme_blank())
  
tikz("Graphics/sim1P.tikz",width=5.5,height=5.5,standAlone=T)
simplot1
dev.off()

SIMS$country <- as.character(SIMS$country)
SIMS$country[SIMS$country == "Cox Threshold"] <- "01\\textbf{Cox Threshold}"
SIMS$country <- as.factor(SIMS$country)

simplot2 <- ggplot(SIMS[SIMS$scen!="cscen",], aes(expval))
simplot2 <- simplot2 + geom_density(aes(fill = as.factor(scen)), alpha = .5)
simplot2 <- simplot2 + xlab("Expected Producer Support \n(Normalized)") + ylab("Density") + theme_bw()
simplot2 <- simplot2 + facet_grid(country~.)
simplot2 <- simplot2 + scale_fill_manual(name = "Scenario"
                                           , values = c(hured,hugreen,hublue))
simplot2 <- simplot2 + opts(legend.position = "top")
simplot2 <- simplot2 + opts(panel.grid.major=theme_blank())

tikz("Graphics/sim2P.tikz",width=5.5,height=5.5,standAlone=T)
simplot2
dev.off()