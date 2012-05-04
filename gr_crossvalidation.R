## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/19/12
## plots of cross-validation

# split in two plots
ESTS1 <- CROSDAT[CROSDAT$estid <= 10,]
ESTS2 <- CROSDAT[CROSDAT$estid > 10 & CROSDAT$estid!=12,]

# plot coefficient estimates
ccvalp1 <- ggplot(ESTS1,aes(subsmpl.excl,Mean,colour = subsmpl.excl))
ccvalp1 <- ccvalp1 + geom_pointrange(aes(ymin=Lower,ymax=Upper))
ccvalp1 <- ccvalp1 + geom_hline(aes(yintercept=0), linetype=2)
ccvalp1 <- ccvalp1 + xlab("") + ylab("") 
ccvalp1 <- ccvalp1 + scale_colour_manual(name="Subsample \nExcluded"
                                           ,values = colours)
ccvalp1 <- ccvalp1 + coord_flip() + theme_bw()
ccvalp1 <- ccvalp1 + facet_wrap(~estimate, scales="free")
ccvalp1 <- ccvalp1 + opts(axis.ticks = theme_blank(), axis.text.y = theme_blank()
                          ,axis.text.x = theme_blank(), panel.grid.minor = theme_blank()
                          ,panel.grid.major = theme_blank())


# export as tikz
tikz("Graphics/ccval1.tikz",width=7.5, height=4.77,standAlone=T)
ccvalp1
dev.off()

# plot variance components, deviance and sampled residuals
ccvalp2 <- ggplot(ESTS2,aes(subsmpl.excl,Mean,colour = subsmpl.excl))
ccvalp2 <- ccvalp2 + geom_pointrange(aes(ymin=Lower,ymax=Upper))
ccvalp2 <- ccvalp2 + xlab("") + ylab("") 
ccvalp2 <- ccvalp2 + scale_colour_manual(name="Subsample \nExcluded"
                                           ,values = colours)
ccvalp2 <- ccvalp2 + coord_flip() + theme_bw()
ccvalp2 <- ccvalp2 + facet_wrap(~estimate, scales="free")
ccvalp2 <- ccvalp2 + opts(axis.ticks = theme_blank(), axis.text.y = theme_blank()
                          , axis.text.x = theme_blank(), panel.grid.major=theme_blank()
                          , panel.grid.minor=theme_blank())

# export as tikz
tikz("Graphics/ccval2.tikz",width=7.5, height=4.77,standAlone=T)
ccvalp2
dev.off()

# plot all for poster
ESTS <- CROSDAT[CROSDAT$estid %in% c(11,12)==F,]
colours <- c("#8A0F14","#8A0F51","#860F8A","#480F8A","#0F138A"
          ,"#00376C","#00376B","#0077E6","#0057A8","#00572C")

# rename variables to get in order
ESTS$estimate[ESTS$estid==16] <- "11$\\sigma_{\\epsilon}$"
ESTS$estimate[ESTS$estid==15] <- "12$\\sigma_{country-year}$"
ESTS$estimate[ESTS$estid==14] <- "13$\\sigma_{country}$"
ESTS$estimate[ESTS$estid==13] <- "14$\\sigma_{commodity}$"
ESTS$estimate[ESTS$estid==10] <- "10Year"
ESTS$estimate[ESTS$estid==9] <- "09Political \nConstraints"
ESTS$estimate[ESTS$estid==8] <- "08\\textbf{Cox} \n\\textbf{Threshold}"
ESTS$estimate[ESTS$estid==7] <- "07Personal Vote"
ESTS$estimate[ESTS$estid==6] <- "06Rural \nPopulation"
ESTS$estimate[ESTS$estid==5] <- "05Land \nper Labor"
ESTS$estimate[ESTS$estid==4] <- "04GDP"
ESTS$estimate[ESTS$estid==3] <- "03Agricultural \nShare"
ESTS$estimate[ESTS$estid==2] <- "02Consumption"
ESTS$estimate[ESTS$estid==1] <- "01Production"

ccvalp <- ggplot(ESTS,aes(Mean,subsmpl.excl,colour = subsmpl.excl))
ccvalp <- ccvalp + geom_errorbarh(aes(xmin=Lower,xmax=Upper, height = 0))
ccvalp <- ccvalp + geom_point(aes(colour = subsmpl.excl))
ccvalp <- ccvalp + xlab("") + ylab("") + theme_bw()
ccvalp <- ccvalp + scale_colour_manual(name="Subsample \nExcluded"
                                       , values = colours)
ccvalp <- ccvalp + geom_vline(aes(xintercept = 0),linetype = 2)
ccvalp <- ccvalp + facet_wrap(~estimate, scales="free", ncol=10)
ccvalp <- ccvalp + opts(axis.ticks = theme_blank(), axis.text.y = theme_blank()
                          , axis.text.x = theme_blank(), panel.grid.major = theme_blank()
                        , legend.position="left")


# export as tikz
tikz("Graphics/ccvalP.tikz",width=11, height=3,standAlone=T)
ccvalp
dev.off()