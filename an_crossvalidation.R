## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/01/12
## 5-fold cross-validation

# since coxenep, the main variable of interest, varies with country-years and not
# commodities, I perform cross-validation selecting entire country years instead
# of single observations. The problem is that due to differences in the number of
# commodities observed in each country year, sample sizes diverge.

# create subsamples
fold = 10
countryyears <- unique(PJ$conid)
samplesize <- length(countryyears)/fold
for(i in 1:fold){
  selection <- sample(countryyears,samplesize)
  PJ$subsample[PJ$conid %in% selection == T] <- LETTERS[i]
  countryyears <- countryyears[countryyears %in% selection == F]
}
table(PJ$subsample)


# run model 2 for each subsample excluded once
# function that runs model 2 on selected data set is specified in an_cross_mocel2
source("an_cross_model2.R")
CROSD <- c()
for(i in 1:fold){
  SEL <- PJ[PJ$subsample != LETTERS[i],]
  CDAT <- crosMod2(SEL)
  CDAT <- cbind(CDAT,LETTERS[i])
  CROSD <- rbind(CROSD,CDAT)
}
CROSDAT <- as.data.frame(CROSD)
head(CROSDAT)
CROSDAT$estimate <- rep(c("Production","Consumption"
                          ,"Agricultural \nShare"
                          ,"GDP","Land per \nLabor","Rural \nPopulation"
                          ,"Personal \nVote","Cox \nThreshold"
                          ,"Political \nConstraints","Year"
                          ,"Deviance","Residuals"
                          ,"$\\sigma_{commodity}$","$\\sigma_{country}$"
                          ,"$\\sigma_{country-year}$","$\\sigma_{\\epsilon}$"),fold)
CROSDAT$estid <- rep(c(1:16),fold)

rownames(CROSDAT) <- c(1:nrow(CROSDAT))
names(CROSDAT) <- c("Mean","SD","Lower","Upper","subsmpl.excl","estimate","estid")

CROSDAT$Mean <- as.numeric(as.character(CROSDAT$Mean))
CROSDAT$SD <- as.numeric(as.character(CROSDAT$SD))
CROSDAT$Upper <- as.numeric(as.character(CROSDAT$Upper))
CROSDAT$Lower <- as.numeric(as.character(CROSDAT$Lower))