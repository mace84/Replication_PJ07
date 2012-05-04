## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
## model simulations for model 2

# make 10000 draws from distribution of estimates:
draws <- matrix(rep(NA,10000*14),ncol=14)

# exclude residual.mix and deviance from estimates matrix
cols <- c(1:10,13:16)
for(i in cols){
  j <- which(cols==i)
  draws[,j] <- sample(unlist(model2.mcmc[,i]),10000)
}


# Scenarios
SIMS <- c()
# USA with low cox threshold (local minimum). Everything else at local mean.
USlow <- c(mean(PJ$production[PJ$countryname == "UnitedStates"])
            ,mean(PJ$consumption[PJ$countryname == "UnitedStates"])
            ,mean(PJ$agshr[PJ$countryname == "UnitedStates"])
            ,mean(PJ$gdppercap[PJ$countryname == "UnitedStates"])
            ,mean(PJ$landperlabor[PJ$countryname == "UnitedStates"])
            ,mean(PJ$ruralpop[PJ$countryname == "UnitedStates"])
            ,mean(PJ$personal[PJ$countryname == "UnitedStates"])
            ,min(PJ$coxenep[PJ$countryname == "UnitedStates"])
            ,mean(PJ$polcon5[PJ$countryname == "UnitedStates"]),1
            ,1,1,1,1) # error terms

sim1 <- draws%*%USlow
SIMS <- rbind(SIMS,cbind(sim1,"United States"
                ,min(PJ$coxenep[PJ$countryname == "UnitedStates"])
                ,mean(PJ$personal[PJ$countryname == "UnitedStates"])
                ,"cscen"))

# USA with high cox threshold (local maximum). Everything else at local mean.
UShigh <- c(mean(PJ$production[PJ$countryname == "UnitedStates"])
               ,mean(PJ$consumption[PJ$countryname == "UnitedStates"])
               ,mean(PJ$agshr[PJ$countryname == "UnitedStates"])
               ,mean(PJ$gdppercap[PJ$countryname == "UnitedStates"])
               ,mean(PJ$landperlabor[PJ$countryname == "UnitedStates"])
               ,mean(PJ$ruralpop[PJ$countryname == "UnitedStates"])
               ,mean(PJ$personal[PJ$countryname == "UnitedStates"])
               ,max(PJ$coxenep[PJ$countryname == "UnitedStates"])
               ,mean(PJ$polcon5[PJ$countryname == "UnitedStates"]),1
               ,1,1,1,1) # error terms
sim2 <- draws%*%UShigh
SIMS <- rbind(SIMS,cbind(sim2,"United States"
                         ,max(PJ$coxenep[PJ$countryname == "UnitedStates"])
                         ,mean(PJ$personal[PJ$countryname == "UnitedStates"])
                         , "cscen"))


# Turkey with low cox threshold (local maximum). Everything else at local mean.
TURlow <- c(mean(PJ$production[PJ$countryname == "Turkey"])
            ,mean(PJ$consumption[PJ$countryname == "Turkey"])
            ,mean(PJ$agshr[PJ$countryname == "Turkey"])
            ,mean(PJ$gdppercap[PJ$countryname == "Turkey"])
            ,mean(PJ$landperlabor[PJ$countryname == "Turkey"])
            ,mean(PJ$ruralpop[PJ$countryname == "Turkey"])
            ,mean(PJ$personal[PJ$countryname == "Turkey"])
            ,min(PJ$coxenep[PJ$countryname == "Turkey"])
            ,mean(PJ$polcon5[PJ$countryname == "Turkey"]),1
            ,1,1,1,1) # error terms

sim3 <- draws%*%TURlow
SIMS <- rbind(SIMS,cbind(sim3,"Turkey"
                         ,min(PJ$coxenep[PJ$countryname == "Turkey"])
                         ,mean(PJ$personal[PJ$countryname == "Turkey"])
                         ,"cscen"))


# Turkey with high cox threshold (local maximum). Everything else at local mean.
TURhigh <- c(mean(PJ$production[PJ$countryname == "Turkey"])
            ,mean(PJ$consumption[PJ$countryname == "Turkey"])
            ,mean(PJ$agshr[PJ$countryname == "Turkey"])
            ,mean(PJ$gdppercap[PJ$countryname == "Turkey"])
            ,mean(PJ$landperlabor[PJ$countryname == "Turkey"])
            ,mean(PJ$ruralpop[PJ$countryname == "Turkey"])
            ,mean(PJ$personal[PJ$countryname == "Turkey"])
            ,max(PJ$coxenep[PJ$countryname == "Turkey"])
            ,mean(PJ$polcon5[PJ$countryname == "Turkey"]),1
            ,1,1,1,1) # error terms

sim4 <- draws%*%TURhigh
SIMS <- rbind(SIMS,cbind(sim4,"Turkey"
                         ,max(PJ$coxenep[PJ$countryname == "Turkey"])
                         ,mean(PJ$personal[PJ$countryname == "Turkey"])
                         ,"cscen"))


# Variance with electoral system (personal vote = 1,6,11)
# Everything at global mean.
PVoteMin <- c(mean(PJ$production),mean(PJ$consumption)
              ,mean(PJ$agshr),mean(PJ$gdppercap),mean(PJ$landperlabor),mean(PJ$ruralpop)
             ,-1.60678074
             ,mean(PJ$coxenep),mean(PJ$polcon5),1
             ,1,1,1,1) # error terms
sim5 <- draws%*%PVoteMin
SIMS <- rbind(SIMS,cbind(sim5,"Personal Vote",mean(PJ$coxenep),-1.60678074,"Minimum"))

PVoteMed <- c(mean(PJ$production),mean(PJ$consumption)
              ,mean(PJ$agshr),mean(PJ$gdppercap),mean(PJ$landperlabor),mean(PJ$ruralpop)
              ,-0.268431379
              ,mean(PJ$coxenep),mean(PJ$polcon5),1
              ,1,1,1,1) # error terms
sim6 <- draws%*%PVoteMed
SIMS <- rbind(SIMS,cbind(sim6,"Personal Vote",mean(PJ$coxenep),-0.268431379,"Median"))

PVoteMax <- c(mean(PJ$production),mean(PJ$consumption)
              ,mean(PJ$agshr),mean(PJ$gdppercap),mean(PJ$landperlabor),mean(PJ$ruralpop)
              ,1.069917981
              ,mean(PJ$coxenep),mean(PJ$polcon5),1
              ,1,1,1,1) # error terms
sim7 <- draws%*%PVoteMax
SIMS <- rbind(SIMS,cbind(sim7,"Personal Vote",mean(PJ$coxenep),1.069917981,"Maximum"))


#  check maximum impact of Cox threshold. Everything else at its mean
CoxMin <- c(mean(PJ$production),mean(PJ$consumption)
              ,mean(PJ$agshr),mean(PJ$gdppercap),mean(PJ$landperlabor),mean(PJ$ruralpop)
              ,-0.268431379
              ,min(PJ$coxenep),mean(PJ$polcon5),1
              ,1,1,1,1) # error terms
sim8 <- draws%*%CoxMin
SIMS <- rbind(SIMS,cbind(sim8,"Cox Threshold",min(PJ$coxenep),-0.268431379,"Minimum"))

CoxMed <- c(mean(PJ$production),mean(PJ$consumption)
            ,mean(PJ$agshr),mean(PJ$gdppercap),mean(PJ$landperlabor),mean(PJ$ruralpop)
            ,-0.268431379
            ,quantile(PJ$coxenep,.5),mean(PJ$polcon5),1
            ,1,1,1,1) # error terms
sim9 <- draws%*%CoxMed
SIMS <- rbind(SIMS,cbind(sim9,"Cox Threshold",quantile(PJ$coxenep,.5),-0.268431379,"Median"))

CoxMax <- c(mean(PJ$production),mean(PJ$consumption)
            ,mean(PJ$agshr),mean(PJ$gdppercap),mean(PJ$landperlabor),mean(PJ$ruralpop)
            ,-0.268431379
            ,max(PJ$coxenep),mean(PJ$polcon5),1
            ,1,1,1,1) # error terms
sim10 <- draws%*%CoxMax
SIMS <- rbind(SIMS,cbind(sim10,"Cox Threshold",max(PJ$coxenep),-0.268431379,"Maximum"))

# create data frame
SIMS <- as.data.frame(SIMS)
names(SIMS) <- c("expval","country","cox","pvote","scen")
SIMS$expval <- as.numeric(as.character(SIMS$expval))
SIMS$cox <- as.numeric(as.character(SIMS$cox))
SIMS$pvote <- as.numeric(as.character(SIMS$pvote))