## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
##  cross-validation of model 2: conditional model

# Note:
# You need to install jags 3.2.0 from http://mcmc-jags.sourceforge.net/ 
# to be able to execute this script. 

# function that performs analysis of conditional model and returns:

crosMod2 <- function(DATA){
  # split data into levels of analysis
  # country year commodity data
  L1 <- data.frame(DATA$countryname,DATA$rawyear,DATA$year,DATA$country,DATA$conid
                   ,DATA$comid,DATA$commodity,DATA$y,DATA$PSE,DATA$production,DATA$consumption)
  names(L1) <-  c("countryname","rawyear","year","country","conid"
                  ,"comid","commodity","y","PSE","production","consumption")
  L1 <- L1[order(L1$commodity,L1$country,L1$year,L1$conid),]
  
  # country year data
  L2 <- data.frame(DATA$countryname,DATA$rawyear,DATA$agshr,DATA$gdppercap,DATA$landperlabor
                   ,DATA$ruralpop,DATA$personal,DATA$coxenep,DATA$polcon5,DATA$commodity)
  names(L2) <-  c("countryname","rawyear","agshr","gdppercap","landperlabor"
                  ,"ruralpop","personal","coxenep","polcon5","commodity")
  L2 <- L2[L2$commodity == 1, -10] 

  # identifiers
  ctryrs <- unique(L1$conid)
  countryyear <- c()
  for(i in 1:nrow(L1)){
    countryyear <- c(countryyear,which(ctryrs == L1$conid[i]))
  }
  
  country <- L1$country
  commodity <- L1$commodity
  
  # level one variables
  y <- L1$y
  prod <- L1$production
  cons <- L1$consumption
  year <- L1$year
  
  # level two variables
  agsh <- L2$agshr
  gdp <- L2$gdppercap
  labland <- L2$landperlabor
  rurpop <- L2$ruralpop
  pvote <- L2$personal
  cox <- L2$coxenep
  pcons <- L2$polcon5
  
  # counts
  n <- nrow(L1)
  J <- nrow(L2)
  K <- length(unique(country))
  L <- length(unique(commodity))
  con <- rep(1,n)
  
  m2.data <- list("y","n","con","countryyear","country","commodity","J","K","L"
                  ,"prod","cons"
                  ,"agsh","gdp","labland","rurpop"
                  ,"pvote","cox","pcons","year")
  
  m2.inits <- function(){
    list(tau.y=runif(1,1,10),tau.cy=runif(1,1,10),tau.ctr=runif(1,1,10)
         ,tau.com=runif(1,1,10)
         ,mu.a=rnorm(2),tau.a=runif(2,1,10),mu.b=rnorm(8),tau.b=runif(8,1,10))
  }
  
  m2.parameters <- c("sigma.y","sigma.cy","sigma.ctr","sigma.com"
                     ,"a","b", "residual.mix")
  
  m2 <- jags(m2.data,m2.inits,m2.parameters,,model.file="m2.jags",
             n.chains=2,n.iter=110000,jags.seed = 14071984
             ,n.burnin=10000, n.thin = 1)
  
  # get results
  model2.mcmc <- as.mcmc(m2)
  res <- summary(model2.mcmc,quantiles=c(.05,.95))
  
  # get mean and sd, including etimates of deviance and residuals
  res1 <- cbind(res$statistics[,1:2],res$quantiles[,1:2])
  return(res1)
}
