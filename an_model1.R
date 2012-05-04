## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
## model 1 tabel 1 p322: unconditional model

# Note:
# You need to install jags 3.2.0 from http://mcmc-jags.sourceforge.net/ 
# to be able to execute this script.

# identifiers
countryyear <- PJ$conid
country <- PJ$country
commodity <- PJ$commodity

# level 1 variable
y  <- PJ$y

# counts
n <- length(y)
J <- length(unique(countryyear))
K <- length(unique(country))
L <- length(unique(commodity))

# include a constant term for error estimation
con <- rep(1,n)

# use jags to estimate model
m1.data <- list("y","n","con","countryyear","country","commodity","J","K","L")

m1.inits <- function(){
  list(tau.y=runif(1,1,10),tau.cy=runif(1,1,10),tau.ctr=runif(1,1,10)
       ,tau.com=runif(1,1,10))
}

m1.parameters <- c("sigma.y","sigma.cy","sigma.ctr","sigma.com")
m1 <- jags(m1.data,m1.inits,m1.parameters,model.file="m1.jags",
           n.chains=2,n.iter=110000, n.burnin = 10000,n.thin=1
           , jags.seed=14071984)

# get results
model1.mcmc <- as.mcmc(m1)
res <- summary(model1.mcmc,quantiles=c(.05,.95))
# get mean and sd
res1 <- res$statistics[-1,1:2]
# get 95% credibility intervals
res2 <- res$quantile[-1,]
results1 <- cbind(res1,res2)
# sort parameters as is paper
results1 <- rbind(results1[2,],results1[3,],results1[1,],results1[4,])
rownames(results1) <- c("sigma.ctr","sigma.cy","sigma.com","sigma.y")
xtable(results1, digits = 4)

