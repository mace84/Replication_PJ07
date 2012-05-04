## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/01/12

## Replication of Park, J.H. & Jensen, N., 2007, 
## Electoral competition and agricultural support in OECD countries,
## American Journal of Political Science, 51(2), pp. 314-29.

## The data was provided by the authors of the original article and can be
## downloaded from http://dvn.iq.harvard.edu/dvn/dv/orlowskm

# Note:
# I run R version 2.14.1 (2011-12-22) on a x86_64-apple-darwin9.8.0
# As a GIBS sampler I use JAGS 3.2.0 from http://mcmc-jags.sourceforge.net/

# packages used:
library(R2jags)
library(coda)
library(xtable)
library(ggplot2)
library(tikzDevice)

# seed
set.seed(14071984)

# data
PJ <- read.table("/Users/mo/Documents/Uni/Dissertation/Empirie/Quanti/sources/Park_Jensen_2007/park_and_jensen_ajps_2007.txt",header=T)


###################################################
####       Get and overview of the dataset     ####
###################################################

summary(PJ)
# y is standardized PSE,
# personal is standardized pers_rank
# agshr is standardized agshare
# landperlabor is standardized landlabor
# other standardized variables:
# production, consumption, polcon5, gdppercap, landperlabor

# non standardized: agshare, coxenep, pers_rank, laborpart, landlabor, 
# landperperson, enep1, ruralpop,ruralpopgrowth

# binaries: president, parliament, major, pr, federal,

PJ <- PJ[order(PJ$country,PJ$conid,PJ$commodity),]
# conid is an identifier for country years
# id counts the comodities in countries starting with 1-11
# for each country year in australia, 13-21 for each country year in Canada,
# 22-27 in Iceland etc.

table(is.na(PJ))
dim(PJ)
# there are no missing values in the data set


###################################################
####               Figure 1  p.315             ####
###################################################

source("gr_figure1.R")

# Comments:
# there are no values for sitzerland previous to 1997


###################################################
####               Figure 2  p.319             ####
###################################################

source("gr_figure2.R")

# Comments:
# Exact replication

###################################################
####        Table 1: main models  p.322        ####
###################################################

# model 1
source("an_model1.R")

# model analysis
print(m1) # deviance = 2312.076, DIC = 2471.1
plot(m1) # Rhat is close to 1 for all parameters
traceplot(m1) # chains converge quickly. 

# model 2
source("an_model2.R")

# model analysis
print(m2) # deviance 2222.449, DIC 2373.3
traceplot(m2) # chains converge
plot(m2)

# coefficient plot for both models
source("gr_coefplots.R")

# comments:
# Could replicate the results of both models. Only minor differences of average
# estimates which are due to radom processes. I don't know which seed was used
# by the authors. I use 14071984.


###################################################
####        Table 2: ols models  p.325         ####
###################################################

source("an_ols.R")

# comments:
# results for model 3 differ.
# everything else is exactly the same.
# one outlier observation with low leverage (829).

###################################################
####             Simulations model 2           ####
###################################################

source("an_simulations.R")
source("gr_simulations.R")


###################################################
####          Cross-validation (5-fold)        ####
###################################################

source("an_crossvalidation")
source("gr_crossvalidation")

# comments:
# largest changes in estimates of cox threshold.
# However, all 90% credibility intervals overlap.


###################################################
####          Correlation matrix p.327         ####
###################################################

source("an_correlations.R")

# Comments:
# correlations for coxenep are not the same as reported in paper