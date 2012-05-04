## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
##  OLS models in table 2

## OLS fixed-effects regressions

# null model
ols0 <- lm(y ~ as.factor(country) + as.factor(commodity) + as.factor(year), data=PJ)
summary(ols0)
plot(ols0)

# controll variables only
ols1 <- lm(y ~ production + consumption + agshr + gdppercap  +  landperlabor +  ruralpop +
  as.factor(country) + as.factor(commodity) + as.factor(year), data=PJ)
summary(ols1)
plot(ols1)

ols2 <- lm(y ~ production + consumption + agshr + gdppercap  +  landperlabor +  ruralpop +
  personal + coxenep + polcon5
  + as.factor(country) + as.factor(commodity) + as.factor(year), data=PJ);
summary(ols2)
plot(ols2)

ols3 <- lm(y ~ production + consumption + agshr + gdppercap  +  landperlabor +  ruralpop +
  personal + coxenep + polcon5 +  president + major + federal +
  as.factor(country) + as.factor(commodity) + as.factor(year), data=PJ);
summary(ols3)
plot(ols3)

## Regression without the Cairns group
nocairns <-  subset(PJ, PJ$country!=1 & PJ$country!=2 & PJ$country!=8 
                    & PJ$country!=11)
ols4 <- lm(y ~ production + consumption + agshr + gdppercap  +  landperlabor +  ruralpop +
  personal + coxenep + polcon5 +  president + major + federal +
  as.factor(country) + as.factor(commodity) + as.factor(year), data=nocairns);
summary(ols4)
plot(ols4)

dim(nocairns)
res0 <- rbind(summary(ols0)$coef[1,1:2],matrix(rep(NA,2*12),ncol=2))
res1 <- rbind(summary(ols1)$coef[1:7,1:2],matrix(rep(NA,2*6),ncol=2))
res2 <- rbind(summary(ols2)$coef[1:11,1:2],matrix(rep(NA,2*2),ncol=2))
res34 <- cbind(summary(ols3)$coef[1:13,1:2],summary(ols4)$coef[1:13,1:2])
results <- cbind(res0,res1,res2,res34)
rownames(results) <- rownames(res34)
xtable(summary(ols2),digit=4)