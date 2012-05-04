## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 03/17/12
## replicate correlation matrix p 327

IVs <- cbind(PJ$agshare,PJ$gdppercap,PJ$landlabor,PJ$ruralpop
             ,PJ$pers_rank,PJ$coxenep, PJ$polcon5)
cortab <- cor(IVs)
colnames(cortab) <- c("Agricultural Share","GDP","Land per Labor"
                      ,"Rural Population","Personal Vote","Cox Threshold"
                      ,"Political Constraints")
rownames(cortab) <- colnames(cortab)
cortab
xtable(cortab)