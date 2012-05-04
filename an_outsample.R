## Duke PS 303-MLE: Replication
## Matthias Orlowski
## 04/11/12
## out of sample predictions

# create training and testing sets
fold = 10
rows <- c(1:nrow(PJ))
samplesize <- floor(length(rows)/fold)
for(i in 1:fold){
  if(length(rows) > length(samplesize)){
    selection <- sample(rows,samplesize)
    rows <- rows[rows %in% selection == F]
    PJ$testset[selection] <- LETTERS[i]
  } else {
    remainder <- rep(c(1:fold),samplesize)[1:length(rows)]
    testsets <- unique(PJ$testset)
    for(j in length(remainder)){
      PJ$testset[rows[j]] <- testsets[remainder[j]]
    }
  }
}
table(PJ$testset)

source("an_cross_model2.R")

