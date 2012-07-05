setwd("/home/mike-bowles/Downloads/campaignData/")
data <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
str(data)
require(gbm)


X <- data[,13:30]
#Y - labels a single column from data
Y <- as.numeric(data[,1])
Y <- log(Y)
Y[is.na(Y)] <- 0.0

#Y - labels as sum of all month sales
for(i in 1:nrow(data)){
  Y[i] <- log(sum(data[i,1:12],na.rm=TRUE))
}

#Y - montly fraction of total (labels sum to one)
Y <- data[,1]/exp(Y)





nc1 <- ncol(X)
d1 <- dimnames(X)[[2]]

idxCat <- c(1,18)
for(i in 1:length(idxCat)) {
  v <- as.factor(X[,idxCat[i]])
  X[,idxCat[i]] <- v
}
for(i in 2:17){
  v <- is.nan(X[,i])
  if(sum(v)>0){
    
    meanx <- mean(X[!v,i])
    X[v,i] <- meanx
    X <- cbind(X,as.factor(v))
  }
}

newCols <- paste("V",1:(ncol(X)-nc1),sep="")
dimnames(X)[[2]] <- c(d1,newCols)




gdata <- cbind(Y,X)
ntrees <- 4000
depth <- 5
minObs <- 10
shrink <- 0.001

folds <- 10
mo1gbm <- gbm(Y~. ,data=gdata,
              distribution = "gaussian",
              n.trees = ntrees,
              shrinkage = shrink,
              cv.folds = folds)


gbm.perf(mo1gbm,method="cv")


sqrt(min(mo1gbm$cv.error))
which.min(mo1gbm$cv.error)


