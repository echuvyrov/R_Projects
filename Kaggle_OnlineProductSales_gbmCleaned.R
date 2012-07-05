#
#
# Use gradient boosted tree's
#
# Rev1 - Try to use a small subset of data 
#
#
#
# Rev2 - Try to use all data and train over the set
#
#
# Rev3 - predict the sum
#
# Rev4 - Use the time series to predict the output and % change.
#
install.packages("gbm")
require(gbm)
rm(list=ls())

setwd("C:\\Projects\\R")

### Clean and make right category
#
# If sparse, don't use the mean.   Set it to the majority sparcicity value.
cleanInputDataForGBM <- function(X) {
  names(X);
  for(i in 1:length(X)) {
    
    name = names(X)[i]
    print (name)
    col = X[,i]  
    
    index = which(is.na(col))
    
    if ( substr(name,1,3) == 'Cat'  ) {
      col[index] = "Unknown"
      X[,i] <- as.factor(col)
    }
    
    if ( substr(name,1,4) == 'Quan' ) {
      column_mean = mean(col, na.rm = TRUE)
      col[index] = column_mean
      X[,i] <- as.numeric(col)
    }
    
    if ( substr(name,1,4) == 'Date' ) {  	
      column_mean = mean(col, na.rm = TRUE)
      col[index] = column_mean
      X[,i] <- as.numeric(col)
    }
    
    result = is.factor(X[,i])
    print(result);
  }
  return (X)
}

idxCat <- c(13,29)
training <- read.table(file="TrainingDataset.csv",header=TRUE, sep=",")
Xtrain <- training[, idxCat[1] : idxCat[2] ]
XtrainClean = cleanInputDataForGBM(Xtrain)

## Create levelsets for the NA's that are factors.   If numeric then abort if there is an NA

## Now run Test Data set, clean and continue.
test <- read.table(file="TestDataset.csv",header=TRUE, sep=",")
Xtest <- test[,  2:(idxCat[2] - idxCat[1] + 2)  ]
XtestClean = cleanInputDataForGBM(Xtest)

## GBM Parameters
ntrees <- 6000
depth <- 5
minObs <- 10
shrink <- 0.001
folds <- 10
Ynames <-   c('id', names(training[,1:12]))

## Setup variables.
ntestrows = nrow(XtestClean)
Yhattest =  matrix(nrow = ntestrows , ncol = 13, dimnames = list (1:ntestrows,Ynames ) )
X = XtrainClean
nColsOutput = 12

for( i in 1:nColsOutput ) {
  
  Y <- as.numeric(training[,i])
  Y <- log(Y)  ## TBD how does this get reconciled?
  Y[is.na(Y)] <- 0.0	
  gdata <- cbind(Y,X)
    
  mo1gbm <- gbm(Y~. ,
                data=gdata,
                distribution = "gaussian",
                n.trees = ntrees,
                shrinkage = shrink,
                cv.folds = folds)
    
  gbm.perf(mo1gbm,method="cv")
  
  sqrt(min(mo1gbm$cv.error))
  which.min(mo1gbm$cv.error)
  
  Yhattest[,i+1] <- exp(predict.gbm(mo1gbm, newdata=XtestClean, n.trees = ntrees))
  
}

Yhattest[,1] = seq(1,ntestrows,1)
write.csv(Yhattest, "campaign_4_jag_gbm.csv", row.names=FALSE)

### Clean and make right category
#
# If sparse, don't use the mean.   Set it to the majority sparcicity value.






