
rm(list=ls())
require(glmnet)
require(sparseLDA)

#function for adding NAs indicators to dataframe and replacing NA's with a value---"cols" is vector of columns to operate on
#   (necessary for randomForest package)
changeNAs <- function(dataset) {
  dataset[is.na(dataset)] = 0
  return(dataset)
}

addOneToCats <- function(dataset) {
  dataset[!is.na(dataset)] = dataset[!is.na(dataset)] + 1
  return(dataset)
}

#preliminaries - get the data
setwd("C:\\Projects\\R")
training = read.csv("TrainingDataset.csv", na.strings="NaN")
test = read.csv("TestDataset.csv", na.strings="NaN")

#put predictors into X
#Xtrain <- as.matrix(training[,13:ncol(training)])
Xtrain <- as.matrix(training[,14:29])
Xtest <- as.matrix(test)
#put response into Y (first month's sales only)
Ytrain <- as.matrix(training[,1])

#demean X and bring to unit variance
for(i in 1:16){
  m <- sum(Xtrain[,i])
  m <- m/length(Xtrain[,i])
  Xtrain[,i] <- Xtrain[,i]-m
  v <- var(Xtrain[,i])
  Xtrain[,i] <- Xtrain[,i]/sqrt(v)
}

#in the parameter space, add 1 to all categorical variables
XCats <- addOneToCats(Xtrain[, 15:ncol(Xtrain)])
XQuants <- Xtrain[, 1:14]
Xtrain <- cbind(XQuants, XCats)
# then, replace NaN with 0s
Xtrain <- changeNAs(Xtrain)

#use glmnet library with lasso penalty
linearCVObject = cv.glmnet(Xtrain,Ytrain, alpha = 1)
plot(linearCVObject)
coef(linearCVObject)
min(linearCVObject$cvm)
which.min(linearCVObject$cvm)

Ytest <- predict(linearCVObject,newx=Xtest[, 2:ncol(Xtest)])

#Sample submission-- column means
submission_colMeans = data.frame(id = test[,1])
for (var in names(training)[1:12]) {
  submission_colMeans[,var] = mean(training[,var], na.rm=TRUE)
}
write.csv(submission_colMeans, "sample_submission_using_training_column_means.csv", row.names=FALSE)


#randomForest benchmark submission:
library(randomForest)


#replacements:
training <- appendNAs(training,13:ncol(training))
test <- appendNAs(test,2:ncol(test))

#begin building submission data frame:
submission_rf = data.frame(id = test$id)

#train a random forest (and make predictions with it) for each prediction column
for (var in names(training)[1:12]) {
  print(var)
  rf = randomForest(training[,13:ncol(training)],training[,var], do.trace=TRUE,importance=TRUE, sampsize = 100, ntree = 500)
  submission_rf[,var] = predict(rf, test[,2:ncol(test)])
}

write.csv(submission_rf, "RandomForestBenchmark.csv", row.names=FALSE)
