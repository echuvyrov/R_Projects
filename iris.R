# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################
rm(list=ls())
require(glmnet)
iris
iris.matrix <- as.matrix(iris)

x <- as.matrix(iris[,1:4])
y <- as.factor(iris[,5])

cvob2=cv.glmnet(x,y,family="multinomial")
plot(cvob2)
title("iris multiclass w. glmnet",line=2.5)

predictObj <- predict(cvob2,x)

yh <- rep(0.0,length(y))
for(i in 1:length(y)){
	temp <- predictObj[i,,1]
	yh[i] <- which.max(temp) - 1
}

ytest <- rep(0.0,150)
ytest[51:100] <- 1
ytest[101:150] <- 2

1 - sum(ytest==yh)/length(y)



