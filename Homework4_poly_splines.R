############################################################################### 
# Author: Eugene Chuvyrov
###############################################################################
rm(list=ls())
require(glmnet)

#preliminaries - get the data
setwd("C:\\Projects\\R")
pdata <- read.table(file="wines.data", sep = ";", header = TRUE)

#put predictors into X
x <- as.matrix(pdata[,1:11])
#put response into Y
y <- as.matrix(pdata[,12])

#1a Linear Regression on Wine Data
#use glmnet library with lasso penalty
linearCVObject = cv.glmnet(x,y, alpha = 1)
plot(linearCVObject)
min(linearCVObject$cvm)
which.min(linearObject$cvm)

#coefficients plot
linearObject = glmnet(x,y, alpha = 1)
plot(linearObject)
title("glmnet regression with lasso penalty on Wines Data",line=2.5)

#1b Second order polynomial basis expansion
xPoly <- x
for(i in 1:ncol(x)){
  vect1 <- x[,i]
  for(j in i:ncol(x)){
    vect2 <- x[,j]
    xPoly <- cbind(xPoly,vect1*vect2)		
  }
}

#lasso regression using glmnet with cross-validation
polyCVObject = cv.glmnet(xPoly,y, alpha = 1)
plot(polyCVObject)
min(polyCVObject$cvm)

#ridge regression, manual validation
X <- xPoly
Y <- y

Err <- matrix(nrow = 31, ncol = 3)
I <- seq(1:nrow(X))

for(iLambda in seq(from = 0, to = 30)){
  exp <- (+2 -4*(iLambda/20))
  xlambda <- 10^exp
  
  testErr <- 0.0
  trainErr <- 0.0
  
  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == (ixval - 1))
    
    Xin <- as.matrix(X[-Iout,])
    Xout <- as.matrix(X[Iout,])
    Yin <- Y[-Iout]
    Yout <- Y[Iout]
    mod <- lm.ridge(Yin~Xin,lambda=xlambda)
    C <- mod$coef/mod$scales
    XM <- Xin
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    } 
    xTemp <- as.matrix(XM)
    A <- as.array(C)
    Yh <- xTemp%*%A + mod$ym
    dY <- Yin - Yh
    trainErr <- trainErr + sum(dY*dY)/(nrow(as.matrix(Yin))*10)	
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    } 
    xTemp <- as.matrix(XM)
    A <- as.array(C)
    Yh <- xTemp%*%A +mod$ym
    dY <- Yout - Yh
    testErr <- testErr + sum(dY*dY)/(nrow(as.matrix(Yout))*10)
  }
  Err[(iLambda+1),1] = sqrt(trainErr)
  Err[(iLambda+1),2] = sqrt(testErr)
  Err[(iLambda+1),3] = xlambda
}
plot(Err[,1], type='p', col='red', 
     main = 'Error vs Log(Lambda)',
     ylab='Error',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
lines(Err[,1], type='l', col='red')
points(Err[,2], pch=16, col='blue')
lines(Err[,2], type='l', col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
       pch = c(15, 16), lty = 1:2)
min(Err[,2])


#1c Splines expansion
degFree <- 4
vect <- x[,1]
XBs <- bs(vect,df=degFree)
for(i in 2:ncol(x)){
  vect <- x[,i]
  XBs <- cbind(XBs,bs(vect,df=degFree))
}

#lasso regression using glmnet with cross-validation
splinesCVObject = cv.glmnet(XBs,y, alpha = 1)
plot(splinesCVObject)
min(splinesCVObject$cvm)

#ridge regression, manual validation
X <- XBs
Y <- y

Err <- matrix(nrow = 31, ncol = 3)
I <- seq(1:nrow(X))

for(iLambda in seq(from = 0, to = 30)){
  exp <- (+3 -4*(iLambda/20))
  xlambda <- 10^exp
  
  testErr <- 0.0
  trainErr <- 0.0
  
  for(ixval in seq(from = 1, to = 10)){
    Iout <- which(I%%10 == (ixval - 1))
    
    Xin <- as.matrix(X[-Iout,])
    Xout <- as.matrix(X[Iout,])
    Yin <- Y[-Iout]
    Yout <- Y[Iout]
    mod <- lm.ridge(Yin~Xin,lambda=xlambda)
    C <- mod$coef/mod$scales
    XM <- Xin
    for(i in seq(from = 1, to = ncol(Xin))){
      XM[,i]<-Xin[,i]-mod$xm[i]
    } 
    xTemp <- as.matrix(XM)
    A <- as.array(C)
    Yh <- xTemp%*%A + mod$ym
    dY <- Yin - Yh
    trainErr <- trainErr + sum(dY*dY)/(nrow(as.matrix(Yin))*10)  
    XM <- Xout
    for(i in seq(from = 1, to = ncol(Xout))){
      XM[,i]<-Xout[,i]-mod$xm[i]
    } 
    xTemp <- as.matrix(XM)
    A <- as.array(C)
    Yh <- xTemp%*%A +mod$ym
    dY <- Yout - Yh
    testErr <- testErr + sum(dY*dY)/(nrow(as.matrix(Yout))*10)
  }
  Err[(iLambda+1),1] = sqrt(trainErr)
  Err[(iLambda+1),2] = sqrt(testErr)
  Err[(iLambda+1),3] = xlambda
}
plot(Err[,1], type='p', col='red', 
     main = 'Error vs Log(Lambda)',
     ylab='Error',
     xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
lines(Err[,1], type='l', col='red')
points(Err[,2], pch=16, col='blue')
lines(Err[,2], type='l', col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
       pch = c(15, 16), lty = 1:2)
min(Err[,2])

#2 glass data classification
#preliminaries - get the data
pdata <- read.table(file="glass.data", sep = ",", header = TRUE)

#put predictors into X
x <- as.matrix(pdata[,1:10])
#put response into Y
y <- as.matrix(pdata[,11])

#use glmnet logistic regression to classify data
lrObject=cv.glmnet(x,y,family="multinomial", type.measure="class")
summary(lrObject)

plot(lrObject)
title("glmnet logistic regression Glass Data ",line=2.5)
min(lrObject$cvm)
#coef(lrObject)

#polynomial basis expansion
xPoly <- x
for(i in 1:ncol(x)){
  vect1 <- x[,i]
  for(j in i:ncol(x)){
    vect2 <- x[,j]
    xPoly <- cbind(xPoly,vect1*vect2)  	
  }
}

#re-run glmnet
polyObject=cv.glmnet(x,y,family="multinomial", type.measure="class")
summary(polyObject)

plot(polyObject)
title("glmnet logistic regression Glass Data, polynomial basis expansion ",line=2.5)
min(polyObject$cvm)
#coef(lrObject)


#Splines basis expansion
degFree <- 4
vect <- x[,1]
XBs <- bs(vect,df=degFree)
for(i in 2:ncol(x)){
  vect <- x[,i]
  XBs <- cbind(XBs,bs(vect,df=degFree))
}

#re-run glmnet
splinesObject=cv.glmnet(x,y,family="multinomial", type.measure="class")
summary(splinesObject)

plot(splinesObject)
title("glmnet logistic regression Glass Data, splines basis expansion ",line=2.5)
min(splinesObject$cvm)
#coef(lrObject)

#test the prediction
#pred = predict(splinesObject, x, type= "class", s=0.01)