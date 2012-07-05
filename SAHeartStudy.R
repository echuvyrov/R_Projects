# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################
rm(list=ls())
require(splines)
require(glmnet)
setwd("C:\\Projects\\R")

data <- read.table(file="data", sep = ",",header = TRUE,row.names = 1)
col <- rep(0.0, nrow(data))
Itemp <- which(data[,5]=="Present")
col[Itemp] <- 1
dataMat <- as.matrix(data[,1:4])
dataMat <- cbind(dataMat, col)
dataMat <- cbind(dataMat, data[,6:9])
x <- as.matrix(dataMat)

y<- as.matrix(data[,10])

cvobj=cv.glmnet(x,y,family="binomial", type.measure="class")
plot(cvobj)
title("glmnet logistic regression SA Heart Data ",line=2.5)
min(cvobj$cvm)
coef(cvobj)


#try without adiposity and typea
iColOut <- c(4,6)
xo <- x[,-iColOut]

cvobjo=cv.glmnet(xo,y,family="binomial", type.measure="class")
plot(cvobjo)
title("glmnet logistic regression SA Heart Data wo adiposity and typea",line=2.5)
min(cvobjo$cvm)
coef(cvobjo)


#form 4-df spline-expansion for numerical variables.
temp <- x[,5]
xBs <- temp
for(i in 1:4){
	temp <- x[,i]
	xBs <- cbind(xBs,bs(temp,df=4))
}
for(i in 6:9){
	temp <- x[,i]
	xBs <- cbind(xBs,bs(temp,df=4))
}

cvobjBs=cv.glmnet(xBs,y,family="binomial", type.measure="class")
plot(cvobjBs)
title("glmnet logistic regression SA Heart Data w 4df spline expansion",line=2.5)
min(cvobjBs$cvm)


coef(cvobjBs)

#Let's see what the resulting curves look like for the original
#attributes

iattr <- 1
xS <- rep(0.0,nrow(xBs))
for(i in 1:4){
	index <- (iattr - 1)*4 + 1 + i
	vect <- xBs[,index]
	xS <- xS + vect*coef(cvobjBs)[index+1]	
}

vect <- x[,iattr]
plot(vect,xS)

iattr <- 2
xS <- rep(0.0,nrow(xBs))
for(i in 1:4){
	index <- (iattr - 1)*4 + 1 + i
	vect <- xBs[,index]
	xS <- xS + vect*coef(cvobjBs)[index+1]	
}

vect <- x[,iattr]
plot(vect,xS)


iattr <- 3
xS <- rep(0.0,nrow(xBs))
for(i in 1:4){
	index <- (iattr - 1)*4 + 1 + i
	vect <- xBs[,index]
	xS <- xS + vect*coef(cvobjBs)[index+1]	
}

vect <- x[,iattr]
plot(vect,xS)

	
iattr <- 4
xS <- rep(0.0,nrow(xBs))
for(i in 1:4){
	index <- (iattr - 1)*4 + 1 + i
	vect <- xBs[,index]
	xS <- xS + vect*coef(cvobjBs)[index+1]	
}

vect <- x[,iattr]
plot(vect,xS)
	
	
	
