#help(package="glmnet")

require("lars")
require("glmnet")
require("MASS")
require("lattice")

#read and initialize the data
rm(list=ls())
setwd("C:\\Projects\\R")

data <- read.table(file="soa_heart.data", sep = ",",header = TRUE,row.names = 1)
famhist <- rep(0.0, nrow(data))

# re-code Family History as 0 or 1 (in the input file it is coded as "Present" or "Absent")
Itemp <- which(data[,5]=="Present")
famhist[Itemp] <- 1
dataMat <- as.matrix(data[,1:4])
dataMat <- cbind(dataMat, famhist)
dataMat <- cbind(dataMat, data[,6:9])

# set input matrix x
x <- as.matrix(dataMat)
# set response matrix y
y<- as.matrix(data[,10])

#1. Linear Regression - Lasso
lassoObject <- lars(x, y, type = "lasso", trace = TRUE)
#lassoObject <- lars(x2,y, type = "lasso", trace = TRUE, max.steps = 4)
plot(lassoObject, xvar="step")
summary(lassoObject)

#2. LDA
splom(x, groups=y)

ldaObject <- lda(chd ~ ., data=data)
ldaObject
plot(ldaObject, dimen=1, type="both", cex=1.2)
predict(ldaObject, x)$class

#4. iteratively-reweighted least squares

#5. Logistic Regression (glmnet)
cvobj=cv.glmnet(x,y,family="binomial", type.measure="class")
plot(cvobj)
title("glmnet logistic regression SA Heart Data ",line=2.5)
min(cvobj$cvm)
coef(cvobj)

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
	
	
	
