# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################

p <- function(Beta,X){
	temp <- exp(t(Beta)%*%X)
	return(temp/(1+temp))
}


setwd("/home/mike-bowles/Documents/StatisticsPapers/ESL/DataSets/SoAfricanHeartData")

data <- read.table(file="data", sep = ",",header = TRUE,row.names = 1)

#code "present/absent" as 1/0
colTemp <- rep(0.0,nrow(data))
for(i in 1:nrow(data)){
	if(data[i,5] == "Present"){
		colTemp[i] <- 1
	}
	else{
		colTemp[i] <- 0
	}
}
data[,5] <- colTemp

data <- as.matrix(data)
X <- data[,-ncol(data)]
Y <- data[,ncol(data)]
#append column of 1's to carry bias offset
colTemp <- rep(1.0,nrow(data))
X <- cbind(colTemp,X)
X <- as.matrix(X)

#initialize Beta
Beta <- as.array(rep(0.0, ncol(X)))

#set a step-size (fraction of Newton step)
eps <- 0.1
small <- 1E-5

#initialize vector of probabilities
pvect <- rep(0.0,nrow(X))
for(i in 1:nrow(data)){
	Xtemp <- X[i,]
	pvect[i] <- p(Beta,Xtemp)
}

W <- matrix(0.0,nrow(X),nrow(X))
count <- 0
maxcount <- 100

repeat{
	count <- count +1
	if(count > maxcount) {break}
	
	for(i in 1:nrow(X)){W[i,i] <- pvect[i]*(1-pvect[i])}
	
		
	XtWX <- t(X)%*%W%*%X
	XtY_p <- t(X)%*%(Y - pvect)
	
	sol <- solve(XtWX,XtY_p)
	sol[] <- eps*sol[]
	BetaOld <- Beta
	for(i in 1:ncol(X)){
		Beta[i] <- BetaOld[i] + sol[i,1]
	}
	step <- sum(abs(sol))
	print(step)
	for(i in 1:nrow(data)){
		Xtemp <- X[i,]
		pvect[i] <- p(Beta,Xtemp)
	}	
}

#let's look at the error rate
yhat <- round(pvect + 0.5)
1-sum(yhat==Y)/length(Y)

#calculate covariance of Betas
r <- Y - pvect
sum(r)
varResid <- var(r)

ndim <- nrow(XtWX)
XtWX_1 <- matrix(0.0,ndim,ndim)

for(i in 1:ndim){
	e <- matrix(0.0,10,1)
	e[i,1] <- 1.0
	XtWX_1[,i] <- solve(XtWX,e)	
}
cov_Beta <- varResid*XtWX_1%*%t(X)%*%X%*%XtWX_1
sdBeta <- rep(0.0,ndim)
for(i in 1:ndim){sdBeta[i] <- sqrt(cov_Beta[i,i])}

Beta/sdBeta


Beta
X[1,]
