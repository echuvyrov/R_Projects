# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################
rm(list=ls())

S <- function(z,gamma){
	return(sign(z)*max(c(abs(z)-gamma,0.0)))
}

gamma = 1
z <- seq(from=-5, to=5, by=0.5)
for(i in 1:length(z)){
	zout[i] <- S(z[i],gamma)
}
plot(z,zout)


setwd("/home/mike-bowles/Documents/StatisticsPapers/ESL/DataSets/SoAfricanHeartData")

data <- read.table(file="data", sep = ",",header = TRUE,row.names = 1)
famHist <- rep(0.0,nrow(data))
I <- which(data[,5]=="Present")
famHist[I] <- 1.0
data[,5] <- famHist
data <- scale(data)
x <- data[,1:9]
y <- data[,10]

Beta <- rep(0.0,ncol(x))
BetaNew <- Beta
uncBeta <- Beta
xrj <- Beta
r <- rep(0.0,length(y))
lambda <- 1000
alpha <- 1
small <- 1e-6

mult <- 0.99
icount <- 0
repeat{
	icount <- icount + 1
	r <- y - x%*%Beta
	for(i in 1:length(Beta)){
		xrj[i] <- sum(r*x[,i])/length(y)
	}
	
	for(j in 1:ncol(x)){
		uncBeta[j] <- xrj[j] + Beta[j]
	}
	
	for(j in 1:ncol(x)){
		z <- uncBeta[j]
		BetaNew[j] <- S(z,lambda*alpha) /(1+lambda*(1-alpha))
	}
	print(icount)
	delta <- sum(abs(Beta-BetaNew))
	print(delta)
	if(delta < small){break}
	Beta <- BetaNew	
}
Beta
lambda <- lambda*mult
lambda


########################
#Logistic Regression

rm(list=ls())

S <- function(z,gamma){
	return(sign(z)*max(c(abs(z)-gamma,0.0)))
}

setwd("/home/mike-bowles/Documents/StatisticsPapers/ESL/DataSets/SoAfricanHeartData")

data <- read.table(file="data", sep = ",",header = TRUE,row.names = 1)
famHist <- rep(0.0,nrow(data))
I <- which(data[,5]=="Present")
famHist[I] <- 1.0
data[,5] <- famHist
data <- scale(data)
x <- data[,1:9]
y <- data[,10]

Beta <- rep(0.0,ncol(x))
Beta0 <- 0.0
BetaNew <- Beta
Beta0New <- 0.0
uncBeta <- Beta
z <- rep(0.0,length(y))
p <- rep(0.5,length(y))
w <- rep(0.0,length(y))
r <- rep(0.0,length(y))
alpha <- 1
small <- 1e-6


lambda <- 1000
alpha <- 1
mult <- 0.99
#set up lambda values, constants, etc.

#update quadratic approximation
for(i in 1:length(y)){
	temp <- Beta0 + x[i,]%*%Beta
	p[i] <- 1.0/(1+exp(-temp))
	w[i] <- p[i]*(1-p[i])
	z[i] <- temp + (y[i] - p[i])/w[i]	
}

#coordinate descent
BetaTemp <- Beta
Beta0Temp <- Beta0
icount <- 0
repeat{
	icount <- icount +1
	for(i in 1:length(y)){
		r[i] <- z[i] - x[i,]%*%BetaTemp
	}
	sumWiRi <- sum(w*r)
	Beta0New <- sumWiRi/sum(w*z)
	
	for(j in 1:length(Beta)){
		r <- z - x%*%BetaTemp + x[,j]*BetaTemp[j] - Beta0Temp
		uncBeta[j] <- sum(w*x[,j]*r)/length(y)
		wxxN <- sum(w*x[,j]*x[,j])/length(y)
		BetaNew[j] <- S(uncBeta[j],lambda*alpha)/(wxxN+lambda*(1-alpha))
	}
	print(icount)
	test <- sum(abs(BetaTemp-BetaNew)) + abs(Beta0Temp - Beta0New)
	print(test)
	BetaTemp <- BetaNew	
	Beta0Temp <- Beta0New
	if(test < small) {break}
	
}
test <- sum(abs(Beta-BetaTemp)) + abs(Beta0 - Beta0Temp)
print(test)
Beta <- BetaTemp
Beta0 <- Beta0Temp


lambda <- mult*lambda