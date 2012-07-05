# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################

require(MASS)
setwd("/home/mike-bowles/Documents/StatisticsPapers/ESL/DataSets/ZipCode")

 


# ordinary regression with vector targets 
# start with redundant target info

decode <- function(yTest,target){
	vec <- rep(0.0, nrow(target))
	for(i in 1:nrow(target)){
		dY <- abs(yTest - target[i,])
		vec[i] <- sum(dY)		
	}
	#print(vec)
	return(which.min(vec))
}
P <- function(x){
	return(exp(x-0.5)/(1+exp(x-0.5)))
}


data <- read.table(file="zip.train", header = FALSE)
iIndices <- list()
for(i in 1:4){
	Itemp <- which(data[,1] == i)
	iIndices[[i]] <- Itemp
}
iLength <- rep(0.0,4)
for(i in 1:4) {iLength[i] <- length(iIndices[[i]])}

I1_4 <- rep(0.0,sum(iLength))
sumlength = 0.0
for(i in 1:4){	
	for(j in 1:iLength[i]){	I1_4[j + sumlength] = iIndices[[i]][j] }
	sumlength <- sumlength + iLength[i]
}


data1_4 <- data[I1_4,]
data.train <- data1_4[,-1]

target <- matrix(0.0,4,7)
target[1,] <- c(1,0,0,0,1,1,1)
target[2,] <- c(0,1,0,0,1,0,0)
target[3,] <- c(0,0,1,0,0,1,0)
target[4,] <- c(0,0,0,1,0,0,1)

bigY <- NULL
for(i in 1:nrow(data1_4)){
	bigY <- rbind(bigY, target[data1_4[i,1],])
}



Err <- matrix(nrow = 21, ncol = 3)
iAll <- seq(1:nrow(data1_4))

for(iLambda in seq(from = 0, to = 20)){
	#
	exp <- (+3 -4*(iLambda/20))
	xlambda <- 10^exp
	
	testErr <- 0.0
	trainErr <- 0.0

	for(ixval in seq(from = 1, to = 5)){
		Iout <- which(iAll%%5 == ixval - 1)
		
		Xin <- data.train[-Iout,]
		Xout <- data.train[Iout,]
		bigYIn <- bigY[-Iout,]
		bigYOut <- bigY[Iout,]
		YhTrain <- NULL
		YhTest <- NULL
		for(icol in 1:ncol(target)){
			Yin <- bigYIn[,icol]
			Yout <- bigYOut[,icol]
			dataIn  <- cbind(Yin,Xin)
			mod <- lm.ridge(Yin~., data = dataIn, lambda = xlambda)
			C <- mod$coef/mod$scales
			XM <- Xin
			for(i in seq(from = 1, to = ncol(Xin))){
				XM[,i]<-Xin[,i]-mod$xm[i]
			} 
			xTemp <- as.matrix(XM)
			A <- as.array(C)
			Yh <- xTemp%*%A + mod$ym
			YhTrain <- cbind(YhTrain,Yh)
			
			XM <- Xout
			for(i in seq(from = 1, to = ncol(Xout))){
				XM[,i]<-Xout[,i]-mod$xm[i]
			} 
			xTemp <- as.matrix(XM)
			A <- as.array(C)
			Yh <- xTemp%*%A +mod$ym
			YhTest <- cbind(YhTest,Yh)
		}
		IyhTrain <- rep(0.0,nrow(YhTrain)) 
		IyTrain <- IyhTrain
		IyhTest <- rep(0.0,nrow(YhTest))
		IyTest <- IyhTest
		for(i in 1:nrow(YhTrain)){
			yTest <- P(YhTrain[i,])
			IyhTrain[i] <- decode(yTest,target)
			yTest <- bigYIn[i,]
			IyTrain[i] <- decode(yTest,target)
		}
		for(i in 1:nrow(YhTest)) {
			yTest <- P(YhTest[i,])
			IyhTest[i] <- decode(yTest,target)
			yTest <- bigYOut[i,]
			IyTest[i] <- decode(yTest,target)
		}
		
		trainErr <- trainErr + (1-sum(IyhTrain == IyTrain)/length(IyTrain))/5
		testErr <- testErr + (1-sum(IyhTest == IyTest)/length(IyTest))/5
	}

Err[(iLambda+1),1] = trainErr
Err[(iLambda+1),2] = testErr
Err[(iLambda+1),3] = xlambda
}

plot(Err[,1], type='p', col='red', ylim=c(0,1),
		main = 'Error vs Log(Lambda)',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)

jpeg('MulticlassRidge_w_redundant_coding.jpg')
plot(Err[,1], type='p', col='red', ylim=c(0,0.05),
		main = 'Error vs Log(Lambda)',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)
dev.off()



#What effect does redundant coding of the target have?
data1_4 <- data[I1_4,]
data.train <- data1_4[,-1]

target <- matrix(0.0,4,4)
target[1,] <- c(1,0,0,0)
target[2,] <- c(0,1,0,0)
target[3,] <- c(0,0,1,0)
target[4,] <- c(0,0,0,1)

bigY <- NULL
for(i in 1:nrow(data1_4)){
	bigY <- rbind(bigY, target[data1_4[i,1],])
}



Err <- matrix(nrow = 21, ncol = 3)
iAll <- seq(1:nrow(data1_4))

for(iLambda in seq(from = 0, to = 20)){
	#
	exp <- (+4 -4*(iLambda/20))
	xlambda <- 10^exp
	
	testErr <- 0.0
	trainErr <- 0.0
	
	for(ixval in seq(from = 1, to = 5)){
		Iout <- which(iAll%%5 == ixval - 1)
		
		Xin <- data.train[-Iout,]
		Xout <- data.train[Iout,]
		bigYIn <- bigY[-Iout,]
		bigYOut <- bigY[Iout,]
		YhTrain <- NULL
		YhTest <- NULL
		for(icol in 1:ncol(target)){
			Yin <- bigYIn[,icol]
			Yout <- bigYOut[,icol]
			dataIn  <- cbind(Yin,Xin)
			mod <- lm.ridge(Yin~., data = dataIn, lambda = xlambda)
			C <- mod$coef/mod$scales
			XM <- Xin
			for(i in seq(from = 1, to = ncol(Xin))){
				XM[,i]<-Xin[,i]-mod$xm[i]
			} 
			xTemp <- as.matrix(XM)
			A <- as.array(C)
			Yh <- xTemp%*%A + mod$ym
			YhTrain <- cbind(YhTrain,Yh)
			
			XM <- Xout
			for(i in seq(from = 1, to = ncol(Xout))){
				XM[,i]<-Xout[,i]-mod$xm[i]
			} 
			xTemp <- as.matrix(XM)
			A <- as.array(C)
			Yh <- xTemp%*%A +mod$ym
			YhTest <- cbind(YhTest,Yh)
		}
		IyhTrain <- rep(0.0,nrow(YhTrain)) 
		IyTrain <- IyhTrain
		IyhTest <- rep(0.0,nrow(YhTest))
		IyTest <- IyhTest
		for(i in 1:nrow(YhTrain)){
			yTest <- P(YhTrain[i,])
			IyhTrain[i] <- decode(yTest,target)
			yTest <- bigYIn[i,]
			IyTrain[i] <- decode(yTest,target)
		}
		for(i in 1:nrow(YhTest)) {
			yTest <- P(YhTest[i,])
			IyhTest[i] <- decode(yTest,target)
			yTest <- bigYOut[i,]
			IyTest[i] <- decode(yTest,target)
		}
		
		trainErr <- trainErr + (1-sum(IyhTrain == IyTrain)/length(IyTrain))/5
		testErr <- testErr + (1-sum(IyhTest == IyTest)/length(IyTest))/5
	}
	
	Err[(iLambda+1),1] = trainErr
	Err[(iLambda+1),2] = testErr
	Err[(iLambda+1),3] = xlambda
}

jpeg('MulticlassRidge_w_simple_coding.jpg')
plot(Err[,1], type='p', col='red', ylim=c(0,0.05),
		main = 'Error vs Log(Lambda)',
		ylab='Error',
		xlab='3 - Log(Lambda)')
points(Err[,1], pch=15, col='red')
#lines(out[,2], type='l', col='blue')
points(Err[,2], pch=16, col='blue')
legend(5, 0.2, c("TRAIN", "TEST"), cex = 1, col = c("red", "blue"),
		pch = c(15, 16), lty = 1:2)
dev.off()   

###########################################################
#linear discriminant analysis
############################################################

Prob <- function(meanX, covX, x){
	# x%*%solve(S,x)
	nDim <- nrow(covX)
	dX <- x - meanX
	qf <- - 0.5*x%*%solve(covX,x)
	const <- sqrt(det(covX)*(2*pi)^nDim)
	return(qf/const)
}



data <- read.table(file="zip.train", header = FALSE)
iIndices <- list()
for(i in 1:4){
	Itemp <- which(data[,1] == i)
	iIndices[[i]] <- Itemp
}
iLength <- rep(0.0,4)
for(i in 1:4) {iLength[i] <- length(iIndices[[i]])}

I1_4 <- rep(0.0,sum(iLength))
sumlength = 0.0
for(i in 1:4){	
	for(j in 1:iLength[i]){	I1_4[j + sumlength] = iIndices[[i]][j] }
	sumlength <- sumlength + iLength[i]
}


data1_4 <- data[I1_4,]
data.train <- data1_4[,-1]
Y <- data1_4[,1]

testErr <- 0.0
trainErr <- 0.0
means <- matrix(0.0,4,256)
iAll <- seq(1:nrow(data1_4))

for(ixval in seq(from = 1, to = 5)){
	Iout <- which(iAll%%5 == ixval - 1)
	
	Xin <- data.train[-Iout,]
	Xout <- data.train[Iout,]
	Yin <- Y[-Iout]
	Yout <- Y[Iout]
	yHTrain <- rep(0.0,nrow(Xin))
	yHTest <- rep(0.0,nrow(Xout))
#calculate means and single covariance
	I1 <- which(Yin == 1)
	I2 <- which(Yin == 2)
	I3 <- which(Yin == 3)
	I4 <- which(Yin == 4)
	
	for(j in 1:256){
		means[1,j] = mean(Xin[I1,j])
		means[2,j] = mean(Xin[I2,j])
		means[3,j] = mean(Xin[I3,j])
		means[4,j] = mean(Xin[I4,j])
	}
	
	XTemp <- matrix(0.0,nrow(Xin),ncol(Xin))
	for(i in 1:nrow(Xin)){
		for(j in 1:ncol(Xin)){
			XTemp[i,j] <- Xin[i,j] - means[Yin[i],j]
		}
	}
	covX <- cov(XTemp)
	nDim <- nrow(covX)
	const <- sqrt(det(covX)*((2*pi)^nDim))
	invCovX <- matrix(0.0,256,256)
	for(i in 1:256){
		ei <- matrix(0.0,256,1)
		ei[i] <- 1.0
		invCovX[,i] <- solve(covX,ei)
	}
	
	pvect <- c(0.0,0.0,0.0,0.0)
	for(i in 1:nrow(Xin)){
		for(j in 1:4){
			meanX <- means[j,]
			x <- Xin[i,]
			dX <- as.matrix(x - meanX)
			dX <- t(dX)
			qf <- - 0.5*t(dX)%*%invCovX%*%dX			
			#return(qf/const)
			pvect[j] <- exp(qf)/const   #Prob(meanX,covX,x)
		}
		yHTrain[i] <- which.max(pvect)		
	}
	
	
	
	for(i in 1:nrow(Xout)){
		for(j in 1:4){
			meanX <- means[j,]
			x <- Xout[i,]
			dX <- as.matrix(x - meanX)
			dX <- t(dX)
			qf <- - 0.5*t(dX)%*%invCovX%*%dX
			pvect[j] <- exp(qf)/const
		}
		yHTest[i] <- which.max(pvect)
	}	
	
	trainErr <- trainErr + (1-sum(yHTrain == Yin)/length(Yin))/5
	testErr <- testErr + (1-sum(yHTest == Yout)/length(Yout))/5
}


##  generates training error-rate of 0.01313090 
##  and test error-rate of 0.03940887
##  for first xval loop
##  appears to be over-trained



##  Let's switch to two-class problem in order to speed the computation
## 
##  this doesn't work because the class-by-class data are linearly dependent
##  the pooled covariance is non-singular if enough different classes are included
##  from the example above (digits 1,2,3 & 4) have a non-singular pooled covariance
##  1&2 or 2&3 are both have singular pooled covariances.

data <- read.table(file="zip.train", header = FALSE)
iIndices <- list()
for(i in 1:2){
	Itemp <- which(data[,1] == i+1)
	iIndices[[i]] <- Itemp
}
iLength <- rep(0.0,2)
for(i in 1:2) {iLength[i] <- length(iIndices[[i]])}

I2_3 <- rep(0.0,sum(iLength))
sumlength = 0.0
for(i in 1:2){	
	for(j in 1:iLength[i]){	I2_3[j + sumlength] = iIndices[[i]][j] }
	sumlength <- sumlength + iLength[i]
}


data2_3 <- data[I2_3,]
data.train <- data2_3[,-1]
Y <- data2_3[,1]

testErr <- 0.0
trainErr <- 0.0
means <- matrix(0.0,2,256)
iAll <- seq(1:nrow(data2_3))
covX <- matrix(0.0,256,256)

for(ixval in seq(from = 1, to = 5)){
	Iout <- which(iAll%%5 == ixval - 1)
	
	Xin <- data.train[-Iout,]
	Xout <- data.train[Iout,]
	Yin <- Y[-Iout]
	Yout <- Y[Iout]
	yHTrain <- rep(0.0,nrow(Xin))
	yHTest <- rep(0.0,nrow(Xout))
#calculate means and single covariance
	#I1 <- which(Yin == 1)
	I2 <- which(Yin == 2)
	I3 <- which(Yin == 3)
	#I4 <- which(Yin == 4)
	
	for(j in 1:256){
		a = I2[1]
		b = I2[length(I2)]
		means[1,j] = mean(Xin[a:b,j])
		a = I3[1]
		b = I3[length(I3)]
		means[2,j] = mean(Xin[a:b,j])
		#means[3,j] = mean(Xin[I3,j])
	#means[4,j] = mean(Xin[I4,j])
	}
	
	XTemp <- matrix(0.0,nrow(Xin),ncol(Xin))
	for(i in 1:length(I2)){
		for(j in 1:ncol(Xin)){
			XTemp[i,j] <- Xin[i,j] - means[1,j]
		}
	}
	for(i in (length(I2)+1):length(I3)){
		for(j in 1:ncol(Xin)){
			XTemp[i,j] <- Xin[i,j] - means[1,j]
		}
	}

	for(i in 1:256){
		for(j in i:256){
			a <- XTemp[,i]
			b <- XTemp[,j]
			covX[i,j] <- t(a)%*%b
		}
	}
	for(i in 2:256){
		for(j in 1:i){
			covX[i,j] <- covX[j,i]
		}
	}
	
	covX <- cov(XTemp)
	nDim <- nrow(covX)
	const <- sqrt(det(covX)*((2*pi)^nDim))
	invCovX <- matrix(0.0,256,256)
	for(i in 1:256){
		ei <- matrix(0.0,256,1)
		ei[i] <- 1.0
		invCovX[,i] <- solve(covX,ei)
	}
	
	pvect <- c(0.0,0.0,0.0,0.0)
	for(i in 1:nrow(Xin)){
		for(j in 1:2){
			meanX <- means[j,]
			x <- Xin[i,]
			dX <- as.matrix(x - meanX)
			dX <- t(dX)
			qf <- - 0.5*t(dX)%*%invCovX%*%dX			
			#return(qf/const)
			pvect[j] <- exp(qf)/const   #Prob(meanX,covX,x)
		}
		yHTrain[i] <- which.max(pvect)		
	}
	
	
	
	for(i in 1:nrow(Xout)){
		for(j in 1:2){
			meanX <- means[j,]
			x <- Xout[i,]
			dX <- as.matrix(x - meanX)
			dX <- t(dX)
			qf <- - 0.5*t(dX)%*%invCovX%*%dX
			pvect[j] <- exp(qf)/const
		}
		yHTest[i] <- which.max(pvect)
	}	
	
	trainErr <- trainErr + (1-sum(yHTrain == Yin)/length(Yin))/5
	testErr <- testErr + (1-sum(yHTest == Yout)/length(Yout))/5
}
