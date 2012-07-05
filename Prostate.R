# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################


setwd("C:\\Projects\\R")

pdata <- read.table(file="data", header = TRUE, row.names=1)

#put predictors into X
X <- pdata[,1:8]
#put response into Y
Y <- pdata[,9]
#pull out indices which authors used as training set
Itrain <- which(pdata[,10])

#demean X and bring to unit variance
for(i in 1:8){
	m <- sum(X[,i])
	m <- m/length(X[,i])
	X[,i] <- X[,i]-m
	v <- var(X[,i])
	X[,i] <- X[,i]/sqrt(v)
}

#put X into matrix form
X <- as.matrix(X)

#check covariance
cor(X)

#linear model on full set (train + test)
linMod <- lm(Y ~ X)
linMod


#restrict model to training set
YY <- Y[Itrain]
XX <- X[Itrain,1:8]

linMod <- lm(YY ~ XX)
linMod


######################
#######################
#best subsets
##############
##########

require(leaps)

leapOut <- leaps(X,Y,nbest = 40)
#have a look at leapOut
str(leapOut)


#have a look at leapOut$which
w <- leapOut$which
w



#Compute cross-validation error for these subsets.
xvalErr <- matrix(0.0, 8,40)
num2char <- c("1","2","3","4","5","6","7","8")
#start a loop for different subset sizes
colIndex <- 1:8

#loop on different subset sizes
for(isize in 2:8){
	iSubsets <- which(rownames(w)[]==num2char[isize])
	
#start a loop to run though best subsets
	for(isets in 1:length(iSubsets)){
		iCol <- which(w[iSubsets[isets],])
		Xtemp <- as.matrix(X[,iCol])		
	
		nxval <- 10
		Index <- 1:length(X[,1])

#crossvalidation
		for(ixval in 1:nxval){
			Iout <- which(Index%%nxval==(ixval-1))
			XtempTemp <- Xtemp[-Iout,1:ncol(Xtemp)]
			Xnew <- Xtemp[Iout,]
			Ytemp <- Y[-Iout]
			Ynew <- Y[Iout]
			linMod <- lm(Ytemp ~ XtempTemp)	
			v <- as.array(linMod$coefficients)
			yHat <- rep(0.0,length(Xnew[,1]))
			for(i in 1:length(Xnew[,1])){
				yHat[i] <- v[1]
				for(j in 1:isize){
					yHat[i] <- yHat[i] + Xnew[i,j]*v[j+1]
				}
				
			}
			dY <- yHat - Ynew
			se <- (1/length(Xnew[,1]))*sum(dY*dY)
			xvalErr[isize,isets] <- xvalErr[isize,isets] + se/nxval
			
		}

	}

}

#run separate for subset size = 1
isize <- 1
iSubsets <- which(rownames(w)[]==num2char[isize])

#start a loop to run though best subsets
for(isets in 1:length(iSubsets)){
	iCol <- which(w[iSubsets[isets],])
	Xtemp <- as.matrix(X[,iCol])	
	
	
	
	nxval <- 10
	Index <- 1:length(X[,1])
	
	for(ixval in 1:nxval){
		Iout <- which(Index%%nxval==(ixval-1))
		XtempTemp <- Xtemp[-Iout,1:ncol(Xtemp)]
		Xnew <- Xtemp[Iout,]
		Ytemp <- Y[-Iout]
		Ynew <- Y[Iout]
		linMod <- lm(Ytemp ~ XtempTemp)	
		v <- as.array(linMod$coefficients)
		yHat <- rep(0.0,length(Xnew))
		for(i in 1:length(Xnew)){
			yHat[i] <- v[1]
			for(j in 1:isize){
				yHat[i] <- yHat[i] + Xnew[i]*v[j+1]
			}
			
		}
		dY <- yHat - Ynew
		se <- (1/length(Xnew))*sum(dY*dY)
		xvalErr[isize,isets] <- xvalErr[isize,isets] + se/nxval
		
	}
	
}




#let's have a look at the xvalErr
xvalErr



#collect all the non-zero elements of xvalErr and plot according to subset size
output <- matrix(0.0,1,2)
temp <- output
output[1,1] <- 8
output[1,2] <- sqrt(xvalErr[8,1])
icount <- 1
for(i in 1:7){
	for(j in 1:40){
		if(xvalErr[i,j]>0.0){
			temp[1,1] <- i
			temp[1,2] <- sqrt(xvalErr[i,j])
			output <- rbind(output,temp)			
		}
	}
}
plot(output)


#Forward Step-wise - using cross-validation for variable selection.  
#Pick the first variable

Index <- 1:nrow(X)
colIndex <- 1:8
seBest <- 1000000.0
seArray <- rep(0.0,7)
Xtemp <- X[,1]
nxval <- 10
for(iTry in 1:8){
	Xtemp <- X[,iTry]
	se <- 0.0
	for(ixval in 1:nxval){
		Iout <- which(Index%%nxval==(ixval-1))
		XtempTemp <- Xtemp[-Iout]
		Xnew <- Xtemp[Iout]
		Ytemp <- Y[-Iout]
		Ynew <- Y[Iout]
		linMod <- lm(Ytemp ~ XtempTemp)	
		v <- as.array(linMod$coefficients)
		yHat <- rep(0.0,length(Xnew))
		for(i in 1:length(Xnew)){
			yHat[i] <- v[1] + Xnew[i]*v[2]		
		}
		dY <- yHat -Ynew
		seTemp <- (1/length(Xnew))*sum(dY*dY)
		se <- se + seTemp/nxval		
	}
	#print(se)
	if(se<seBest){
		seBest <- se
		iBest <- iTry
	}
}
seArray[1] <- seBest
I <- iBest


#run through the same calculation for the next 6 variables
for(iStep in 1:6){
	colSelection <- colIndex[-I]
	seBest <- 1000000
	for(iTry in 1:length(colSelection)){
		iCols <- c(I,colSelection[iTry])
		Xtemp <- as.matrix(X[,iCols])
		se <- 0.0
		for(ixval in 1:nxval){
			Iout <- which(Index%%nxval==(ixval-1))
			XtempTemp <- Xtemp[-Iout,]
			Xnew <- Xtemp[Iout,]
			Ytemp <- Y[-Iout]
			Ynew <- Y[Iout]
			linMod <- lm(Ytemp ~ XtempTemp)	
			
			v <- as.array(linMod$coefficients)
			isize <- length(v) - 1
			yHat <- rep(0.0,nrow(Xnew))
			for(i in 1:nrow(Xnew)){
				yHat[i] <- v[1]
				for(j in 1:isize){
					yHat[i] <- yHat[i] + Xnew[i,j]*v[j+1]
				}				
			}
			dY <- yHat - Ynew
			seTemp <- ((1/nrow(Xnew))*sum(dY*dY))
			se <- se + seTemp/nxval		
		}
		#print(se)
		if(se<seBest){
			seBest <- se
			iBest <- colSelection[iTry]
		}		
	}
	I <- c(I,iBest)
	print(I)
	seArray[iStep + 1] <- seBest	
}
points(sqrt(seArray), pch=".", cex=3, col=2)


#Backwards stepwise regression - using xval for selection
Index <- 1:nrow(X)
colIndex <- 1:8
colSelection <- colIndex
I <- NULL

seArray2 <- rep(0.0,7)

nxval <- 10

for(iStep in 1:6){
	
	seBest <- 1000000
	for(iTry in 1:length(colSelection)){
		iCols <- c(I,colSelection[iTry])
		Xtemp <- as.matrix(X[,-iCols])
		se <- 0.0
		for(ixval in 1:nxval){
			Iout <- which(Index%%nxval==(ixval-1))
			XtempTemp <- Xtemp[-Iout,]
			Xnew <- Xtemp[Iout,]
			Ytemp <- Y[-Iout]
			Ynew <- Y[Iout]
			linMod <- lm(Ytemp ~ XtempTemp)	
			
			v <- as.array(linMod$coefficients)
			isize <- length(v) - 1
			len <- length(Iout)
			yHat <- rep(0.0,len)
			for(i in 1:len){
				yHat[i] <- v[1]
				for(j in 1:isize){
					yHat[i] <- yHat[i] + Xnew[i,j]*v[j+1]
				}
				
			}
			dY <- yHat - Ynew
			seTemp <- ((1/len)*sum(dY*dY))
			se <- se + seTemp/nxval		
		}
		#print(se)
		if(se<seBest){
			seBest <- se
			iBest <- colSelection[iTry]
		}		
	}
	I <- c(I,iBest)
	colSelection <- colIndex[-I]
	print(colIndex[-I])
	seArray2[8-iStep] <- seBest
	
}

iStep <- 7
	
seBest <- 1000000
for(iTry in 1:length(colSelection)){
	iCols <- c(I,colSelection[iTry])
	Xtemp <- as.matrix(X[,-iCols])
	se <- 0.0
	for(ixval in 1:nxval){
		Iout <- which(Index%%nxval==(ixval-1))
		XtempTemp <- Xtemp[-Iout]
		Xnew <- Xtemp[Iout]
		Ytemp <- Y[-Iout]
		Ynew <- Y[Iout]
		linMod <- lm(Ytemp ~ XtempTemp)	
		
		v <- as.array(linMod$coefficients)
		isize <- length(v) - 1
		len <- length(Iout)
		yHat <- rep(0.0,len)
		for(i in 1:len){
			yHat[i] <- v[1] + Xnew[i]*v[2]
		}
		dY <- yHat - Ynew
		seTemp <- ((1/len)*sum(dY*dY))
		se <- se + seTemp/nxval		
	}
	#print(se)
	if(se<seBest){
		seBest <- se
		iBest <- colSelection[iTry]
	}		
}
I <- c(I,iBest)
colSelection <- colIndex[-I]
print(colIndex[-I])
seArray2[8-iStep] <- seBest
	


plot(sqrt(seArray2))
points(sqrt(seArray), pch=".",col=3, cex=3)

