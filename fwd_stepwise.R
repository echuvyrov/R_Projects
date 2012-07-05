############################################################################### 
# Author: Eugene Chuvyrov
###############################################################################

setwd("C:\\Projects\\R")
pdata <- read.table(file="wines.data", sep = ";", header = TRUE)

#put predictors into X
X <- pdata[,1:11]
#put response into Y
Y <- pdata[,12]

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

#begin forward step-wise regression modeling work
#pick the first variable

Index <- 1:nrow(X)
colIndex <- 1:11
seBest <- 1000000.0
seArray <- rep(0.0,7)
Xtemp <- X[,1]
nxval <- 12

#loop through all variables
for(iTry in 1:11){
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


#run through the same calculation for the next 10 variables
for(iStep in 1:10){
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

plot(sqrt(seArray))
points(sqrt(seArray), pch=".", cex=3, col=2)
