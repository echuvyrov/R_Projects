# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################

setwd("C:\\Projects\\R")

mixSim <- read.table(file="mixtureSimData.data")
mixSimMat <- matrix(0.0,200,2)

mixSimMat[,1] <- mixSim[1:200,1]
mixSimMat[,2] <- mixSim[201:400,1]

plot(mixSimMat)
points(mixSimMat[101:200,1:2], col = 2)
points(mixSimMat[1:100,1:2], col = 3)
Y <- rep(0.0,200)
Y[101:200] <- 1.0

linMod <- lm(Y~mixSimMat)
coef <- linMod$coefficients

a <- (0.5-coef[1])/coef[3]
b <- -coef[2]/coef[3]
abline(a,b)


maxX1 <- max(mixSimMat[,1])
minX1 <- min(mixSimMat[,1])
maxX2 <- max(mixSimMat[,2])
minX2 <- min(mixSimMat[,2])

testMat <- matrix(0.0,10000,2)

for(i in 1:100){
	for(j in 1:100){
		x1 <- minX1 + i*(maxX1 - minX1)/100
		x2 <- minX2 + j*(maxX2 - minX2)/100
		index <- (i-1)*100 + j
		testMat[index,1] <- x1
		testMat[index,2] <- x2
	}	
}


XX <- c((1:100)*(maxX1 - minX1))
XX <- XX/100
XX <- XX + minX1

YY <- c((1:100)*(maxX2 - minX2))
YY <- YY/100
YY <- YY + minX2


#1st knn plot (15)
require(class)
KNN <- knn(mixSimMat, testMat, Y, 15)
ZZ <- matrix(0.0,100,100)
for(i in 1:100){
	for(j in 1:100){
		index <- (i-1)*100 + j
		ZZ[i,j] <- KNN[index]
	}
}
I1 <- which(KNN == 1)

plot(testMat, pch=".")
points(testMat[I1,], col=2, pch=".")
points(testMat[-I1,],col = 3, pch=".")
points(mixSimMat[1:100,], col = 3)
points(mixSimMat[101:200,], col = 2)
contour(XX,YY,ZZ,levels = 1.5, drawlabels = FALSE, add = TRUE)


#second knn plot (5)
KNN <- knn(mixSimMat, testMat, Y, 5)
ZZ <- matrix(0.0,100,100)
for(i in 1:100){
	for(j in 1:100){
		index <- (i-1)*100 + j
		ZZ[i,j] <- KNN[index]
	}
}
I1 <- which(KNN == 1)

plot(testMat, pch=".")
points(testMat[I1,], col=2, pch=".")
points(testMat[-I1,],col = 3, pch=".")
points(mixSimMat[1:100,], col = 3)
points(mixSimMat[101:200,], col = 2)
contour(XX,YY,ZZ,levels = 1.5, drawlabels = FALSE, add = TRUE)


# third knn plot (1)
KNN <- knn(mixSimMat, testMat, Y, 1)
ZZ <- matrix(0.0,100,100)
for(i in 1:100){
	for(j in 1:100){
		index <- (i-1)*100 + j
		ZZ[i,j] <- KNN[index]
	}
}
I1 <- which(KNN == 1)

plot(testMat, pch=".")
points(testMat[I1,], col=2, pch=".")
points(testMat[-I1,],col = 3, pch=".")
points(mixSimMat[1:100,], col = 3)
points(mixSimMat[101:200,], col = 2)
contour(XX,YY,ZZ,levels = 1.5, drawlabels = FALSE, add = TRUE)

