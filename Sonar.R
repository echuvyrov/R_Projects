# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################


library(MASS)
require(glmnet)
require(splines)
setwd("/home/mike-bowles/Documents/ML101/DataSets/Sonar")
STrain <- read.table("sonar_train.csv",sep = ",",header = FALSE)
STest <- read.table("sonar_test.csv",sep = ",",header = FALSE)
Sonar <- rbind(STrain,STest)
sonarMat <- as.matrix(Sonar)
x <- sonarMat[,1:60]
y <- as.factor(sonarMat[,61])

#plain ole logistic regression
cvob2=cv.glmnet(x,y,family="binomial",type.measure="class")
plot(cvob2)
title("glmnet logistic regression of sonar",line=2.5)
min(cvob2$cvm)


#re-do with 4df spline function basis expansion
vtemp <- x[,1]
xBs <- bs(vtemp,df=4)
for(i in 2:60){
	vtemp <- x[,i]
	xBs <- cbind(xBs,bs(vtemp,df=4))
}

cvob3=cv.glmnet(xBs,y,family="binomial",type.measure="class")
plot(cvob3)
title("glmnet logistic regression of sonar w. 4df spline expansion",line=2.5)
min(cvob3$cvm)


#try 2nd order polynomial expansion
xPoly <- x

for(i in 1:60){	
	vtemp <- x[,i]
	for(j in i:60){
		vtemp2 <- x[,j]
		xPoly <- cbind(xPoly, vtemp*vtemp2)
	}
}



cvob4=cv.glmnet(xPoly,y,family="binomial",type.measure="class")
plot(cvob4)
title("glmnet logistic regression of sonar w. polynomial expansion",line=2.5)
min(cvob4$cvm)


#apparently, granulatiry on the x's is more important than pair-wise
#interaction.  let's try more granularity
vtemp <- x[,1]
xBs2 <- bs(vtemp,df=6)
for(i in 2:60){
	vtemp <- x[,i]
	xBs2 <- cbind(xBs2,bs(vtemp,df=4))
}



cvob5=cv.glmnet(xBs2,y,family="binomial",type.measure="class")
plot(cvob5)
title("glmnet logistic regression of sonar w. 6df spline expansion",line=2.5)
min(cvob5$cvm)


# and even more
vtemp <- x[,1]
xBs3 <- bs(vtemp,df=10)
for(i in 2:60){
	vtemp <- x[,i]
	xBs3 <- cbind(xBs3,bs(vtemp,df=4))
}

cvob6=cv.glmnet(xBs3,y,family="binomial",type.measure="class")
plot(cvob6)
title("glmnet logistic regression of sonar w. 10df spline expansion",line=2.5)
min(cvob6$cvm)