# larsESLCh3fig10  
# examples of 
#     fit.lasso<-lars(x,y, type="lasso")  
#           default normalize = TRUE, intercept = TRUE)
#     plot(fit.lasso,breaks=F,xvar="norm")
# uses data set from http://www-stat.stanford.edu/~tibs/ElemStatLearn/
#      prostrate data set
#  
# Author: PatriciaHoffman
###############################################################################
rm(list=ls())

#install.packages("faraway")
#install.packages("lars")
#   help.search("lars")
#   ?lars::lars
#   library(help="lars")
library(faraway)
library(lars)

prostate<-read.csv("ProstateCancerDataESL.csv", sep=",",header=T)

dim(prostate)
names(prostate)
attributes(prostate)


#split the data into training and test sets using ESL designation

I <- seq(from = 1, to = nrow(prostate))
Itest <- which(prostate[I,10] == FALSE)
x.train <- prostate[-Itest,1:8]
x.test <- prostate[Itest,1:8]
y.train<-prostate[-Itest,9]
y.test<-prostate[Itest,9]


## check some values
#   x.test;Itest
#
#   summary(x.train);str(x.train);attributes(x.train)
#   summary(y.train);str(y.train);attributes(y.train)
#   attributes(x.test)


#use lasso to fit the data and then plot
fit.lasso<-lars(as.matrix(x.train),y.train, type="lasso")#,normalize = TRUE, intercept = TRUE)
plot(fit.lasso,breaks=F,xvar="norm")
#plot of the L1 norm of the coefficient vector, 
#    as a fraction of the maximal L1 norm



###########################################
#
#   Now for some fun experiments inspired by lars example from Seni's book
#
###########################################

rm(list=ls())

#install.packages("faraway")
#install.packages("lars")
#   help.search("lars")
#   ?lars::lars
#   library(help="lars")
library(faraway)
library(lars)

#load in and look at the data
data(prostate)
#help(prostate)
attach(prostate)
dim(prostate)
names(prostate)

#error in 32 data needs to be corrected
prostate[32,]
prostate[32,]
prostate$lweight[32] <- 44.9
prostate[32,]

# note that the scaling of the data has NOT been performed
# the function lars by default does this

#split the data into training and test sets
set.seed(321);i.train<-sample(1:97,67)
x.train<-prostate[i.train,1:8];y.train<-prostate[i.train,9]
x.test<-prostate[-i.train,1:8];y.test<-prostate[-i.train,9]


#use lasso to fit the data and then plot
fit.lasso<-lars(as.matrix(x.train),y.train, type="lasso")
plot(fit.lasso,breaks=F,xvar="norm")
#plot of the L1 norm of the coefficient vector, 
#    as a fraction of the maximal L1 norm


#next use cross validation to select optimal value for coef vector a
cv.lasso <- cv.lars(as.matrix(x.train), y.train, type="lasso")

#figure out the "best" model
# most parsimonious model 
#	with prediction error within one standard error of the minimun
i.min <- which.min(cv.lasso$cv)
i.se <- which.min(abs(cv.lasso$cv-(cv.lasso$cv[i.min]+cv.lasso$cv.error[i.min])))
s.best <- cv.lasso$fraction[i.se]

#now s.best corresponds to the "best" model 

s.best

#retrieve the optimal coefficients

predict.lars(fit.lasso, s = s.best, type="coefficients", mode = "fraction")


#compute the corresponding error on the test set

y.hat.test <- predict.lars(fit.lasso, x.test, s=s.best, type = "fit", mode = "fraction")
sum((y.hat.test$fit-y.test)^2)/30

#compare this with OLR using all coefficients:
fit.lm <- lm(lpsa ~., data = prostate[i.train,])
fit.lm$coeff

y.hat.lm.test <- predict(fit.lm, prostate[-i.train,])
sum((y.hat.lm.test - prostate$lpsa[-i.train])^2)/30

#############################################################################
#more experiments

#setting the fraction (s.best) nearer to zero results in more zero coeff
#   while setting the fraction (s.best) toward one results in non-zero coeffs

##################################################################
low = 0.3; high = 0.6


predict.lars(fit.lasso, s = low, type="coefficients", mode = "fraction")

predict.lars(fit.lasso, s = high, type="coefficients", mode = "fraction")


#next just look at the model which is the simple minimum

simplemin <-which.min(cv.lasso$cv)
simple <- cv.lasso$fraction[simplemin]

predict.lars(fit.lasso, s = simple, type="coefficients", mode = "fraction")

cv.lasso
names(cv.lasso)

# example with more parameters in cv.lars set ... K is the # of folds in cv
junk <- cv.lars(as.matrix(x.train), y.train, type="lasso", K = 10, fraction = seq(from = 0, to = 1, length = 100), 
		trace = FALSE, plot.it = TRUE, se = TRUE)




#Now try it on all the data

#split the data into training and test sets
set.seed(321);i.train<-sample(1:97,67)
x.all<-prostate[,1:8]; y.all<-prostate[,9]



#use lasso to fit the data and then plot
fit.lasso<-lars(as.matrix(x.all),y.all, type="lasso")
plot(fit.lasso,breaks=F,xvar="norm")
#plot of the L1 norm of the coefficient vector, 
#    as a fraction of the maximal L1 norm


#next use cross validation to select optimal value for coef vector a
cv.lasso <- cv.lars(as.matrix(x.all), y.all, type="lasso")

#figure out the "best" model
i.min <- which.min(cv.lasso$cv)
i.se <- which.min(abs(cv.lasso$cv-(cv.lasso$cv[i.min]+cv.lasso$cv.error[i.min])))
s.best <- cv.lasso$fraction[i.se]

#now s.best corresponds to the "best" model 

s.best

#retrieve the optimal coefficients

predict.lars(fit.lasso, s = s.best, type="coefficients", mode = "fraction")


# note that the answer changes slightly 
#    depending on the random sample that is choosen
