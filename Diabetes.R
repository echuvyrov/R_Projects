#install.packages("elasticnet")
#install.packages("glmnet")
#install.packages("parcor")
#help(package="elasticnet")
#help(package="lars")
#help(package="parcor")

require("lars")
require("elasticnet")
require("parcor")
data(diabetes)
attach(diabetes)

#1. LARS
larsCVErrorsObject <- cv.lars(x2, y, K = 10, type="lar", plot.it = TRUE, trace = TRUE)
which.min(larsCVErrorsObject$cv)

larsObject <- lars(x2,y, type = "lar", trace = TRUE)
#larsObject <- lars(x2,y, type = "lar", trace = TRUE, max.steps = 16)
plot(larsObject, xvar="step")
print(larsObject)

#2. Lasso
lassoCVErrorsObject <- cv.lars(x2, y, K = 10, type="lasso", plot.it = TRUE, trace = TRUE)
which.min(lassoCVErrorsObject$cv)

lassoObject <- lars(x2,y, type = "lasso", trace = TRUE)
#lassoObject <- lars(x2,y, type = "lasso", trace = TRUE, max.steps = 4)
plot(lassoObject, xvar="step")
print(lassoObject)

#3. Ridge
ridgeCVErrorsObject <- ridge.cv(x2, y, k = 10, plot.it = TRUE, trace = TRUE)
which.min(lassoCVErrorsObject$cv)

lassoObject <- lars(x2,y, type = "lasso", trace = TRUE)
#lassoObject <- lars(x2,y, type = "lasso", trace = TRUE, max.steps = 4)
plot(lassoObject, xvar="step")
print(lassoObject)


#4. Elastic Net
enetCVErrorsObject <- cv.enet(x2,y, K = 10, lambda=0.05, s=1:50, mode="step", plot.it = TRUE, trace = TRUE)
which.min(enetCVErrorsObject$cv)

enetObject <- enet(x2,y, lambda = 0.05)
#enetObject <- enet(x2,y, lambda = 0.05, max.steps = 19)
plot(enetObject, xvar="step")
print(enetObject)

detach(diabetes)
