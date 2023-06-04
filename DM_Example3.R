####################################
# Statistical Data Mining Example3 #
####################################

library(ISLR)

?Carseats

attach(Carseats)
high = ifelse(Sales > 8, 'Y', 'N')
cs = data.frame(Carseats,high)
detach(Carseats)



####################
# Cross-Validation #
####################

install.packages('cvTools')
library(cvTools)

?cvFit

##### Linear Regression #####
# LOOCV
fit = lm(Sales ~ CompPrice + Income + Population + Age, data=Carseats)

LOOCV.lm = cvFit(fit, data=Carseats, y=Carseats$Sales, cost=mspe, K=nrow(Carseats))

LOOCV.lm$cv


# K-fold Cross-validation
cv.lm = cvFit(fit, data=Carseats, y=Carseats$Sales, cost=mspe, K=5, R=10)

cv.lm$cv
cv.lm$se


##### Logistic Regression #####

install.packages('boot')
library(boot)

?cv.glm

fit = glm(high ~ CompPrice + Income + Population + Age, data=cs, family=binomial)

cv.lg = cv.glm(data=cs, glmfit=fit, cost=mspe, K=5)

cv.lg$delta[1]

##### General case #####

n = nrow(cs)
K = 5

cvf = cvFolds(n, K)

library(MASS)

cvstat = numeric(K)
for (k in 1:K)
{
	index = cvf$subsets[cvf$which == k]
	train = cs[-index,]
	test = cs[index,]
	fit = lda(high ~ CompPrice + Income + Population + Age, data=train)
	pred = predict(fit, newdata = test, type='response')
	yhat = pred$class
	cvstat[k] = 1 - sum(test$high == yhat)/nrow(test)
}
mean(cvstat)

#############
# Bootstrap #
#############

# Estimation of Standard Error

n = 50
x1 = runif(n, 0, 20)
x2 = runif(n, -20, 20)
y = 5 + 5*x1 - 2*x2 + rnorm(n,sd=10)
dat = data.frame(y,x1,x2)

fit = lm(y ~ x1 + x2)
summary(fit)

boot.fn = function(data, index)
{
	bdat = data[index,]
	fit.boot = lm(y ~ x1 + x2, data=bdat)
	return(coef(fit.boot))
}


b.boot = boot(data=dat, boot.fn, R=1000)
b.boot

# Test error estimation using bootstrap

test.fn = function(data, index)
{
	bdat = data[index,]
	fit = lda(high ~ CompPrice + Income + Population + Age, data=bdat)
	pred = predict(fit, newdata = data, type='response')
	yhat = pred$class
	return(1 - sum(data$high == yhat)/nrow(data))
}

test.boot = boot(data=cs, test.fn, R=1000)
test.boot









