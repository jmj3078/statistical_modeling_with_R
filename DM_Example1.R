####################################
# Statistical Data Mining Example1 #
####################################

install.packages('ISLR')
library(ISLR)

?Hitters

# Create Training and Test datasets ------------------
dat = Hitters[!is.na(Hitters$Salary),]

m = nrow(dat)
n = round(m*0.6)

train = sample(1:m,n)
dat.tr = dat[train,]
dat.te = dat[-train,]

#write.table(dat.tr,'D:/Class/SKKU/2020/Datamining/Example1/datr.txt')
#write.table(dat.te,'D:/Class/SKKU/2020/Datamining/Example1/date.txt')

#dat.tr = read.table('D:/Class/SKKU/2020/Datamining/Example1/datr.txt',header=T)
#dat.te = read.table('D:/Class/SKKU/2020/Datamining/Example1/date.txt',header=T)

#####################
# Linear regression #
#####################
# Estimation of beta ---------------------------------
attach(dat.tr)
X.tr = cbind(1,AtBat,Hits,Years)
Y.tr = Salary
bhat = solve(t(X.tr)%*%X.tr)%*%t(X.tr)%*%Y.tr
detach(dat.tr)

# beta hat(LSE)
bhat

# Prediction for training data -----------------------
Yhat.tr = X.tr %*% bhat
trainMSE = mean((Y.tr - Yhat.tr)^2)
trainMSE

# Prediction for test data ---------------------------
attach(dat.te)
X.te = cbind(1,AtBat,Hits,Years)
Y.te = Salary
detach(dat.te)

Yhat.te = X.te %*% bhat
testMSE = mean((Y.te - as.vector(Yhat.te))^2)
testMSE

##### Using built-in function 'lm' #####

# Fit a linear regression model.
fit = lm(Salary ~ AtBat + Hits + Years,data=dat.tr)
summary(fit)

# beta hat (LSE)
fit$coef

Yhat = fit$fitted
Yhat = predict(fit)
train.MSE = mean((Y.tr-Yhat)^2)
train.MSE

Yhat0 = predict(fit,dat.te)
test.MSE = mean((Y.te-Yhat0)^2)
test.MSE

# F-test for a group of variables
tn = nrow(dat.tr)
div.tr = character(tn)
div.tr[dat.tr$League == 'A' & dat.tr$Division == 'E'] = 'AE'
div.tr[dat.tr$League == 'A' & dat.tr$Division == 'W'] = 'AW'
div.tr[dat.tr$League == 'N' & dat.tr$Division == 'E'] = 'NE'
div.tr[dat.tr$League == 'N' & dat.tr$Division == 'W'] = 'NW'

tn = nrow(dat.te)
div.te = character(tn)
div.te[dat.te$League == 'A' & dat.te$Division == 'E'] = 'AE'
div.te[dat.te$League == 'A' & dat.te$Division == 'W'] = 'AW'
div.te[dat.te$League == 'N' & dat.te$Division == 'E'] = 'NE'
div.te[dat.te$League == 'N' & dat.te$Division == 'W'] = 'NW'

fit.full = lm(Salary ~ AtBat + Hits + Years + factor(div.tr), data=dat.tr)

summary(fit.full)
anova(fit,fit.full)

# Confidence interval for training data
predict(fit,interval='confidence',level=0.95)

# Prediction interval for test data
predict(fit,dat.te,interval='prediction',level=0.95)

#################################
# K-Nearest Neighbor regression #
#################################

KNN.reg = function(K, X.tr ,Y.tr, X.te=NULL, Y.te=NULL)
# K: Smoothness parameter
# X.tr: Input matrix of training data
# Y.tr: Output vector of training data
# X.te: Input matrix of test data
# Y.te: Output vector of test data
{
	if (is.null(X.te) || is.null(Y.te))
	{
		X.te = X.tr
		Y.te = Y.tr
	}

	ntr = nrow(X.tr)
	nte = nrow(X.te)
	# Predictors should be standardized in KNN.
	sd.tr = apply (X.tr, 2, sd)
	X.tr = X.tr / matrix(sd.tr, nrow=ntr, ncol=length(sd.tr), byrow=T)
	X.te = X.te / matrix(sd.tr, nrow=nte, ncol=length(sd.tr), byrow=T)

	# Compute distance values between test obs and training obs.
	norm.tr = apply(X.tr^2, 1, sum) 
	norm.te = apply(X.te^2, 1, sum)  

	mat.norm.tr = matrix(data=norm.tr, nrow=nte, ncol=ntr, byrow=T) 
	mat.norm.te = matrix(data=norm.te, nrow=nte, ncol=ntr, byrow=F)

	dist.te.tr = mat.norm.te + mat.norm.tr - 2*X.te %*% t(X.tr)

	Yhat = numeric(nte)
	for (i in 1:nte) 
	{
		neighbors = order(dist.te.tr[i,])
		Yhat[i] = mean(Y.tr[neighbors[1:K]])
	}
	MSE = mean((Y.te - Yhat)^2)
	result = list(Yhat,MSE)
	names(result) = c('Yhat','MSE')
	return(result)
}


attach(dat.tr)
X.tr = cbind(AtBat,Hits,Years)
Y.tr = Salary
detach(dat.tr)

attach(dat.te)
X.te = cbind(AtBat,Hits,Years)
Y.te = Salary
detach(dat.te)

KNN.reg(3, X.tr ,Y.tr)

KNN.reg(3, X.tr ,Y.tr, X.te, Y.te)

K = 1:50
testMSE = numeric(length(K))
for (k in K)
{
	fit = KNN.reg(k, X.tr ,Y.tr, X.te, Y.te)
	testMSE[k] = fit$MSE
}

plot(K,testMSE,type='l')


###################################
# Estimation of Expected Test MSE #
###################################

set.seed(0)
# Standard deviation of error term
sde = 1

# Test data
x0 = seq(-1,1.5,0.01)
m = length(x0)
y0 = 1 + x0 - 3*x0^2 + 3*x0^3 + rnorm(m,sd=sde)
plot(x0,y0)


N = 1000
n = 50
MSE.M1 = NULL
MSE.M2 = NULL
for (i in 1:N)
{
  # Training set
  x = runif(n,-1,1.5)
  y = 1 + x - 3*x^2 + 3*x^3 + rnorm(n,sd=sde)
  
  # Model 1
  fit1 = lm(y ~ x + I(x^2) + I(x^3))
  yhat0M1 = cbind(1,x0,x0^2,x0^3) %*% fit1$coef
  MSE.M1 = rbind(MSE.M1,(y0-yhat0M1)^2)
  
  # Model 2
  fit2 = lm(y ~ x)
  yhat0M2 = cbind(1,x0) %*% fit2$coef
  MSE.M2 = rbind(MSE.M2,(y0-yhat0M2)^2)
}

# Model 1
M1 = apply(MSE.M1,2,mean)
mean(M1)	# Estimated Expected Test MSE for Model 1

# Model 2
M2 = apply(MSE.M2,2,mean)
mean(M2)	# Estimated Expected Test MSE for Model 2
# polynomial regression model has better fit on data 

############################
# Effect of High dimension #
############################
# 3 dimensions

set.seed(0)
# Standard deviation of error term
sde = 0.1

# Test data
m = 100
x10 = runif(m)
x20 = runif(m)
x30 = runif(m)
X0 = cbind(1,x10,x10^2,x20,x20^2,x30,x30^2)
beta = c(1,2,2,1,2,1,1)
y0 = X0 %*% beta + rnorm(m,sd=sde)

N = 1000
n = 50
MSE.M1 = NULL
MSE.M2 = NULL
for (i in 1:N)
{
  x1 = runif(n)
  x2 = runif(n)
  x3 = runif(n)
  X = cbind(1,x1,x1^2,x2,x2^2,x3,x3^2)
  beta = c(1,2,2,1,2,1,1)
  y = X %*% beta + rnorm(n,sd=sde)
  # Model 1
  fit1 = KNN.reg(K=5,cbind(x1,x2,x3),y,cbind(x10,x20,x30),y0)
  MSE.M1 = c(MSE.M1,fit1$MSE)
  
  # Model 2
  fit2 = lm(y ~ x1 + x2 + x3)
  yhat0M2 = cbind(1,x10,x20,x30) %*% fit2$coef
  MSE.M2 = rbind(MSE.M2,mean((y0-yhat0M2)^2))
}

# Model 1: KNN
mean(MSE.M1)

# Model 2: Linear model
mean(MSE.M2)








