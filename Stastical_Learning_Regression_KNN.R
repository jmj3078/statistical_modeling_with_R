install.packages('ISLR')
library(ISLR)

?Hitters
dat = Hitters[!is.na(Hitters$Salary), ]
dat

m = nrow(dat)
n = round(m*0.6)
train = sample(1:m, n)
dat.tr = dat[train, ]
dat.te = dat[-train, ]

#linear regression
attach(dat.tr)
X.tr = cbind(1,AtBat,Hits,Years)
Y.tr = Salary
bhat = solve(t(X.tr)%*%X.tr)%*%t(X.tr)%*%Y.tr
detach(dat.tr)
#solve : bhat 값의 계산, b vector저장

Yhat.tr = X.tr%*%bhat
# b vector 계산하여 yhat 함수 계산

trainMSE = mean((Y.tr-Yhat.tr)^2)
trainMSE
#Train MSE, Mean Squared Error
trainRSS = sum((Y.tr-Yhat.tr)^2)
trainRSS
#Train RSS

attach(dat.te)
X.te=cbind(1,AtBat,Hits,Years)
Y.te=Salary
detach(dat.te)

Yhat.te = X.te%*%bhat
testMSE = mean((Y.te-Yhat.te)^2)
trainRSS = sum((Y.te-Yhat.te)^2)
testMSE
trainRSS
# train MSE <<< test MSE

fit=lm(Salary~AtBat + Hits + Years, data=dat.tr)
summary(fit)

#beta hat - LSE
fit$coefficients
fit$fitted.values
Yhat = fit$fitted
Yhat = predict(fit)
train.MSE = mean((Y.tr-Yhat)^2)
train.MSE
Yhat0 = predict(fit, dat.te)
test.MSE = mean((Y.te-Yhat0)^2)
test.MSE
#train.MSE << test.MSE

#F-test for group of variables
#training data
tn = nrow(dat.tr)
div.tr = character(tn)
div.tr[dat.tr$League == 'A' & dat.tr$Division == 'E'] = 'AE'
div.tr[dat.tr$League == 'A' & dat.tr$Division == 'W'] = 'AW'
div.tr[dat.tr$League == 'N' & dat.tr$Division == 'E'] = 'NE'
div.tr[dat.tr$League == 'N' & dat.tr$Division == 'W'] = 'NW'
factor(div.tr)

tn = nrow(dat.te)
div.te = character(tn)
div.te[dat.te$League == 'A' & dat.te$Division == 'E'] = 'AE'
div.te[dat.te$League == 'A' & dat.te$Division == 'W'] = 'AW'
div.te[dat.te$League == 'N' & dat.te$Division == 'E'] = 'NE'
div.te[dat.te$League == 'N' & dat.te$Division == 'W'] = 'NW'
factor(div.te)

fit.full = lm(Salary ~ AtBat + Hits + Years + factor(div.tr), data=dat.tr)
summary(fit.full) #division, league를 포함한 model
summary(fit) #division, league를 포함하지 않은 model
anova(fit, fit.full) #두 모델간의 다중분산분석(F-test)
# F-value = ((RSSf-RSSt)/l)/RSSf/n-p-1 ~ F(l, n-p-1)

#t-test for beta3
summary(fit)
# t-value = beta3/Var(beta1) = std.error(beta1)
# t-value = 35,1667/6.1515 = 5.717
qt(0.95, 158-3-1) #t DOF = n-p-1
# 5.717 >>>> 1.654808 이므로 beta3은 유효한 변수,

#Confience Inteval(CI) for training data
predict(fit, interval = 'confidence', level = 0.95)
#회귀모형 기댓값의 신뢰구간

#Prediction Inverval(PI) for test data
predict(fit, dat.te, interval = 'prediction', level = 0.95)
#회귀모형 예측값(Y^ new의)의 신뢰구간

##########################
###KNN-Regression Model###
##########################

KNN.reg = function(K, X.tr,Y.tr, X.te = NULL, Y.te=NULL)
  {
  if (is.null(X.te) || is.null(Y.te))
  {
    X.te = X.tr
    Y.te = Y.tr
  }
# K: Smoothness parameter
# X.tr: Input matrix of training data
# Y.tr: Output vector of training data
# X.te: Input matrix of test data 
# Y.te: Output vector of test data
  ntr = nrow(X.tr)
  nte = nrow(X.te)

#predictor의 standardization
  sd.tr = apply(X.tr, 2, sd) #sd - 표준편차 함수

  X.tr = X.tr / matrix(sd.tr, nrow=ntr, ncol=length(sd.tr), byrow=T)
  X.te = X.te / matrix(sd.tr, nrow=nte, ncol=length(sd.tr), byrow=T)

#Compute distance values between test obs and trainig obs
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
  names(result) = c('Yhat', 'MSE')
  return(result)
}

attach(dat.tr)
X.tr = cbind(AtBat, Hits, Years)
Y.tr = Salary
detach(dat.tr)

attach(dat.te)
X.te = cbind(AtBat, Hits, Years)
Y.te = Salary
detach(dat.te)

KNN.reg(3, X.tr, Y.tr) #MSE : 72099.39
#training MSE, mean square error 
KNN.reg(3, X.tr, Y.tr, X.te, Y.te) #MSE : 133269.2
#test MSE, mean square error
#test MSE >>> training MSE

K=1:50
testMSE = numeric(length(K)) 
for(k in K){
  fit = KNN.reg(k, X.tr ,Y.tr, X.te, Y.te)
  testMSE[k] = fit$MSE
  }

plot(K,testMSE,type='l')
#test MSE가 가장 낮은 K값 - 가장 설명력이 좋은 모델

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
#랜덤한 uniform distribution을 가진 x에 대하여
#y를 형성하고 모델을 만들고 MSE를 계산하는 과정을 1000번 반복
for (i in 1:N)
{
  # Training set
  x = runif(n,-1,1.5) #runif : 난수생성, 1과 1.5사이
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

plot(MSE.M1)
plot(MSE.M2)
plot(x, y)

# Model 1
M1 = apply(MSE.M1,2,mean)
mean(M1)	
# Estimated Expected Test MSE for Model 1

# Model 2
M2 = apply(MSE.M2,2,mean)
mean(M2)	
# Estimated Expected Test MSE for Model 2

#단순 linear model인 M2의 MSE가 더 낮음
#3차원 모델을 사용하는것은 overfitting problem 있음을 확인
#높은 선형성을 띄기 떄문에 가능한 상황으로 보임

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
#KNN MSE 0.2996737

# Model 2: Linear model
mean(MSE.M2)
#Linear MSE : 0.0558423
#차원이 커질때 KNN모델이 더 큰 영향을 받음(Model Variance증가)
