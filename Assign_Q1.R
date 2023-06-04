# Problem 1
train = read.csv("train.csv")
test = read.csv("test.csv")
library(mice)
library(rms)
library(finalfit)

describe(train)
md.pattern(train)
# 총 149개의 missing value
missing_pairs(train) 
# missing value와 다른 값들의 분포가 거의 동일하다. 
# 별다른 pattern이 없는 것을 확인
missing.clus = naclus(train,method='average')
missing.clus
plot(missing.clus) 
# (X1, X2), (X3, X4) cluster 확인
# Y변수에는 missing이 없기 때문에, y변수에 대한 패턴은 확인X
train
set.seed(1)
imp= mice(train, m=10, method=c('rf', 'rf', 'rf', 'rf',''))
pred = imp$predictorMatrix 
pred[, 'Y'] = 0
imp= mice(train, m=10, method=c('rf', 'rf', 'pmm', 'rf',''), predictorMatrix = pred, print=F)

plot(imp, c('X1', 'X2', 'X3', 'X4'), layout=c(2,4)) # convergence 확인
stripplot(imp, pch=20, cex=1.2) # 비슷한 값을 가지는가? 확인
densityplot(imp, scales=list(relation='free'),layout=c(2,2)) # Density 확인
# X3 : rf 사용시 p-value를 지나치게 낮추는 경향
# X4 : ppm 사용시 수렴 X
# linear regression for multiple imputation, averaging ~ 1개의 linear regression model화
library(gam)
library(randomForest)
library(glmnet)
comp.dat = na.omit(train)
fit.no.imp = lm(Y ~ X1 + X2 + X3 + X4, data=comp.dat)
summary(fit.no.imp) 
# 원 데이터셋 사용시 X3의 p-value가 줄어드는 경향 확인

fit = with(imp, lm(Y ~ X1 + X2 + X3 + X4))
# Average standard value, beta value
summary(pool(fit))
# 그러나 유의성은 그대로. 모든 변수가 유효(p.value << 0.05)
par(mfrow=c(2,2))
library(lmtest)
plot(fit.no.imp) 
dwtest(fit.no.imp)
# qqplot, residual plot 모두 정상
# 자기상관성 X, 독립성 만족

fit.gam.imp = gam(Y ~ s(X1,5) + s(X2,5) + s(X3,5) + s(X4,5), data=comp.dat)
plot(fit.gam.imp)
summary(fit.gam.imp) 
# X1, X2, X4은 linear하게 fitting 가능, X3은 3차 이상으로 fitting하는게 적절해 보임

# Model fitting 
M = imp$m
imp.dat = vector(mode='list',length=M)
for (m in 1:M) imp.dat[[m]] = complete(imp,m)

p.model = function(dat) gam(Y ~ X1 + s(X2,3) + s(X3,3) + s(X4,2), data=dat)
p
fit.imp = lapply(imp.dat, p.model)
yhat = lapply(fit.imp, predict, newdata=test)
yhat = matrix(unlist(yhat),nrow(test), M)
apply(yhat,1,mean)
MSE = mean((test$Y - yhat)^2)
MSE
# gam  : 19.93078
# RF : 24.62711
# linear regression : 22.3412
par(mfrow=c(1,1))
r = test$Y - yhat
plot(yhat, r)
lines(c(0,500),c(0,0),col='red') # fitting 확인, residual 정상.
qqplot(r)

