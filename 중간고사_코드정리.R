####### variable selection #########
####### selecting variables : best subset selection ########
install.packages('leaps')
library(leaps)
library(datasets)
attenu
sum(is.na(attenu)) # 결측값 확인
data = na.omit(attenu) # 결측값 제거
####### best subset selection #########
fit.full = regsubsets(accel ~ event+mag+dist, data=data, nvmax=5, really.big = T)
bs = summary(fit.full)
l = which.max(bs$adjr2) # 2
l = which.min(bs$rss) # 3
l = which.min(bs$bic) # 2
# mag, dist를 변수로 선택
# method = 'Foward', 'Backward'로만 변경하면 다르게 사용가능

####### PCA : with correlation matrix ########
install.packages('HSAUR')
library(HSAUR)
data("heptathlon")
# checking multicolinearity
head(heptathlon)
summary(heptathlon)
hep.data = heptathlon[,-8]
fit = lm(score~., data=heptathlon)

library(car) 
vif(fit) #  VIF > 5일 경우 correlation이 높음
# 다양한 변수들이 correlation이 높게 나타남

pc = prcomp(hep.data, scale=T) # Scale : Normalization
pc# 7개의 pc 생성
hep.data
screeplot(pc, type='lines', pch=1, main="scree plot")
biplot(pc, main='Biplot')
# scree plot을 통해, cumulative proportion을 보고 결정
# PC4까지 사용
pc_val = pc$x[,1:4]
pc_val = cbind(pc_val, Y)
pc_val = as.data.frame(pc_val)
fit1 = gam(Y ~ s(PC1,5)+s(PC2,5)+s(PC3,5)+s(PC4,5), data=pc_val)
par(mfrow=c(1,4))
plot(fit1)
summary(fit1) #이렇게 PCA를 사용한 다음 modeling을 진행해도 된다

# 1. Nonlinear, Nonconstant modeling
fit1 = lm(accel ~ mag + dist, data=attenu)
plot(fit1)

##### GAM #####
library(gam)
fit2 = gam(accel ~ s(mag,5) + s(dist,5),data=attenu)
par(mfrow=c(1,2))
plot(fit2) # gam을 통해서 plot그린 후 model form 확인

fit2_1 = gam(accel ~ mag + s(dist,5),data=attenu)
anova(fit2,fit2_1) # 직선 fitting과의 유효성 비교
# mag: linear function, dist: exponential function
# Y = accel, X1 = mag, X2 = dist.
# Nonlinear model: Y = beta1 + beta2*X1 + beta3*X1^2 + beta4*exp(-beta5*X2).
# Parametical modeling
f = function(beta,X)
{
  X1 = X[,1]; X2 = X[,2]  
  beta[1] + beta[2]*X1 + beta[3]*X1^2 + beta[4]*exp(-beta[5]*X2)
}

# Objective function: RSS(mean function 추정)
RSS = function(beta,Y,X) sum((Y-f(beta,X))^2)

# Gradient vector of the objective function
grv = function(beta,Y,X) # 직접 계산한 후 넣어준다
{
  X1 = X[,1]; X2 = X[,2]
  R = Y - f(beta,X)
  c(-2*sum(R), -2*sum(R*X1), -2*sum(R*X1^2), -2*sum(R*exp(-beta[5]*X2)), 
    2*beta[4]*sum(R*X2*exp(-beta[5]*X2)))  
}

# Optimization
X = cbind(attenu$mag,attenu$dist)
colnames(X) = c('mag', 'dist')
Y = attenu$accel
ml1 = optim(rep(0.1,5), RSS, gr=grv, method='BFGS', X=X, Y=Y)
ml1

beta.hat = ml1$par
beta.hat

# Fitted value
Yhat = f(beta.hat,X)
# Residual plot
r = Y - Yhat
par(mfrow=c(1,1))
plot(Yhat,r,ylim=c(-0.5,0.5))
lines(c(-10,10),c(0,0),col='red')
# Linearly increasing variance pattern.
# Objective function for mean function: 
# Genearalized least square method
# beta의 추정 : max(yi-yhat/S), S = sigma^2 * g(z;gamma), z = cbind(1, Yhat)
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
# S: Covariance matrix

# Gradient vector function for beta
gr.mean = function(beta,Y,X,S)
{
  sigma2 = diag(S)
  X1 = X[,1]; X2 = X[,2]
  R = Y - f(beta,X)
  c(-2*sum(R/sigma2), -2*sum(R*X1/sigma2), -2*sum(R*X1^2/sigma2), 
    -2*sum(R*exp(-beta[5]*X2)/sigma2), 
    2*beta[4]*sum(R*X2*exp(-beta[5]*X2)/sigma2))  
}

# Linear variance function: |r| = gam1 + gam2*Yhat.
# For linear variance function, we can consider absolute residuals,
# instead of squared residuals.
# gam.hat = (Z^T W Z)^(-1) Z^T W |r|. # weighted least square 
install.packages('matrixcalc')
library(matrixcalc)
beta.new = ml1$par      # initial parameter설정
W = diag(rep(1,length(Y))) # weight function
mdif = 100000

# beta를 고정한 후 gamma, sigma를 추정 -> 다시 beta 추정
# nuisance parameter의 estimation이후 beta를 다시 추정한다
# beta의 변동이 거의 없어질 때 까지 진행(수렴)
while(mdif > 0.000001)
{
  Yhat = f(beta.new,X)
  r = Y - Yhat
  Z = cbind(1,Yhat)
  gam.hat = solve(t(Z) %*% W %*% Z) %*% t(Z) %*% W %*% abs(r) 
  sigma = Z %*% gam.hat
  S = diag(as.vector(sigma^2)) # covariance matrix
  
  if (is.non.singular.matrix(S)) W = solve(S)
  else W = solve(S + 0.000000001*diag(rep(1,nrow(S))))
  
  ml2 = optim(beta.new, obj.mean, gr=gr.mean,method='BFGS', Y=Y, X=X, S=S)
  beta.old = beta.new
  beta.new = ml2$par
  mdif = max(abs(beta.new - beta.old))
}

beta.new

Yhat = f(beta.new,X)
sigma = Z %*% gam.hat 
r = (Y - Yhat)/sigma # 이분산 가정이므로 sigma로 나눠야함

# Residual plot
plot(Yhat,r,ylim=c(-4,4))
lines(c(0,10),c(0,0),col='red') # 등분산을 잘 나타내면 성공

# variance pattern별 model정리
# linear
# exponential
# quadratic

##### Linear regression with nonconstant variance #####
# lmvar: 
# Linear mean function
# Linear variance function: log(sigma) = X%*%beta
# install.packages('lmvar')
library(lmvar)

X_s = cbind(attenu$mag, attenu$dist)
colnames(X_s) = c('mag', 'dist')
fit3 = lmvar(attenu$accel, X, X_s)
summary(fit3)

ms = predict(fit3, X_mu=X, X_sigma=X_s)
r1 = (Y - ms[,1])/ms[,2]
plot(ms[,1],r1)
lines(c(-10,10),c(0,0),col='red')

# 2. Time-series data modeling
tsdat = read.table('tsdat.txt',header=T)
fit = lm(y ~ x, data=tsdat)
summary(fit)
par(mfrow=c(2,2))
plot(fit) # use linear model for fitting
# Durbin-Watson test
install.packages('lmtest')
library(lmtest)
dwtest(fit)
# 만약 dwtest를 쓸 수 없는 경우(직접 모델링한 경우) : 
# 직접 dw statistic 계산
dw.stat = sum((r[2:80]-r[1:79])^2/sum(r^2)) 

# Check ACF & PACF
install.packages('astsa')
library(astsa)

# AR(p): ACF: Exponentially decreasing; PACF: Non-zero values at first p lags.
# MA(q): ACF: Non-zero values at first q lags; PACF: Exponentially decreasing.
# ARMA(p,q): ACF: Similar to ACF of AR(p); PACF: Similar to PACF of MA(q).
acf2(residuals(fit))
# exponentially decrease / ar(p), p는 범위 밖으로 튀어나온 p로 설정
ar1 = sarima(residuals(fit), 1,0,0, no.constant=T)   #AR(1)
ar1$fit
# MLE: Multivariate normal distribution
# 첫번째 방법, MVN 분포가정으로 진행
X = cbind(1,tsdat$x)
Y = tsdat$pm25
n = length(Y)
S = diag(rep(1,n))    # initial covariance matrix
mdif = 1000000
beta.old = rep(100000,2)
while(mdif > 0.0000001)
{
  beta.new = as.vector(solve(t(X) %*% solve(S) %*% X) %*%t(X) %*% solve(S) %*% Y)
  r = as.vector(Y - (X %*% beta.new))
  ar1 = sarima (r, 1,0,0, no.constant=T, details=F)
  alpha = ar1$fit$coef
  sigma2 = ar1$fit$sigma2
  
  mdif = max(abs(beta.new - beta.old))
  beta.old = beta.new
  # Construct covariance matrix
  S = matrix(nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if (i == j) S[i,j] = 1
      if (i != j) S[i,j] = alpha^(abs(i-j))
    }
  }
  S = (sigma2 / (1-alpha^2)) * S
}
round(beta.new,4)
# final model fitting

# 두번째 방법
# MLE: Product of conditional distribution (Approximation)
# Y_t | Y_t-1 ~ N(X_t*beta + alpha*epsilon_t-1, sigma^2)

fit = lm(y ~ x, data=tsdat)
Yt = tsdat$y[2:n]
Xt = tsdat$x[2:n]
et = residuals(fit)[1:(n-1)]
mdif = 10000
b.old = rep(0,3)

while(mdif > 0.0000001)
{
  fit.temp = lm(Yt ~ Xt + et)
  b.new = fit.temp$coefficient
  mdif = max(abs(b.new[1:2] - b.old[1:2]))
  
  et = (Y - X %*% b.new[1:2])[1:(n-1)]
  b.old = b.new
}

round(b.new,4)

# Built-in function 
# cochrane.orcutt => f: linear model, error: AR(p) process.
# install.packages("orcutt")
library(orcutt)
fit = lm(y ~ x, data=tsdat)
cochrane.orcutt(fit)

# 3. Spatial data modeling
install.packages('spatialreg')
library(spatialreg)
data(oldcol)
?COL.OLD
# 'COL.nb' has the neighbors list.
# 2-D Coordinates of observations
crds = cbind(COL.OLD$X, COL.OLD$Y)  

# Compute the maximum distance 
mdist = sqrt(sum(diff(apply(crds, 2, range))^2))   

# All obs. between 0 and mdist are identified as neighborhoods.
dnb = dnearneigh(crds, 0, mdist) 

# Compute Euclidean distance between obs.
dists = nbdists(dnb, crds)

# Compute Power distance weight d^(-2)
glst = lapply(dists, function(d) d^(-2))

# Construct weight matrix with normalization 
# style='C': global normalization; 'W': row normalization
lw = nb2listw(dnb, glist=glst, style='C')

# Spatial Autoregressive Model
fit = lagsarlm(CRIME ~ HOVAL + INC, data=COL.OLD, listw=lw)
summary(fit)

# Fitted values
predict(fit)
########## Cumulative logit model ##########
install.packages('ordinal')
library(ordinal)

?wine

?clm

fit = clm(rating ~ temp + contact, data=wine, link = 'logit')
summary(fit)

# 4. GLM modeling
########## Poisson regression model ##########
# For data
install.packages('lme4')
library(lme4)
data(grouseticks)
?grouseticks
head(grouseticks)
hist(grouseticks$TICKS,breaks=0:90)
# zero가 너무 많은 형태 : zero-inflated poisson
fit = glm(TICKS ~ HEIGHT*YEAR, data = grouseticks, family=poisson)
summary(fit)
# categorial variable : 96 : 1, 97 : 1 인 case로 coding됨 
# residual deviance : phat = residual deviance / dof
# overdispersion : phat >> 1인 경우

########## Negative binomial regression model ##########
library(MASS)

fit1 = glm.nb(TICKS ~ HEIGHT*YEAR, data = grouseticks, link=log)
summary(fit1)
# overdispersion : phat = 1.05, problem solved

# 7. Proportional Hazard model
ibrary(survival)

# For data
# install.packages('carData')
library(carData)

?Rossi #재범률 관련 데이터

Surv(Rossi$week, Rossi$arrest) 
# censored data, survival object로의 변환

fit = coxph(Surv(week,arrest) ~ fin + age+ race + wexp + mar + prio, 
            data=Rossi) 
summary(fit)
# 해석: h(t|x1)/h(t|x2).. = exp(x1 - x2)^T * beta
# binomial(1, 0) : 1 group, 0 group간의 비율 차이 : exp(coef)만큼의 차이
# 연속형 변수 : 특정 데이터 포인트와 포인트 간 비율 차이 : exp(coef)만큼의 차이
# 경향성은 설명할 수 있어도 전체를 설명하긴 힘듬
# firyes -> yes/no ; no/yes 순서대로 출력됨(햇갈리지 말것)

# Estimated survival function
plot(survfit(fit),ylim=c(0.6,1),xlab="Weeks", ylab="Prop.of Not Rearrested")


# Estimated survival functions for financial aid
Rossi.fin = with(Rossi, data.frame(fin=c(0, 1), age=rep(mean(age), 2), 
                                   race=rep(mean(race=='other'),2), 
                                   wexp=rep(mean(wexp=="yes"),2), 
                                   mar=rep(mean(mar=="not married"),2),
                                   prio=rep(mean(prio),2)))

plot(survfit(fit,newdata=Rossi.fin), conf.int=TRUE,
     lty=c(1, 2), ylim=c(0.6, 1), col=c('red','blue'), 
     xlab="Weeks", ylab="Prop. of Not Rearrested")

legend("bottomleft", legend=c("fin = no","fin = yes"), 
       lty=c(1 ,2),col=c('red','blue'))