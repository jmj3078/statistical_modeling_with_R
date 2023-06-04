# 모범답안 : Exponentially Incresing Time-series data
# Regression function
f = function(beta,X) beta[1]+beta[2]*exp(beta[3]*X)
# Objective function
RSS = function(beta,X,Y) sum((Y-f(beta, X))^2)
# Gradient Vector
grv = function(beta,X,Y){
  R = Y - f(beta, X)
  c(-2*sum(R), -2*sum(R*exp(beta[3]*X)), -2*beta[2]*sum(R*X*exp(beta[3]*X)))
# Checking Normality
qqnorm(r)
# Checking constant residual
plot(Y.hat, r)
# Durbin-Watson test
install.packages('lmtest')
library(lmtest)
fit = lm(Y~I(exp(beta.hat[3]*X))) 
# 어차피 beta가 linear형태를 유지하니까 간단하게 내장함수로 할 수 있음
dwtest(fit)
# 직접 계산
dw.stat = sum((r[2:80]-r[1:79])^2/sum(r^2))
# AR model 설정
library(astsa)
acf2(residuals(fit)) # 그냥 residual을 계산해 넣어도 된다
# ACF - exponentially decreasing
# PACF - AR(p) 결정

# MLE : beta의 추정
obj.mean = function(beta,Y,X,S) t(Y-f(beta,X)) %*% solve(S) %*% (Y-f(beta,X))
gr.mean = function(beta,Y,X,S){
  n = length(Y)
  R = Y - f(beta, X)
  Z = 2*t(R)%*%t(solve(S))%*%diag(rep(-1, n))
  c(Z%*%rep(1,n),
    Z%*%(exp(beta[3]*X)),
    Z%*%(X*exp(beta[3]*X)))
}
# Estimation
n = length(Y)
S = diag(rep(1,n))
mdif = 1000000
beta.new = ml$par
beta.old = rep(1000000, 3)
while(mdif > 0.000001){
  ml1 = optim(beta.new, obj.mean, gr=gr.mean,method=’BFGS’, Y=Y,X=X,S=S)
  beta.new = ml1$par
  r = as.vector(Y - f(beta.new,X))
  ar1 = sarima (r, 1,0,0, no.constant=T, details=F)
  alpha = ar1$fit$coef
  sigma2 = ar1$fit$sigma2 # alpha, sigma 추정
  
  mdif =  max(abs(beta.new - beta.old))
  beta.old = beta.new
  S = matrix(nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      if(i==j) S[i,j]=1
      if (i != j) S[i,j] = alpha^(abs(i-j)) 
    }
  }
  S = (sigma2 / (1-alpha^2)) * S # constant matrix형성, 우리가 아는 형태 그대로
}

#유효한 변수의 확인 : gam만 써도 가능

install.packages('gam')
library(gam)
fit=gam(Y ~s(X1, 5)+s(X2, 5)+s(X3, 5), data = Q6)
summary(fit)
# ANOVA for Parametric effects : not significant
par(mfrow=c(1,3))

plot(fit) # X2 : fitted line이 0근처에서 움직이고있음
# formula를 모두 function으로 만들 필요 없이 lm을 사용해도 상관없음
fit = lm(Y ~ X1 + I(X3^2), data=Q6)
summary(fit)
par(mfrow=c(1,1))
plot(fit)
X = model.matrix(Y~X1 + I(X3^2), data=Q6)
Y = as.vector(Q6$Y)
beta.new = fit$coefficient
mdif = 1000000
W = diag(rep(1,length(Y)))
library(matrixcalc)
while(mdif>0.000001)
{
  Yhat = X %*% beta.new
  r = Y - Yhat
  Z = cbind(1,Yhat)
  gam.hat = solve(t(Z)%*% W %*%Z)%*%t(Z)%*% W %*%abs(r)
  # gamma.hat은 위처럼 Weighted least square의 형태로 계산 가능
  sigma = Z %*% gam.hat
  S = diag(as.vector(sigma^2))
  
  if (is.non.singular.matrix(S)) W = solve(S)
  else W = solve(S+0.000000001*diag(rep(1,nrow(S))))
  
  beta.old = beta.new
  beta.new = solve(t(X)%*%W%*%X) %*% t(X) %*% W %*% Y
  mdif = max(abs(beta.new - beta.old))
}
# Z => r0 + r1*y를 위해 만든 계산식
beta.new
Yhat = X %*% beta.new
sigma = Z %*% gam.hat
r = (Y - Yhat)/sigma
par(mfrow=c(1,1))
plot(Yhat, r)
lines(c(0,150), c(0,0), col='red') # constant variance

