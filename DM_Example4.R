####################################
# Statistical Data Mining Example4 #
####################################

library(MASS)

# Dataset
Boston[1:5,]

dim(Boston)

?Boston

####################
# Subset Selection #
####################

install.packages('leaps')
library(leaps)

?regsubsets

# Best subset selection =======================================
fit.full = regsubsets(medv~., data=Boston, nvmax=13, nbest=3)
summary(fit.full)

fit.full = regsubsets(medv~., data=Boston, nvmax=13)
summary(fit.full)

bs = summary(fit.full)
names(bs)

bs$rss

par(mfrow=c(2,2))

plot(bs$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(bs$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
l = which.max(bs$adjr2)
points(l,bs$adjr2[l], col="red",cex=2,pch=20)

plot(bs$cp,xlab="Number of Variables",ylab="Cp",type='l')
l = which.min(bs$cp)
points(l,bs$cp[l],col="red",cex=2,pch=20)

plot(bs$bic,xlab="Number of Variables",ylab="BIC",type='l')
l = which.min(bs$bic)
points(l,bs$bic[l],col="red",cex=2,pch=20)

plot(fit.full,scale="r2")
plot(fit.full,scale="adjr2")
plot(fit.full,scale="Cp")
plot(fit.full,scale="bic")

# The model with 11 predictors was selected. 
coef(fit.full,11)

# Forward and Backward Stepwise Selection =========================

fit.fwd = regsubsets(medv~.,data=Boston,nvmax=13,method="forward")
summary(fit.fwd)

fs = summary(fit.fwd)
plot(fs$cp,xlab="Number of Variables",ylab="Cp",type='l')
l = which.min(bs$cp)
points(l,bs$cp[l],col="red",cex=2,pch=20)

fit.bwd=regsubsets(medv~.,data=Boston,nvmax=13,method="backward")
summary(fit.bwd)

# Example: Forward and backward stepwise methods have different paths to find the best one.
coef(fit.full,6)
coef(fit.fwd,6)
coef(fit.bwd,6)


#=======================================================================

# Dataset
install.packages('ISLR')
library(ISLR)

Hitters = na.omit(Hitters)
dim(Hitters)

n = nrow(Hitters)

index = sample(1:n, 163, replace=F)
train = Hitters[index,]
test = Hitters[-index,]

tr.x = model.matrix(Salary~.,data=train)[,-1]
tr.x[1:5,]
tr.y = train$Salary

te.x = model.matrix(Salary~.,data=test)[,-1]
te.y = test$Salary


####################
# Ridge Regression #
####################

# For ridge and lasso, use 'glmnet' package.
install.packages('glmnet')
library(glmnet)

?glmnet
# if alpha=0 -> ridge; if alpha=1 -> lasso.
# Default: standardize = TRUE

# Example
ridge=glmnet(tr.x,tr.y,alpha=0,lambda=1000)
coef(ridge)
te.yhat = predict(ridge,newx=te.x)
testmse = mean((te.y - te.yhat)^2)
testmse

# Find a tunning parameter.
grid = c(10^seq(10,0,length=100),0)
ridge = glmnet(tr.x,tr.y,alpha=0,lambda=grid)
coef.ridge = coef(ridge)

te.yhat = predict(ridge,s=grid,newx=te.x)

te.mse = function (yhat,y) mean((y - yhat)^2)

mse = apply(te.yhat, 2, te.mse, y=te.y)

plot(1:101,mse,type='l',col='red',lwd=2,xlab='Index of lambda',ylab='Test MSE')
plot(60:80,mse[60:80],type='l',col='red',lwd=2,xlab='Index of lambda',ylab='Test MSE')

l = which(mse == min(mse))
ridge$lambda[l]
coef.ridge[,l]


####################
# Lasso Regression #
####################

grid = c(10^seq(3,0,length=50),0)

lasso = glmnet(tr.x, tr.y,alpha=1,lambda=grid)
coef.lasso = coef(lasso)
te.yhat = predict(lasso,s=grid,newx=te.x)

mse = apply(te.yhat, 2, te.mse, y=te.y)

plot(1:51,mse,type='l',col='red',lwd=2,xlab='Index of lambda',ylab='Test MSE')
plot(10:30,mse[10:30],type='l',col='red',lwd=2,xlab='Index of lambda',ylab='Test MSE')

l = which(mse == min(mse))
lasso$lambda[l]

coef.lasso[,l]

round(coef.lasso[,l],4)


# cv.glmnet ------------------------------

x = model.matrix(Salary~.,data=Hitters)[,-1]
y = Hitters$Salary

cvfit = cv.glmnet(x,y,family='gaussian',alpha=1)
plot(cvfit)

# lambda that gives the smallest CV stat.
cvfit$lambda.min

yhat = predict(cvfit,newx=x,s='lambda.min')


###################################
# Principal Components Regression #
###################################

install.packages('pls')

library(pls)

pcr.fit = pcr(Salary~.,data=train,scale=TRUE)
summary(pcr.fit)

mse = numeric(19)
for (npc in 1:19)
{
  te.yhat = predict(pcr.fit,te.x,ncomp=npc)
  mse[npc] = mean((te.y - te.yhat)^2)
}

plot(1:19,mse,type='b',col='red')


# By cross-validation
pcr.fit = pcr(Salary~.,data=Hitters,scale=TRUE,validation='CV')
validationplot(pcr.fit,val.type="MSEP")


#########################
# Partial Least Squares #
#########################

pls.fit=plsr(Salary~., data=Hitters,scale=TRUE)
summary(pls.fit)

mse = numeric(19)
for (npc in 1:19)
{
  te.yhat = predict(pls.fit,te.x,ncomp=npc)
  mse[npc] = mean((te.y - te.yhat)^2)
}

plot(1:19,mse[1:19],type='b',col='red')


# By cross-validation
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")








