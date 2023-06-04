####################################
# Statistical Data Mining Example7 #
####################################

install.packages('e1071')
library(e1071)
library(mvtnorm)

# Data generation
set.seed(1)
n1 = 20; n2 = 20
mean1 = c(6,6)
mean2 = c(3,3)
sig1 = matrix(c(2,0,0,2),2,2)
sig2 = matrix(c(2,-0.8,-0.8,2),2,2)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(1,n1),rep(-1,n2)))
train = data.frame(y,x)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(1,n1),rep(-1,n2)))
test = data.frame(y,x)


# Scatter plot for the training data
plot(cbind(train$X1,train$X2),col=(3-as.numeric(train$y)),xlab='X1',ylab='X2')


##### Support vector classifier #####

# In svm function, y should be a factor object.
# cost: Reverse direction of tuning parameter C.

fit = svm(y ~ ., data=train, kernel='linear', cost=10, scale=F)
summary(fit)
plot(fit,data=train)

# Indices of support vectors
fit$index


# smaller cost -> larger margin -> More support vectors.
fit1 = svm(y ~ ., data=train, kernel='linear', cost=0.01, scale=F)
summary(fit1)

# Determine cost: CV (cross=5 -> 5-fold CV)
set.seed(1)
cv.fit = tune(svm, y ~ ., data=train, kernel='linear', 
              tunecontrol=tune.control(cross=5),
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(cv.fit)

# SVC with the best C
best.fit = cv.fit$best.model
summary(best.fit)

# Prediction for test data
y.test = predict(best.fit, test)
table(predict=y.test, true = test$y)


##### Support Vector Machine (SVM) #####

# Data generation
set.seed(1)
n1 = 100; n2 = 100
mean1 = c(6,6)
mean2 = c(3,3)
sig1 = matrix(c(2,0,0,2),2,2)
sig2 = matrix(c(4,-0.8,-0.8,4),2,2)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(1,n1),rep(-1,n2)))
train = data.frame(y,x)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(1,n1),rep(-1,n2)))
test = data.frame(y,x)

# Scatter plot for the training data
plot(cbind(train$X1,train$X2),col=(3-as.numeric(train$y)),xlab='X1',ylab='X2')


fit = svm(y ~ ., data=train, kernel='radial',gamma=1, cost=1, scale=F)
summary(fit)
plot(fit, train)

set.seed(1)
cv.fit = tune(svm, y~., data=train, kernel='radial', 
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
              gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit)


# SVM with the best parameters
best.fit = cv.fit$best.model
summary(best.fit)

# Prediction for test data
y.test = predict(best.fit, test)
table(predict=y.test, true = test$y)




