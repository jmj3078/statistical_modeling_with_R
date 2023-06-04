####################################
# Statistical Data Mining Example2 #
####################################

# Dataset
install.packages('ISLR')
library(ISLR)

?Carseats
attach(Carseats)
high = ifelse(Sales > 8, 'Y', 'N')
cs = data.frame(Carseats,high)
detach(Carseats)

cs[1:5,]

#######################
# Logistic Regression #
#######################

fit = glm(high ~ CompPrice + Income + Advertising + Population + Price + Age + Education, data=cs, family=binomial)
summary(fit)
coef(fit)

probs = predict(fit,type="response")
probs[1:10]

pred = rep('N',nrow(cs))
pred[probs > 0.5] = 'Y'

table(pred,high)

# Misclassification rate
1 - sum(high == pred)/nrow(cs)


###################################
# Multinomial Logistic Regression #
###################################

install.packages('nnet')
library(nnet)

?iris

n = nrow(iris)
index = sample(1:n, 100)
train = iris[index,]
test = iris[-index,]


mfit = multinom(Species ~ ., data=train)
summary(mfit)

train$S2 = relevel(train$Species, ref='virginica')	# Baseline is 'virginica'.
mfit2 = multinom(S2 ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train)
summary(mfit2)

yhat = predict(mfit, newdata=test)

# Test error
1 - sum(test$Species == yhat)/nrow(test)


################################
# Linear Discriminant Analysis #
################################

library(MASS)

train = train[,-6]

fit.lda = lda(Species ~ ., data=train)
pred = predict(fit.lda, newdata=test)

pred$posterior[1:10,]

temp = apply(pred$posterior,1,which.max)
yhat = character(nrow(test))
yhat[temp == 1] = 'setosa'
yhat[temp == 2] = 'versicolor'
yhat[temp == 3] = 'virginica'

# Test error
1 - sum(test$Species == yhat)/nrow(test)


###################################
# Quadratic Discriminant Analysis #
###################################

fit.qda = qda(Species ~ ., data=train)
pred = predict(fit.qda, newdata=test)

temp = apply(pred$posterior,1,which.max)
yhat = character(nrow(test))
yhat[temp == 1] = 'setosa'
yhat[temp == 2] = 'versicolor'
yhat[temp == 3] = 'virginica'

# Test error
1 - sum(test$Species == yhat)/nrow(test)

#######################
# K-Nearest Neighbors #
#######################

library(class)

x.tr = train[,1:4]
y.tr = train[,5]
x.te = test[,1:4]
y.te = test[,5]

yhat = knn(x.tr, x.te, y.tr, k=3)

# Test error
1 - sum(test$Species == yhat)/nrow(test)


