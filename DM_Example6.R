####################################
# Statistical Data Mining Example6 #
####################################

install.packages('tree')
library(tree)
library(ISLR)
attach(Carseats)

########################
# Classification Trees #
########################

High = as.factor(ifelse(Sales<=8, "No", "Yes"))
Carseats = data.frame(Carseats, High)

# Training set & Test set
set.seed(100)
train = sample(1:nrow(Carseats), 200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

# Tree model
tree.carseats = tree(High~.-Sales, Carseats, subset=train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty=0)

tree.carseats

tree.pred = predict(tree.carseats, Carseats.test, type="class")

table(tree.pred,High.test)

# Pruning-------------------------------------------------
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass)
# FUN: default = deviance

names(cv.carseats)
cv.carseats
# $size: # of terminal nodes
# $dev: deviance, misclassification rate
# $k: tuning parameter alpha

# Alpha corresponding to the smallest cv statistic.
bk = cv.carseats$k[which(cv.carseats$dev == min(cv.carseats$dev))]

prune.carseats = prune.misclass(tree.carseats, k=bk)
plot(prune.carseats)
text(prune.carseats,pretty=0)

tree.pred = predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)


####################
# Regression Trees #
####################

library(MASS)
set.seed(20)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~.,Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

# Pruning ----------------------------------------
cv.boston = cv.tree(tree.boston)
cv.boston

# Alpha corresponding to the smallest cv statistic.
bk = cv.boston$k[which(cv.boston$dev == min(cv.boston$dev))]

prune.boston = prune.tree(tree.boston, k=bk)
plot(prune.boston)
text(prune.boston,pretty=0)

yhat = predict(tree.boston,newdata=Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

##############################
# Bagging and Random Forests #
##############################

install.packages('randomForest')
library(randomForest)

# Bagging (m = p)
bag.boston = randomForest(medv~.,data=Boston,subset=train, mtry=13, importance=TRUE)
bag.boston

yhat.bag = predict(bag.boston, newdata=Boston[-train,])

plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

# Importance of variables
importance(bag.boston)
varImpPlot(bag.boston)

# Random Forests (m < p)
rf.boston=randomForest(medv~., data=Boston, subset=train, mtry=4, importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

############
# Boosting #
############

install.packages('gbm')
library(gbm)

boost.boston = gbm(medv~., data=Boston[train,], distribution='gaussian', 
                   n.trees=10000, interaction.depth=2, shrinkage=0.01)
# Continuous output: distribution = 'gaussian'
# Binary output: distribution = 'bernoulli'
# n.trees: # of iterations
# interaction.depth: # of terminal nodes in each tree
# shrinkage: lambda

# Partial dependence plot: Marginal effect of individual input variables
plot(boost.boston, i='rm')
plot(boost.boston, i='lstat')

yhat.boost = predict(boost.boston, Boston[-train,], n.trees=10000)
mean((yhat.boost - Boston[-train,]$medv)^2)




