############################################
# Statistical Modelling & Machine Learning #
#               R Example5                 #
############################################

options(warn = -1)  # Turn off warning message

# Data generation
install.packages('mvtnorm')
library(mvtnorm)
# 다변량 정규분포에서 나오는 값을 생성해 비율을 맞춘다

set.seed(10)
n1 = 500; n2 = 50 
# n2 : 작은그룹 데이터
# Seperability가 있는 경우, 기존 모델로도 잘 작동
# Separability가 없는 경우에는 기존의 메서드로는 어렵다.
# 평가기준을 바꿀 필요가 있다(f1 score - TPR, FPR)
mean1 = c(6,6)
mean2 = c(4,5)
sig1 = matrix(c(2,0,0,2),2,2) # variance, covariance matrix
sig2 = matrix(c(2,-0.8,-0.8,2),2,2) # variance, covariance matrix

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(0,n1),rep(1,n2))) # train dataset 생성
train = data.frame(y,x)

x = rbind(rmvnorm(n1,mean1,sig1),rmvnorm(n2,mean2,sig2))
y = as.factor(c(rep(0,n1),rep(1,n2)))
test = data.frame(y,x)


# Scatter plot for the training data
plot(cbind(train$X1,train$X2),
     col=(3-as.numeric(train$y)),xlab='X1',ylab='X2')
# 상당히 겹쳐있는 상태. 구별하기 어렵다
# f-score measure를 써야 한다는 것을 쉽게 판단 가능하다
# 대부분의 경우 작은 그룹을 1로, 큰 그룹을 0으로 설정해두고 진행한다
# 이는 매우 중요하다(다양한 메서드에서 이를 요구함)

# Classifier: Logistic regression 
fit = glm(y~., family=binomial, data=train)
phat.test = predict(fit, test, type='response')
yhat.test = ifelse(phat.test > 0.5, 1, 0)
# 일반적인 decision rule 사용

cm  = table(true = test$y, predict=yhat.test)
cm 
# TP / FN
# FP / TN
# 잘못 분류된 42개의 관찰값이 매우 큰 영향을 미치는 상황 

misclass = function(cm) 1 - sum(diag(cm))/sum(cm)
# cm: confusion matrix (higher value = positive class)
fmeasure = function(cm)
{
  TPR = cm[2,2]/sum(cm[2,])
  PPV = cm[2,2]/sum(cm[,2])
  return((2*TPR*PPV)/(TPR + PPV))
}

# Test misclassification rate
misclass(cm)
# 낮을수록 좋다.
# 0.09, 매우 낮다, 91 % 잘 분류한다는 결과

# Test F-measure
fmeasure(cm)
# 높을수록 좋다.
# 0.2424
# 이 경우는 겹침+Imabalanced dataset의 영향이 큼.
# 다만, 전체적인 겹쳐있는 부분을 완화시켜주진 않는다.
# Imbalanced dataset이 가진 문제를 완화시킬 뿐이다. 

# ROC curve ------------------------------------------
install.packages('pROC')
library(pROC)

phat.tr = predict(fit, train, type='response')
  
lr.roc = roc(train$y ~ phat.tr)
plot(lr.roc)

# AUC, 모델의 성능을 평가
# AUC = 1 : perfect model
# 전체 MCR이 영향을 주기 때문에 모델의 성능이 매우 좋게 나온다.
auc(lr.roc) 

########## Alternate cut-off (using training dataset) ##########
  # Fird the closest point on ROC curve to (1,1).
th = coords(lr.roc,x='best', best.method = 'closest.topleft')
th # 가장 좋은 specificity와 sensitivity를 보인 threshold값 반환
# 새로운 cutoff값으로 사용한다.

# Evaluation for the new cut-off
yhat.test1 = ifelse(phat.test > th$threshold, 1, 0)

cm1 = table(true = test$y, predict=yhat.test1)
cm1 
# False Positve의 수가 10으로 줄어들었다.
# 모델의 pvalue는 모두 그대로이다. threshold만 달라진 것.
# error가 trade-off된 것을 확인할 수 있다.

# Test misclassification rate
misclass(cm1)
# 0.2363636. 매우 안좋아졌다.
# Test F-measure
fmeasure(cm1)
# 0.3809524. 2배 이상 향상된 모습을 볼 수 있다.

################### Adjusting prior prob. ####################
library(MASS)
# lda, qda에 사용하는 prior probability를 조절한다.
# 큰 그룹은 높은 prior probability, 작은 그룹은 작은 prior probaibility가 정상
# 이를 조절해서 둘의 균형을 맞춰준다.

fit = lda(y~., data=train)
yhat.te = predict(fit,test)$class

cm = table(true = test$y, predict=yhat.te)
cm

# Test misclassification rate
misclass(cm)

# Test F-measure
fmeasure(cm)

# Adjust prior prob.
fit1 = lda(train$y, x = as.matrix(train[,-1]), prior=c(0.6,0.4)) 
# prior 직접 지정, 데이터로부터 계산하지 마라!
yhat.te1 = predict(fit1 ,as.matrix(test[,-1]))$class

cm1 = table(true = test$y, predict=yhat.te1)
cm1

# Test misclassification rate
misclass(cm1)
# 0.1763636
# Test F-measure
fmeasure(cm1)
# 0.4327485, 엄청 향상되었다. 직접 모델에 영향을 미치기 때문에 좀 더 강력한 방법

#################### Sampling methods ####################
install.packages('caret')
library(caret)

# SVM model from the original imbalaned data
install.packages('e1071')
library(e1071)

# tune 함수를 통해서 cost, gamma를 찾는다
cv.fit = tune(svm, y~., data=train, kernel='radial', 
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                          gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit)

# SVM with the best parameters
best.fit = cv.fit$best.model

# Prediction for test data
yhat.te = predict(best.fit, test)
cm = table(true = test$y, predict=yhat.te)
cm

misclass(cm)

fmeasure(cm)

# Upsampling ---------------------------------------------------
# Sampling with replacement from the small class.
set.seed(10)
uptrain = upSample(x = train[,-1], y=train$y, yname='y')

dim(uptrain)
 
table(uptrain$y) # 두 클래스의 관찰값 수가 똑같을 때 까지 업샘플링

# Scatter plot for the upsampled training data
plot(cbind(uptrain$X1,uptrain$X2),
     col=(3-as.numeric(uptrain$y)),xlab='X1',ylab='X2')
# 똑같은 위치에 똑같은 점이 평균적으로 10개씩 더 찍힌 상태임.
# 그림으로 그려보면 50개가 그대로 있다.

# SVM for upsampled data.
set.seed(10)
cv.fit1 = tune(svm, y~., data=uptrain, kernel='radial', 
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                          gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit1) 

# SVM with the best parameters
best.fit1 = cv.fit1$best.model

# Prediction for test data
yhat.te1 = predict(best.fit1, test)
cm1 = table(true = test$y, predict=yhat.te1)
cm1

misclass(cm1) # 떨어짐

fmeasure(cm1) # 미미하게 상승


# Dwonsampling --------------------------------------------
# Randomly remove obs. in the large class.
set.seed(1)
dntrain = downSample(x = train[,-1], y=train$y, yname='y')

dim(dntrain)

table(dntrain$y)

# Scatter plot for the downsampled training data
plot(cbind(dntrain$X1,dntrain$X2),
     col=(3-as.numeric(dntrain$y)),xlab='X1',ylab='X2')


# SVM for downsampled data.
set.seed(10)
cv.fit2 = tune(svm, y~., data=dntrain, kernel='radial', 
               ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit2)

# SVM with the best parameters
best.fit2 = cv.fit2$best.model

# Prediction for test data
yhat.te2 = predict(best.fit2, test)
cm2 = table(true = test$y, predict=yhat.te2)
cm2

misclass(cm2) # 안좋아짐

fmeasure(cm2) # 미미하게 상승

# SMOTE ----------------------------------------------
install.packages("smotefamily")
library(smotefamily)

set.seed(1)
sm = SMOTE(X=train[,-1], target=train$y, K=7)
# 가상의 데이터를 통한 upsampling
# KNN에 쓰이는 K 값을 설정해줘야 한다

smtrain = sm$data

dim(smtrain)
table(smtrain$class)


# Scatter plot for the training data from SMOTE
plot(cbind(smtrain$X1,smtrain$X2),
     col=(3-as.numeric(smtrain$class)),xlab='X1',ylab='X2')
# 데이터가 많이 매꿔진 모습을 확인 가능함

# SVM for data from SMOTE.
smtrain$class = as.factor(smtrain$class)

set.seed(10)
cv.fit3 = tune(svm, class~., data=smtrain, kernel='radial', 
               ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit3)

# SVM with the best parameters
best.fit3 = cv.fit3$best.model

# Prediction for test data
yhat.te3 = predict(best.fit3, test)
cm3 = table(true = test$y, predict=yhat.te3)

misclass(cm3)

fmeasure(cm3)

################# One-class learning ##################
# 거의 outlier를 찾는 것과 같은 방식
# anomarly detection은 굉장히 특수한경우임 (센서 고장 등 탐색)
# 일반적인 상황에는 잘 적용되지 않는다.
# Support vector data description (SVDD) -----------------

# Training data for the large class.
train.x0 = train[train$y == 0,-1]

result = NULL
# tuning parameter nu, gaussian kernel의 gamma에 대하여 
# loop를 돌 수 있도록 설정
for (nu in c(0.001,0.01,0.1,0.3,0.5,0.7,0.9))
{
  for (gamma in c(0.01,0.1,0.5,1,2,3,5))
  {
    svddfit = svm(x=train.x0, type='one-classification', kernel='radial',
                  nu=nu, gamma=gamma)
    
    minor = predict(svddfit,test[,-1])
    # predict: TRUE: small class(outlier), FALSE: large class
    yhat.te = numeric(nrow(test))
    yhat.te[minor==TRUE] = 1
    cm = table(true = test$y, predict=yhat.te)
    result = rbind(result, c(nu,gamma,fmeasure(cm)))
  }
}
# Best result for F-measure.
result
names(result) <- c('nu','gamma','F-measure')
result[which.max(result[,3]),]
# F-measure값이 가장 클 때를 return

############## Cost-sensitive Learning ################

# Class weighted SVM -----------------------------
# svm function using 'class.weight' option

wts = 500 / table(train$y) 
# major class를 잘못 찾았을 때, 
# minor classs를 찾았을 때 cost를 부여하기
# 10배 더 많으니 1 group에 10을 부여한 것.
# cost를 계산할 수 있다면 cost를 여기 넣어도됨.
# 각 관찰값에 부여해야 한다

set.seed(10)
cv.fit = tune(svm, y~., data=train, kernel='radial', 
              class.weights=wts,
              ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100),
                           gamma=c(0.01,0.1,0.5,1,2,3,4)))
summary(cv.fit)

# SVM with the best parameters
best.fit = cv.fit$best.model

# Prediction for test data
yhat.te = predict(best.fit, test)
cm = table(true = test$y, predict=yhat.te)

misclass(cm)

fmeasure(cm)

################# Ensemble-based methods ###################
install.packages("ebmc")
library(ebmc)

# SMOTE Boost -----------------------------------------
set.seed(10)

fit1 = sbo(y~., data=train, size=200, alg='cart', over=300)
# y should be encoded by (0,1); 0 large class, 1 small class
# size: # of boosting iterations, overfitting을 피하기 위해 잘 조절해줘야 한다
# alg: weak learner 
# over: oversampling rate, 100단위로 적어줘야 한다.

yhat.te = predict(fit1, test, type='class')
cm = table(true = test$y, predict=yhat.te)

misclass(cm)
# 0.147
fmeasure(cm)
# 0.330

# SMOTE Bagging --------------------------------------
set.seed(10)
# smote된 데이터에 대하여 여러 트리를 만들어 합치는 방법(변동을 줄이기)

fit2 = sbag(y~., data=train, size=300, alg='cart')
# size : tree 개수
# y should be encoded by (0,1); 0 large class, 1 small class

yhat.te = predict(fit2, test, type='class')
cm = table(true = test$y, predict=yhat.te)

misclass(cm)

fmeasure(cm)

