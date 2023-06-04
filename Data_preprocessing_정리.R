############################################
# Statistical Modelling & Machine Learning #
#               R Example2                 #
############################################

#################
# Missing value #
#################

# mice package: Multivariate imputation by chained equation.
install.packages('mice')
library(mice)

install.packages('rms')
library(rms)

install.packages('finalfit')
library(finalfit)

# Data
head(nhanes)
# nhanes[is.na(nhanes)] <- NA 
# 결측값이 NA로 표기되어 있지 않다면 이렇게 진행하면 될듯하다.
nhanes
summary(nhanes) # 범주형변수 : age, hyp, 연속형 변수 : bmi, chl
nhanes$age = as.factor(nhanes$age) # as.factor적용은 바로바로 해두자
nhanes$hyp = as.factor(nhanes$hyp) 

# Description of data
# 범주형 변수의 비율도 확인 가능하다.
describe(nhanes)

# Missing pattern
md.pattern(nhanes) # missing pattern확인.
nhanes 
# bmi, hyp, chl이 동시에 없는 경우 7개
# hyp, bmi가 없는 경우 1개
# bmi만 없는 경우 1개
# chl만 없는 경우 3개
# hyp 8개, bmi 9개, chl 10개 결측치 존재

missing_pairs(nhanes,'chl',c('age','bmi','hyp') )
# missing variable의 pattern 확인. 패턴이 비슷하면 MCAR나 MAR로 생각할 수 있다
# 평균값이 다르다면 패턴을 고려한 모델링이 필요하다.

# Missing pattern between variables 
# (rr: both observed, mm: both missing, 
#  mr: row variable is missing & column variable is observed)
md.pairs(nhanes)

missing.clus = naclus(nhanes,method='average')
missing.clus
plot(missing.clus)
# 결측값 clustering. bmi는 hyp와 비슷한 패턴을 띄고 그 다음이 chl이다.
# 같은 cluster내부에 있으면 imputation에 활용하지 않는다.


# Check missing pattern of Y variable. logistic regression으로 할 수 있다.
fit = glm(is.na(chl) ~ age + bmi + hyp, data=nhanes, family=binomial )
summary(fit)
# missing value가 Y와의 연관성이 있는지 확인한다. 위 경우는 Y와의 연관성이 나타나지 않았다.
# missing=T, non-missing=F으로 설정하고 실행하면 한다.
# 통계적으로 missing되는 패턴이 발견되는지 확인한다.
# 왜 missing이 있는지 패턴을 확인하고 생긴 이유를 확인해보는 과정이 필요하다.

# MICE
?mice

# Missing values should be coded as NA.
# m: The number of imputed datasets
set.seed(0) # 재현성을 위해서 나중에 코드를 제출해야 할 때 꼭 적용해 둘 것
imp = mice(nhanes, m=10, method=c('','pmm','rf','pmm'), print=F)
# predictors for imputation of each column
imp$predictorMatrix

# 만약에 특정 변수를 imputation에 사용하고 싶지 않다면.
pred = imp$predictorMatrix
pred[,'chl'] = 0
pred

imp1 = mice(nhanes, m=10, method=c('','pmm','rf','pmm'), 
            predictorMatrix = pred, print=F)

imp1$predictorMatrix

# list the actual imputations for BMI
imp$imp

# The first imputed datas et
complete(imp,1)
# chl은 다시 NA가 된다.
# The second imputed dataset
complete(imp,2)

# Checking the convergence of MICE.
plot(imp, c('bmi','hyp','chl'))
# 계속 서로 교차하는 형태가 도출되어야 한다.
# 수렴이 확인되지 않으면, model을 변경하는 등 다양한 시도를 해야한다.

# Compare the imputed data and observed data.
stripplot(imp, pch=20, cex=1.2)
# blue point: observed, red point: imputed

xyplot(imp,chl ~ bmi |.imp)
# The first plot: original complete set.

densityplot(imp, scales=list(relation='free'),layout=c(2,1))
# blue line: density for observed data, red line: density for imputed data


# Prediction model with MICE
# Goal: predict chl based on age, hyp, bmi variables
set.seed(0)

imp = mice(nhanes, m=10, method=c('','pmm','rf','pmm'), print=F)
# y변수를 사용해서 imputation진행. chl에도 많은 missing이 존재하기 때문에

# In mice, all variables with missing values should be imputed, 
# even if it is Y variable. 

# To delete obs with missing Y value, imputed Y is replaced with NA.
md = dim(imp$imp$chl)
md
iy = as.data.frame(matrix(NA,md[1],md[2]))
colnames(iy) = colnames(imp$imp$chl)
rownames(iy) = rownames(imp$imp$chl)
imp$imp$chl = iy
imp$imp # imputed된 y를 10x10크기의 dataframe으로 대체해서 제외시킨다.
# imp$imp$chl은 데이터프레임 형태로 제공되니 꼭 기억해둘 것

# Apply prediction model to each imputed dataset.
# E.g., prediction model => linear regression model

fit = with(imp, lm(chl ~ age + bmi + hyp))
# 이렇게 그냥 넣으면 내장 함수들은 알아서 y에 NA가 있으면 제외하고 모델링을 한다.
# 실제로 모델링을 진행하면 다른 결과가 나오니 조심해야 한다.

# 최종적으로 imputed dataset으로 만든 모델을 averaging한다.
summary(pool(fit))

# y변수를 사용해서 imputation을 진행했기 때문에
# imputation을 진행해서 유의해 진 것인지 아닌지,
# 원래 데이터셋으로 fitting을 하고 유효성을 확인해야 한다.
comp.dat = na.omit(nhanes) # completed dataset ~ na.omit
fit1 = lm(chl ~ age + bmi + hyp, data=comp.dat)
summary(fit1)


# Predict test obs.
M = imp$m
imp.dat = vector(mode='list',length=M)
for (m in 1:M) imp.dat[[m]] = complete(imp,m)

p.model = function(dat) lm(chl ~ age + bmi + hyp, data=dat)

fit.imp = lapply(imp.dat, p.model) # 10개의 예측 모델
fit.imp # 10개의 모델이 리스트 형태로 저장되어있는 상태

test.obs = data.frame(age=c(2,1), bmi = c(23.3,21.5), hyp=c(1,2))
# test dataset 을 직접 형성해야 하는데
# numeric 형태로 지금 제공되고 있기 때문에 numeric을 사용..

yhat = lapply(fit.imp, predict, newdata=test.obs)
yhat
yhat = matrix(unlist(yhat),nrow(test.obs),M)
apply(yhat,1,mean)

#######################
# Data transformation #
#######################
# missing value를 채워주고 난 다음에 사용한다.

install.packages('rms')
library(rms)
library(e1071)

getHdata(titanic3)
dat = titanic3[,c('survived','pclass','age','sex','sibsp','parch')]
describe(dat)
# distinct : 범주형 변수 변수 개수
md.pattern(dat)
# age만 missing이 있는 상황

imp = mice(dat,m=1,method='pmm')
imp.dat = complete(imp) # imputed dataset의 평균으로 대체

par(mfrow=c(1,3))
for (j in c('age','sibsp','parch')) 
{
  hist(imp.dat[,j], main=j, xlab = skewness(imp.dat[,j]))  
}
# skewness의 확인 : "skewness"함수의 사용 
# 연속형 변수들의 skewness까지 고려해서 histogram을 확인
# skewness >> 0, 좌로 매우 치우침
# Standardization or centering: Use 'scale()' function.
scale(imp.dat$age) # standardization, centering
par(mfrow=c(1,3))
for (j in c('age','sibsp','parch')) 
{
  hist(imp.dat[,j], main=j,xlab = skewness(imp.dat[,j]))  
}

# Yeo-Johnson transformation
install.packages('bestNormalize')
library(bestNormalize)

imp.dat1 = imp.dat
imp.dat1$sibsp = yeojohnson(imp.dat$sibsp)$x.t
imp.dat1$parch = yeojohnson(imp.dat$parch)$x.t

par(mfrow=c(1,2))
for (j in c('sibsp','parch')) 
{
  hist(imp.dat1[,j], main=j,xlab = skewness(imp.dat1[,j]))  
}
# skewness가 많이 완화된 모습을 확인 가능. 데이터에 적용

# Discretization of continuous variable
# parents숫자, 자식 숫자같은 경우 0이 굉장히 많음
# 차라리 binary형태로 바꾸는게 더 이득일지도.
# 왠만하면 안시키는게 좋은데, 분석의 목적에 따라 적용 가능
imp.dat2 = transform(imp.dat,
    agec = ifelse(age < 21, 'child','adult'),
    sibsp = ifelse(sibsp==0, 'no sibsp','sibsp'),
    parch = ifelse(parch==0, 'no parch','parch'))

##########################################################
# log, exp transformation의 경우 : log(), exp()를 통해서 쉽게 적용 가능
# count형 변수에는 적용하지 못하며(0이 너무 많음) yeo-johnson transformation을 적용해야 한다
# 예측변수에 적용하는건 쉽지만 종속변수에는 적용하면 되돌리기가 불가능하니까 쓰지말자