###### variable selection #########
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
bs
which.max(bs$adjr2) # 2
which.min(bs$rss) # 3
which.min(bs$bic) # 2
# mag, dist를 변수로 선택
# method = 'Forward', 'Backward'로만 변경하면 다르게 사용가능

options(warn = -1)  # Turn off warning message

######### Variable Importance: Regression problem ##########

# Building data
# 90 economic variables and sales variable (output)
dat = read.table('building.csv', sep=',', header=T)
head(dat)
describe(dat) # 모두 연속형 변수
summary(dat)
# Correlation coefficient ---------------------------
VI = cor(dat)[,'price'] # price에 대한 correlation을 계산
SVI = sort(abs(VI), decreasing = T)[-1] #절댓값으로 계산
SVI 
# Pearson correlation 순으로 볼 수 있음, 절댓값을 씌우고 봐야 한다.
par(mfrow=c(1,1))
plot(1:length(SVI),SVI, type='b', ylab='Size of correlation',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SVI), labels=names(SVI), cex.axis=0.3,las=2)
# 비슷비슷한 값을 나타내는 변수가 많고, 관계가 적어보이는 변수는 상당히 뒷부분부터 확인 가능하다.

dat[,names(SVI[SVI>0.5])] # 이런식으로 Filtering을 하면 된다

# Spearman rank correlation coefficient -------------
SP = cor(dat, method='spearman')[,'price']
# 좀 더 Curve-linear한 관계의 수준 또한 고려한 방식이다.
SPI = sort(abs(SP), decreasing = T)[-1]
SPI

plot(1:length(SPI),SPI, type='b', ylab='Size of correlation',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SPI), labels=names(SPI), cex.axis=0.3,las=2)
# 마찬가지로 비슷비슷한 값을 나타내는 변수가 많고, 관계가 적어보이는 변수는 상당히 뒷부분부터 확인 가능하다.


# Pseudo R^2 ----------------------------------------
# non-parametric model을 사용해야 한다.
p = 90 
PR2 = numeric(p)
names(PR2) = colnames(dat[,-91])
PR2

for (j in 1:p)
{
  fit = loess(price ~ dat[,j], data=dat)  # Local linear regression, kernel로 weight를 준 형태
  yhat = predict(fit, dat[,j])
  PR2[j] = 1-(sum((dat$price - yhat)^2)/sum((dat$price - mean(dat$price))^2))
  # 각각의 변수들을 하나씩 넣어서, local linear regression을 진행하고
  # 그 때의 r^2값 같은 것을 구해서 크기 순으로 나열한것.
}

SPR2 = sort(PR2, decreasing = T)
SPR2

plot(1:length(SPR2),SPR2, type='b', ylab='Pseudo R2',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SPR2), labels=names(SPR2), cex.axis=0.3,las=2)
# 다양한 measure를 통해서, screening을 하면 최대한 여러 부분을 고려해서 filtering을 진행할 수 있다.
# filtering을 이렇게 진행하면, 걸러진 변수들 중에 "조합"이 영향을 미치는 경우가 있을 수도 있다.
# 근데 걸러버리면 다시 돌아오지 못함.

# Maximal information coefficient (MIC) -------------
# 선형 / 비선형 관계를 모두 고려하는 방식
install.packages('minerva')
library(minerva)

MIC = mine(dat)
MIC = MIC$MIC[,'price'] # 'PRICE'에 대해 본 것만 확인

SMIC = sort(MIC, decreasing = T)[-1]
SMIC

# 비슷한, Marginal한 effect 패턴이 확인됨.
plot(1:length(SMIC),SMIC, type='b', ylab='MIC',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SMIC), labels=names(SMIC), cex.axis=0.3,las=2)

######### Variable Importance: Classification problem ##########

# Data
install.packages('mlbench')
library(mlbench)
data(BreastCancer)
dat = BreastCancer[,-1]

# Relief algorithm ------------------------------
# Y변수가 categorical할 때, classification에 영향을 미치는 변수들을
# 거리 기반으로 선택하는 방법.
install.packages('CORElearn')
library(CORElearn)
# 반드시 시행 전에 연속형 X변수에 대해서 [0, 1]로 scaling하고 진행해야 한다.

# Relief algorithm
RE = attrEval(Class ~ ., data=dat, estimator='Relief',
              ReliefIterations=30)

SRE = sort(RE, decreasing = T)
SRE

plot(1:length(SRE),SRE, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SRE), labels=names(SRE), cex.axis=0.8,las=2)


# ReliefF algorithm
# 좀 더 stable한 결과를 기대할 수 있음
REF = attrEval(Class ~ ., data=dat, estimator='ReliefFequalK',
               ReliefIterations=30)

SREF = sort(REF, decreasing = T)
SREF

plot(1:length(SREF),SREF, type='b', ylab='Separability',
     xlab ='variables', main='Variable Importance', xaxt='n')
axis(side=1, at=1:length(SREF), labels=names(SREF), cex.axis=0.8,las=2)


########## Variable Selection: Simulated Annealing ##########
# 그닥 효율적이지 않음(비추)
install.packages('mvtnorm')
library(mvtnorm)

# Data generation
set.seed(10)

n = 500
p = 20
S = matrix(0.3, nrow=p, ncol=p)
diag(S) = 1
X = rmvnorm(n, mean=rep(0,p), sigma=S)

XN = NULL
for (j in 1:p) XN = c(XN,paste('X',j,sep=''))
colnames(X) = XN

Y = 2 + 0.5*X[,1] - 0.3*X[,2] + 1.2*X[,3] + rnorm(n,sd=0.1)
# True모델에 X1, X2, X3만 사용해서 만들어씀

# Simulated Annealing ---------------------------

# X1, X2, X3을 잘 찾는가?
# 20^20조합을 에서 최적을 찾는 것이니 당연히 오래 걸린다.
install.packages('caret')
library(caret)

ctrl = safsControl(functions=caretSA, method='cv', number=5)
# 속도의 향상을 위해선 AIC, BIC같은(분포가정이 있는 모델을 사용할 때)걸 사용할 수 있다.

obj = safs(x=X, y=Y, iters=20, safsControl=ctrl, method='lm')
# method : 원하는 걸 넣어주면 됨. 어떤 모델이든 간에 가능하다.
# 단 평가지표는 모델에 따라 달라질 수 있다. 그리고 모델마다 시간의 차이가 존재하게 된다.

obj 
# 14개를 유효하다고 판단했음.
#################### ISIS ####################
# Linear model에 모두 적용가능한 방식.
install.packages('SIS')
library(SIS)

?SIS

# Data generation
set.seed(0)
n = 400; p = 50; rho = 0.5
corrmat = diag(rep(1-rho, p)) + matrix(rho, p, p)
corrmat[,4] = sqrt(rho)
corrmat[4, ] = sqrt(rho)
corrmat[4,4] = 1
corrmat[,5] = 0
corrmat[5, ] = 0
corrmat[5,5] = 1
cholmat = chol(corrmat)
x = matrix(rnorm(n*p, mean=0, sd=1), n, p)
x = x%*%cholmat

# Linear regression
set.seed(1)
b = c(4,4,4,-6*sqrt(2),4/3)
y=x[, 1:5]%*%b + rnorm(n)
# 6:50까지는 y에 변수를 미치지 않는 변수의 형태로 만들었음.
# family : guassian = linear regression
# binomial = logistic regression
# poisson, cox = poisson regression

# ISIS with regularization
# tune : lasso, ridge등의 tuning parameter를 무엇으로 찾을지 결정.
model11=SIS(x, y, family='gaussian', tune='bic')
model11$ix
# 바로 1, 2, 3, 4, 5를 선택

# aggregation methods의 사용
model12=SIS(x, y, family='gaussian', tune='bic', varISIS='aggr', seed=11)
model12$ix
# 바로 1, 2, 3, 4, 5를 선택


# logistic regression
set.seed(2)
feta = x[, 1:5]%*%b; fprob = exp(feta)/(1+exp(feta))
y = rbinom(n, 1, fprob) 

# ISIS with regularization
model21=SIS(x, y, family='binomial', tune='bic', penalty='SCAD', perm=T, q=0.9)
model21$ix
# penalty : SCAD(default), MCP, lasso 셋 중 선택 가능함.

# aggregation methods의 사용
model22=SIS(x, y, family='binomial', tune='bic', varISIS='aggr', seed=21)
model22$ix

# 해석을 놓치면 안되고 원래 스케일을 유지한 채 모델링을 진행한다 -> 변수선택
# 해석보다는 예측이 중요하고 차원을 축소하는게 효과적인 상황이다 -> 차원축소
# 둘 중 하나를 골라서 진행해야 한다.
# 그리고 SIS의 경우 linear model에만 사용 가능하다는 것을 꼭 기억할 것
# Y변수의 분포에 맞춰서 사용하면 된다. gaussian, binomial, poisson, cox 넷 중 하나를 선택



