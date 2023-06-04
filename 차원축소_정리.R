############################################
# Statistical Modelling & Machine Learning #
#               R Example3                 #
############################################

options(warn = -1)  # Turn off warning message

# Data: Breast Cancer Wisconsin Data
dat = read.csv("wdbc.csv", header = F)

x.name = c("radius", "texture", "perimeter", "area", "smoothness", 
            "compactness", "concavity", "concave_points", "symmetry", 
            "fractal_dimension")

names(dat) = c("id", "diagnosis", paste0(x.name,"_mean"), 
               paste0(x.name,"_se"), paste0(x.name,"_worst"))

dat = dat[,-1]

head(dat) # x변수 데이터 column 이름 변경

# Principal Component Analysis (PCA) ----------------------------

pr = prcomp(dat[,2:31], center = TRUE, scale = TRUE)
# center : centering
# scale : scaling
# standardization을 하고 진행한다는 뜻

summary(pr) 
# PCA별 std, proportion of variance, cumulative proportion of varinance반환 

# Scree plot
screeplot(pr, type = 'l', npcs = 15, main = 'Scree plot')
# 서로 연결해서, 15개 까지만 그려서 보자.
# elbow형성 확인, Linear correlation이 꽤 있는 것으로 보임
# 4나 7을 elbow point로 볼 수 있음

# Visulalization: Scatter plot matrix

library(lattice)

pc.dat = data.frame(type = dat$diagnosis,pr$x[,1:3])
# pr$x : PC1, PC2, PC3을 사용한다.
# PC1에서 가장 잘 안겹치는 것을 확인할 수 있음
# 두 그룹을 잘 분류해준다고 볼 수 이씅ㅁ

pc.dat$type = as.factor(pc.dat$type)

splom(~pc.dat[,2:4], groups=type, data=pc.dat, panel=panel.superpose)
# PC3개에 대한 scatter plot확인
# "type" 그룹에 대한 시각화. 그룹별 특징 확인 가능

install.packages('car')
library(car)
scatterplotMatrix(~PC1+PC2+PC3|type, data=pc.dat)
# 두 그룹의 분포도 함께 보여주면서, 어떤 PC가 가장 분류에 잘 기여하는지 확인가능
# 항상 먼저 package를 다운받아두자!
# 분류문제에서는 항상 분산이 큰 쪽이 분류에 유리한 영향을 주진 않는다.

# Application to logistic regression:
install.packages('boot')
library(boot)

dat$diagnosis = as.factor(dat$diagnosis)
# as.factor로 변환. categorial 변수

# Comparison of CV statistics:
set.seed(1)
fit = glm(diagnosis~., data=dat, family=binomial)
cv.glm(dat, fit, K=5)$delta
# 0.05982303 0.82816919, cv error의 분산도 엄청 크다.

fit1 = glm(type~., data=pc.dat, family=binomial)
cv.glm(pc.dat, fit1, K=5)$delta
# 0.03728052 0.03671780 더 improved된 결과를 확인 가능.
# cv error의 분산 또한 훨씬 감소하였다.

# Principal Curve ------------------------------------------------
install.packages('princurve')
library(princurve)

# Simple example
set.seed(1)
n=100
x1 = runif(n,-1,1)
x2 = x1^2 + rnorm(n, sd = 0.1)
z = cbind(x1,x2)

cor(x1,x2)
# x1, x3의 correlation은 0.08으로 낮게 나옴
# 2차 관계가 있음에도 불구하고 그런 경향이 나온다(not linear)

fit = principal_curve(z)

plot(x1,x2) # x1, x2 scatterplot
lines(sort(x1),(sort(x1))^2,col='red',lty=2)
# true 관계를 보이는 line추가.

lines(fit,col='blue')
# fitting한 principal curve의 시각화

whiskers(z, fit$s)
# projection line 시각화

# WDBC data application:
# 두 그룹의 density를 principal curve위에서 보기
fit = principal_curve(as.matrix(dat[,2:31]))

# Density of two groups in principal curve and PC1
par(mfrow=c(1,2))
# project된 좌표값들을 두 그룹으로 나눠서, Density로 보는것
plot(density(fit$lambda[dat$diagnosis == 'B']),col='red',
     xlim=c(500,5500),main='Principal Curve')
lines(density(fit$lambda[dat$diagnosis == 'M']),col='blue')

# Principal Curve로 구했을 때의 두 그룹의 Density
plot(density(pc.dat[dat$diagnosis == 'B',2]),col='red',
     xlim=c(-15,7),main='PC1')
lines(density(pc.dat[dat$diagnosis == 'M',2]),col='blue')

# 현재는 PC가 더 좋아 보이는데, 이는 Linear한 관계이기 때문이다.
# Elbow point가 있기 때문에, Linear correlation이 충분하다
# 따라서 굳이 non-linear한 관계를 볼 이유는 없다.
# 이런 순서로 시도해나가면 된다.

# Density for principal curve and PC1
# 두 그룹을 하나로 합쳐서 관계를 보는 것.
# 확실한 두개의 봉우리가 보이는 것으로 보아, PC가 더 좋은 성능을 보이는 상황이다.
par(mfrow=c(1,2))
plot(density(fit$lambda),col='red',main='Principal Curve')
plot(density(pc.dat[,2]),col='red',main='PC1')

dat1 = cbind(dat, pcurve=fit$lambda)
dat1$diagnosis = as.factor(dat1$diagnosis)

fit2 = glm(diagnosis~pcurve, data=dat1, family=binomial)
cv.glm(dat1, fit2, K=5)$delta
# 0.05982303 0.82816919, 썩 좋은 성능이 아니다(개션x)

# 새로운 데이터를 pcurve에 projection하고 싶다면.
# Projection onto Principal curve
new.obs = as.matrix(dat[31:40,2:31])
project_to_curve(new.obs,fit$s)$lambda  
# principal curve상에서의 좌표값을 구할 수 있다.

# arc-length along the curve.

# Kernel PCA ---------------------------------------------------------
install.packages('kernlab')
library(kernlab)

x = dat[,2:31]
fit = kpca(~., data=x, kernel='rbfdot', kpar=list(sigma=3), features=2)
# radial basis kernel function, kpar=list(sigma=3), hyperparameter, features=2
# PC1, PC2만 구하겠다는 뜻
# 적절한 결과가 나올 때 까지 sigma를 바꾸면서 계속 해보는 수 밖에 없다.
# feature: # of PC's

# Kernel PC's
pc = pcv(fit)

# 그룹 분리
B = pc[dat$diagnosis=='B',]
M = pc[dat$diagnosis=='M',]

par(mfrow=c(1,1))
plot(B, col='red', xlab='KPC1',ylab='KPC2')
points(M, col='blue')
# 썩 만족스런 결과가 나오진 않았음
# 하지만 3이 그나마 잴 낫다. 그 이유는 ? PCA로 이미 충분하니까.

# New observations
predict(fit, new.obs)
# PC1, PC2에 새로운 observation을 projection하기


# Non-negative matrix factorization ------------------------------------

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Biobase")
# biostatistics에 많이 쓰이는 패키지 모음 사이트
# CRAN같은 개념
install.packages('NMF')
library(NMF)

?esGolub
# gene expression data

data(esGolub)

# 5000 gene, 38 Samples
dim(esGolub)

# random seed를 주고 (W) 시작하는데, seed고정가능
res = nmf(esGolub, rank = 3, seed=123456)

# W matrix
W = basis(res)
dim(W)
# row가 줄어듬

# H matrix
H = coef(res)
dim(H)
# column이 줄어듬

# rank3이 적절한가? 2:6까지 계속 시도해서 안정적으로 나오는지
# 확인하는 절차.
if(requireNamespace("Biobase", quietly=TRUE))
{
  estim.r = nmf(esGolub, 2:6, nrun=10, seed=123456)
  plot(estim.r)
}
# 5로 결정, cophenetic이 가장 낮게 나오는 숫자로 선택

res = nmf(esGolub, rank = 5, seed=123456)
# Visualization
basismap(res, subsetRow=TRUE)
#  gene pattern을5개 group으로 분리해서 확인 가능
coefmap(res)
# gene들의 pattern을 cluster하여 시각화 가능

# Independent component analysis -----------------------------------

install.packages('fastICA')
library(fastICA)
# 독립적 Source를 찾기
# Ex1:

S = matrix(runif(10000), 5000, 2) # 독립적 random number
A = matrix(c(1, 1, -1, 3), 2, 2, byrow = TRUE)
X = S %*% A
# source 2개, algorithm 설정, 나머지는 모두 method관련된것
# max iteration 수 결정, 수렴 옵션(tol)
a = fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)
par(mfrow = c(1, 3))
plot(a$X, main = "Pre-processed data") # A가 곱해져서 S가 휘어진 형태
plot(a$X %*% a$K, main = "PCA components") # 제대로 분리 불가능
plot(a$S, main = "ICA components") # 원래 Source를 recover


# Ex2:

S = cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
A = matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
X = S %*% A
a = fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

par(mfcol = c(2, 3))
plot(1:1000, S[,1], type = "l", main = "Original Signals",
     xlab = "", ylab = "")
plot(1:1000, S[,2], type = "l", xlab = "", ylab = "")
plot(1:1000, X[,1], type = "l", main = "Mixed Signals",
     xlab = "", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "", ylab = "")
plot(1:1000, a$S[,1], type = "l", main = "ICA source estimates",
     xlab = "", ylab = "")
plot(1:1000, a$S[, 2], type = "l", xlab = "", ylab = "")
# 이것도 예제에서 나온 그림과 똑같은것
# 이건 답이 정해져 있는 케이스라서 잘 나온거고
# 실제로는 잘 안나올 경우가 많다. 특수한 dataset에서만 잘 작동하는 경우가 대부분.

