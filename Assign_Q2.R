# Q2 모범답안 구현
train = read.csv('pm25_tr.csv')
test = read.csv('pm25_te.csv')

library('mice')
library('finalfit')
library('rms')
describe(train)
train$cbwd = as.factor(train$cbwd)
train$month = as.factor(train$month)
train$day = as.factor(train$day)

# hour는 연속형 변수로 취급
describe(train)
train$year = NULL
head(train)
par(mfrow=c(1,1))
library(corrplot) # PRES와 TEMP가 높은 correlation을 보여줌
head(train)
corrplot(cor(train[,c(3,4,5,6,7,9)]))

par(mfrow=c(1,5))
# histogram을 통한 변수들의 분포 확인
for (i in c("pm25","DEWP","TEMP","PRES","Iws")){
  hist(train[,i], xlab=i)
}
train$pm25 = log(train$pm25)
train$Iws = log(train$Iws)
# 똑같이 테스트 데이터 전처리
x.te = test[,-5]
y.te = test[5]

x.te$month = as.factor(x.te$month)
x.te$cbwd = as.factor(x.te$cbwd)
x.te$day = as.factor(x.te$day)
x.te$Iws = log(x.te$Iws)
y.te = log(y.te)

for (i in c("pm25","DEWP","TEMP","PRES","Iws")){
  hist(train[,i], xlab=i)
}

fit_lm = lm(pm25~., data=x.tr)
summary(fit_lm)
# 북서풍, 남서풍에 의한 미세먼지 농도 변화 유의하게 있는듯 함
# month의 경우도 상관관계가 존재, 3월 > 4월 > 5월
library(gam)
library(car)
fit_gam = gam(pm25 ~ s(hour,5)+s(DEWP,5)+s(TEMP,5)+s(Iws,5)+s(PRES,5)+cbwd+month+day, data=train)
summary(fit_gam)
par(mfrow=c(2, 5))
plot(fit_gam)
# 모든 변수가 유효하다고 나옴
yhat = predict(fit_gam, x.te)
class(yhat)
r = (exp(yhat) - exp(y.te))[[1]]
mse = sum(r^2)/120
mse # 4138.284
plot(yhat, r)
lines(c(0,120), c(0,0), col='red')
# mean을 잡지 못함. 

par(mfrow=c(1, 5))
for (i in c("hour","Iws","DEWP","PRES","TEMP")){
  plot(train$month, train[,i], xlab = i)
}

fit_gam_1 = gam(pm25 ~ s(DEWP,5)+s(TEMP,5)+s(PRES,5)+cbwd+month*DEWP+month*TEMP+month*Iws+month*PRES, data=train)
summary(fit_gam_1)
par(mfrow=c(2,5))
plot(fit_gam_1)
# categorial 변수를 교차항으로 합치고, day변수를 제거한 뒤 
# gam의 결과에 따라 무의미한 변수 제거
# test mse의 확인
yhat = predict(fit_gam_1, x.te)
r = (exp(yhat) - exp(y.te))[[1]]
mse = sum(r^2)/120
mse # 1752.644
plot(yhat, r)
lines(c(0,120), c(0,0), col='red')
# mean이 잡힌 모습 화깅ㄴ

fit_gam_2 = gam(pm25 ~ s(DEWP,3)+s(TEMP,5)+s(PRES,3)+cbwd+month*DEWP+month*TEMP+month*Iws+month*PRES, data=train)
par(mfrow=c(2,5))
plot(fit_gam_2)
summary(fit_gam2)
anova(fit_gam_1, fit_gam_2)

yhat = predict(fit_gam_2, x.te)
r = (exp(yhat) - exp(y.te))[[1]]
mse = sum(r^2)/120
mse # 1745.36
plot(yhat, r)
lines(c(0,120), c(0,0), col='red')
# mean이 어느정도 잡힌 모습 확인

# AR모델을 통한 잔차 fitting
library(astsa)
acf2(residuals(fit_gam_2))
ar1 = sarima(residuals(fit_gam_2), 1,0,0, no.constant=T)   #AR(1)
# ACF정상 확인
ar1$fit
