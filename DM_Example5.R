####################################
# Statistical Data Mining Example5 #
####################################

library(ISLR)
attach(Wage)

#########################
# Polynomial Regression #
#########################

fit=lm(wage~ age + I(age^2) + I(age^3),data=Wage)
coef(summary(fit))

agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

######################
# Regression Splines #
######################

library(splines)

fit = lm(wage~bs(age,knots=c(25,40,60)), data=Wage)
pred = predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lty="dashed")
lines(age.grid, pred$fit-2*pred$se, lty="dashed")

dim(bs(age,knots=c(25,40,60)))	
# =>  4 regions * 4 parameters - 3 knots * 3 constraints 
#	= 7 (intercept + 6 basis functions)

dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

###################
# Natrual Splines #
###################

fit2 = lm(wage~ns(age,df=4),data=Wage)

attr(ns(age,df=4),"knots")
attr(ns(age,knots=c(25,50,75)),"knots")

pred2 = predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)

#####################
# Smoothing Splines #
#####################

fit=smooth.spline(age, wage, df=16)
fit2=smooth.spline(age, wage, cv=TRUE)
fit2$df

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

####################
# Local Regression #
####################

fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

###############################
# Generalized Additive Models #
###############################

# Estimation of fj(Xj): Natural Spline
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)

# Estimation of fj(Xj): Smoothing Spline

install.packages('gam')
library(gam)

gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")

# F-test
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")

# Prediction
preds=predict(gam.m2,newdata=Wage)


