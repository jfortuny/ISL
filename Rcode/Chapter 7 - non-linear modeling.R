# Non-linear modeling
library(ISLR)
attach(Wage)
View(Wage)

# 7.8.1 - polynomial regression and step functions
fit <- lm(wage ~ poly(age,4), data=Wage)
coef(summary(fit))

fit2 <- lm(wage ~ poly(age,4, raw=T), data=Wage)
coef(summary(fit2))

# to predict
agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=T)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
# now we can plot
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=0.5, col="darkgrey")
title("Degree 4 polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", tly=3)

# select the degree of the polynomial to fit
fit.1=lm(wage∼age,data=Wage)
fit.2=lm(wage∼poly(age,2),data=Wage)
fit.3=lm(wage∼poly(age,3),data=Wage)
fit.4=lm(wage∼poly(age,4),data=Wage)
fit.5=lm(wage∼poly(age,5),data=Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)

# Does an individual earn more than $250K per year?
fit <- glm(I(wage>250) ~ poly(age,4), data=Wage, family="binomial")
preds <- predict(fit, newdata=list(age=age.grid), se=T)
pfit <- exp(preds$fit)/(1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
plot(age, I(wage>250), xlim=agelims, type="n", ylim=c(0,0.2))
points(jitter(age), I((wage>250)/5), cex=0.5, pch="|", col="darkgrey")
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="red", lty=3)


# 7.8.2 - Splines
library(splines)

# Fit a cubic spline
fit <- lm(wage ~ bs(age, knots=c(25,40,60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2*pred$se, lty="dashed", lwd=2)
lines(age.grid, pred$fit - 2*pred$se, lty="dashed", lwd=2)

# Fit a natural spline
fit2 <- lm(wage ~ ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, lwd=2, col="red")

# Fit s smootihing spline
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
fit2$df
legend("topright",legend=c("16 DF","6.8 DF"),
       col=c("red "," blue "),lty =1, lwd =2, cex =.8)

# Fit local regression
plot(age ,wage ,xlim=agelims ,cex =.5, col ="darkgrey")
title ("Local Regression")
fit=loess(wage∼age,span=.2,data=Wage)
fit2=loess(wage∼age,span=.5,data=Wage)
lines(age.grid ,predict (fit ,data.frame(age=age.grid)),
      col ="red ",lwd =2)
lines(age.grid ,predict (fit2 ,data.frame(age=age.grid)),
      col =" blue",lwd =2)
legend ("topright",legend =c("Span =0.2" ," Span =0.5") ,
        col=c("red "," blue "),lty =1, lwd =2, cex =.8)

# 7.8.3 GAMs
library(gam)
library(akima)
# Predict wage as a function of year, age and education (qualitative predictor)
# and use natural splines for the continuous predictors
gam1 <- lm(wage ~ ns(year,4) + ns(age,5) + education, data=Wage)

# Using the gam library
gam.m3=gam(wage∼s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col="blue")
plot.gam(gam1, se=TRUE, col="red")
# Now determine which of these three models is best: a GAM that excludes year (M1), a
# GAM that uses a linear function of year (M2), or a GAM that uses a spline function
# of year (M3)
# The S() function in GAM is used to create smooth splines
gam.m1=gam(wage∼s(age ,5) + education ,data=Wage)
gam.m2=gam(wage∼year+s(age ,5)+education ,data=Wage)
anova(gam.m1 ,gam.m2 ,gam.m3,test="F")
summary(gam.m3)
summary(gam.m2)
# to predict
preds <- predict(gam.m2, newdata=Wage)

# Using local regression as part of the GAM; the lo() function is used as a member of the GAM
gam.lo <- gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education, data=Wage)
plot.gam(gam.lo, se=TRUE, col="green")
# We can also use the lo() function to create interactions before calling GAM
gam.lo.i <- gam(wage∼lo(year, age, span=0.5) + education, data=Wage)
plot(gam.lo.i)
