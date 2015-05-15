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
