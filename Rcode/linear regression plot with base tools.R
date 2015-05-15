# Basic regression plot
library(MASS)
plot(Boston$lstat,Boston$medv, pch="+")
lm.fit <- lm(medv ~lstat, data=Boston)
abline(lm.fit)
# abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

# plot(Boston$lstat,Boston$medv,col="red")
# plot(Boston$lstat,Boston$medv, pch=20)
# plot(Boston$lstat,Boston$medv, pch="+")
# plot(1:20,1:20,pch=1:20)

# Plot regression line and residuals
# placing multiple plots on a page
library(MASS)
lm.fit <- lm(medv ~lstat, data=Boston)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval= "confidence")
# graphs
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple regression
library(MASS)
lm.fit2 <- lm(medv ~lstat+age, data=Boston)
summary(lm.fit2)
lm.fit3 <- lm(medv ~ ., data=Boston)
summary(lm.fit3)
# remove non-significant predictors from lm.fit3
lm.fit4 <- update(lm.fit3, ~ .-age-indus)
summary(lm.fit4)
# graphs
par(mfrow=c(2,2))
plot(lm.fit4)

# Nonlinearity and Interactions
# Interaction
lm.fit5 <- lm(medv~lstat*age, Boston)
summary(lm.fit5)
# Non linear term
lm.fit6 <- lm(medv~lstat + I(lstat^2), data=Boston)
summary(lm.fit6)
# graphs
par(mfrow=c(1,1))
plot(Boston$medv~Boston$lstat)
points(Boston$lstat, fitted(lm.fit6), col="red", pch=20)
# polynomial plot
lm.fit7 <- lm(medv ~ poly(lstat,4), data=Boston)
points(Boston$lstat, fitted(lm.fit7), col="blue", pch=20)

# Qualitative predictors
library(ISLR)
View(Carseats)
lm.car1 <- lm(Sales ~ . + Income:Advertising + Age:Price, data=Carseats)
summary(lm.car1)
contrasts(Carseats$ShelveLoc)

