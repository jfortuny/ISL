# Basic regression plot
plot(Boston$lstat,Boston$medv, pch="+")
abline(lm.fit)
# abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

# plot(Boston$lstat,Boston$medv,col="red")
# plot(Boston$lstat,Boston$medv, pch=20)
# plot(Boston$lstat,Boston$medv, pch="+")
# plot(1:20,1:20,pch=1:20)

# Plot regression line and residuals
# placing multiple plots on a page
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))