## Cross Validation

# Prepare the environment
library (ISLR)
set.seed(1)
train <- sample(392, 196)

# Use the Auto dataset to fit a linear model
# Fix the NA values in horsepower first
Auto[is.na(Auto$horsepower),c("horsepower")] <- mean(Auto$horsepower, na.rm=T)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
# And to fit a polynomial model
lm.fit2 <- lm(mpg[train]~poly(horsepower[train],2),data=Auto)
mean((Auto$mpg[train] - predict(lm.fit2,Auto))[-train]^2)

# Leave one out Cross Validation (LOOCV)
library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
#cv.err <- cv.glm(Auto, glm.fit)
#cv.err$delta
# fit through the fifth degree polynomial
cv.error <- rep(0,5)
for (i in 1:5) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(cv.error)
lines(cv.error)

# k-fold CV
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10
plot(cv.error.10)
lines(cv.error.10)

# The Bootstrap
# This is for the example of the investment selection in section 5.2
# First we create the function that estimates the value of alpha
alpha.fn <- function (data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))
boot(Portfolio, alpha.fn, R=1000)

# Now to estimate the accuracy of a linear regression model
boot.fn <- function (data, index) {
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
set.seed(1)
boot.fn(Auto, sample(392,392,replace=TRUE))
# now run the bootstrap
boot(Auto, boot.fn, 1000)
# compare against the theoretical values
summary(lm(mpg∼horsepower,data=Auto))$coef

# Let's now fit a quadratic model and use the bootstrap
boot.fn <- function (data, index) {
  coefficients(lm(mpg∼horsepower+I(horsepower^2),data=data,subset=index))
}
set.seed(1)
boot(Auto, boot.fn, 1000)
# and compare against the lm calculated SE values
summary(lm(mpg∼horsepower+I(horsepower^2),data=Auto))$coef

# Bootstrap on the Burns dataset
# (http://www.burns-stat.com/documents/tutorials/the-statistical-bootstrap-and-other-resampling-methods-2/)
spxibm <- as.matrix(read.table(
  "http://www.burns-stat.com/pages/Tutor/spx_ibm.txt",
  header=TRUE, sep='\t', row.names=1))
View(spxibm)
# extract the separate indices
spxret <- spxibm[, "spx"]
ibmret <- spxibm[, "ibm"]
# We want to calculate log returns (sum of the returns for the year)
# Here is how we put the bootstrap to use
spx.boot.sum <- numeric(1000)     # create a 1000 long numeric vector
for (i in 1:1000) {               # set up the loop to create 1000 samples
  this.sample <- spxret[ sample(251, 251, replace = TRUE) ]     # select 251 values at random with replacement
  spx.boot.sum[i] <- sum(this.sample)      # calculate the sum of daily returns for this sample
}
# Since we now have 1000 samples for log returns, let's plot the density function
# (or a histogram) for the sample diustribution
# and superimpose the actual value from the original file not sampled
plot(density(spx.boot.sum), lwd=3, col="steelblue")
abline(v = sum(spxret), lwd=3, col="red")
# If I were trying to estimate the ibm return from the spx return using a linear model
# we could bootstrap the value of the linear coefficient (or the intercept for that matter)
# as follows
beta.obs.boot <- numeric(1000)     # create a 1000 long numeric vector
for (i in 1:1000) {               # set up the loop to create 1000 samples
  this.sample <- sample(251, 251, replace = T)
  beta.obs.boot[i] <- coef(lm(ibmret[this.sample]~spxret[this.sample]))[2]
}
# plot the results
plot(density(beta.obs.boot), lwd=3, col="steelblue")
abline(v=coef(lm(ibmret ~ spxret))[2], lwd=3, col='gold')
# Instead of bootstrappint the linear regression coefficient we could bootstrap the
# residuals, as follows
ibm.lm <- lm(ibmret ~ spxret)
ibm.fit <- fitted(ibm.lm)
ibm.resid <- resid(ibm.lm)
beta.resid.boot <- numeric(1000)
for (i in 1:1000) {
  this.sample <- sample(251, 251, replace=TRUE)
  beta.resid.boot[i] <- coef(lm(ibm.fit + ibm.resid[this.sample] ~ spxret))[2]
}
plot(density(beta.resid.boot), lwd=3, col="steelblue")
abline(v=coef(lm(ibmret ~ spxret))[2], lwd=3, col='gold')


