# Basic regression plot
library(ggplot2)

intercept <- coefficients(lm.fit)[1]
slope <- coefficients(lm.fit)[2]
ggplot(data=Boston, aes(x=lstat,y=medv)) + geom_point() + 
  geom_abline(intercept = intercept, slope = slope, colour="red", size=2)

# 
# 
# abline(lm.fit)
# # abline(lm.fit,lwd=3)
# abline(lm.fit,lwd=3,col="red")