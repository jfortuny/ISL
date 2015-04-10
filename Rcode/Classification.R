## Set up the basic environment and data
library(ISLR)
View(Default)
str(Default)
summary(Default)

# Are students the only defaulters?
table(Default$default, Default$student, useNA = "ifany")
# A better table
library(gmodels)
CrossTable(Default$default, Default$student, prop.chisq=FALSE)
