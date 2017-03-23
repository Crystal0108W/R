# Collinearity - Highly Correlated Data
# Ask: 
# which variables are most predictive of online spending? 
# If we wished to increase online spending by customers, which factors might we consider?

cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

# 1.1. Initial Linear Model of Online Spend
spend.m1 <- lm(online.spend ~ ., 
               data = subset(cust.df[, -1], online.spend > 0))
summary(spend.m1)
# online.trans and online.visits are closely related with each other
# But the estimates for those two variables are drastically different. 

# 1.2. Visulize the plot to spot correlation 
install.packages("gpairs")
library(gpairs)
gpairs(cust.df)
# It shows variables with extreme skew and pairs of variables that are very highly correlated.

# 2.2. Tackle Correlation: transform the data using a Box-Cox transformation
install.packages("forecast")
autoTransform <- function(x) {
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[, numcols] <- lapply(cust.df.bc[, numcols], autoTransform)
#cust.df.bc is a standardized, more normally distributed vlues
gpairs(cust.df.bc)
