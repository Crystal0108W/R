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

# 2.1. Tackle Correlation: transform the data using a Box-Cox transformation
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

# 2.2. Fit the model using the transformed data:
spend.m2 <- lm(online.spend ~ ., data = cust.df.bc)
summary(spend.m2) # Transformin gand standardizing the data have not changed the unbelievable estimate that online spend is highly related to tranactions yet unrelated to visits.
spend.m3 <- lm(online.spend ~ online.trans, data = cust.df.bc)
anova(spend.m3, spend.m2) # High p-value indicated that null-hypothesis is not rejected and there is no difference between model 2 and model 3

# 2.3 [PROBLEM: Collinearity] - degree of collinearity: VIF
# A linear model assumes that effects are additive. 
# An effect attributed to one variable (such as transactions) is not available in the model to be attributed jointly to another that is highly correlated (visits)
# This will cause the stan- dard errors of the predictors to increase, which means that the coefficient estimates will be highly uncertain or unstable.


#3.3 Remediating Collinearity
install.packages("car")
library(car)
vif(spend.m2) # A common rule of thumb is that VIF > 5.0(10.0) indicates the need to mitigate collinearity.

# STRATEGY 1: Omit variables that are highly correlated. 
# STRATEGY 2: Eliminate correlation by extracting principal components or factors for sets of highly correlated predictors.
# STRATEGY 3: Use a method that is robust to collinearity i.e., something other than traditional linear modeling.[such as randome forest]


# STRATEGY 1: Omit variables that are highly correlated. 
spend.m4 <- lm(online.spend~. -online.trans -store.trans, data = cust.df.bc)
vif(spend.m4)
summary(spend.m4)

# STRATEGY 2: Principal Components 
# principal components are uncorrelated (orthogonal). Thus, PCA provides a way to extract composite variables that are guaranteed to be free of collinearity with other variables that are included in the same PCA.
pc.online <- prcomp(cust.df.bc[, c("online.visits", "online.trans")])
cust.df.bc$online <- pc.online$x[, 1] # extract the first component for the online variables
pc.store <- prcomp(cust.df.bc[, c("store.trans", "store.spend")])
cust.df.bc$store <- pc.store$x[, 1] # extract the first component for the store variables
#fit the new model: 
spend.m5 <- lm(online.spend ~ email + age + credit.score + 
                 distance.to.store + sat.service + 
                 sat.selection + online + store,
               data = cust.df.bc)
summary(spend.m5)
# CAUTION: when interpreting results that use principal components as explanatory variables, 
#          the components have arbitrary numerical direction; the negative coefficient for online here does not imply that online activity results in lower sales.
vif(spend.m5) # Check vif
