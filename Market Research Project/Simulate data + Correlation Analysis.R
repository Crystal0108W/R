############ Market Analysis Project ############ 
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("car")
install.packages("gpairs")
install.packages("corrplot")
install.packages("gplots")
install.packages("psych")

library(gridExtra)
library(ggplot2)
library(car)
library(gpairs)
library(corrplot)
library(gplots)
library(psych)
#1. Simulate a data set that describes

#1.1 Simulate Customer Data
# 2000 Customers age, 
# 2000 Customers age credit in store 
# 2000 Customers email (yes or no) 
# 2000 Customers distance to store (use lognormal distribution to simulate distance)
set.seed(1234)
ncust <- 2000
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))

cust.df$age <- rnorm(n=ncust, mean = 35, sd = 5)
cust.df$credit.score <- rnorm(n=ncust, mean = 3*cust.df$age + 500, sd = 50)
cust.df$email <- factor(sample(c("yes","no"), size = ncust, replace = TRUE, prob = c(0.7, 0.3)))
cust.df$distance <- exp(rnorm(n=ncust, mean = 2, sd = 1.2)) 
summary(cust.df)


#1.2 Simulate Online and In-Store Sales Data
# Online visits (negative binomial distribution to simulate the counts)
# tranaction 
# totle spendings

# model the mean of the negative binomial with a baseline value of 20.
# The size argument sets the degree of dispersion (variation) for the samples. 
# For customers with email on file, an average 15 more online visits are added
# Lastly, assume that younger customers tend to visit online store more than older customers.
cust.df$online.visits <- rnbinom(ncust, size = 0.3, 
                                 mu = 20 + ifelse(cust.df$email =="yes", 15, 0) - 0.8 * (cust.df$age-median(cust.df$age)))

# For each online visit that a customer makes, we assume there is a 20 % chance of placing an order and use rbinom() to create the variable online.trans.
cust.df$online.trans <- rbinom(ncust, size = cust.df$online.visits, prob = 0.2)
cust.df$online.spend <- exp(rnorm(ncust, mean = 3, sd = 0.1)) * cust.df$online.trans

# Repeat those steps for in-store sales
cust.df$store.trans <- rnbinom(ncust, size=5, mu=3 / sqrt(cust.df$distance))
cust.df$store.spend <- exp(rnorm(ncust, mean=3.5, sd=0.4)) * cust.df$store.trans
summary(cust.df)


#1.2 Simulate Satisfaction Survey Responses

#Assume that overall satisfaction is a psychological construct that is not directly observable.
sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
sat.overall
summary(sat.overall)
sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))
summary(cbind(sat.service, sat.selection))

# Keep the satisfaction score between 1 to 5 
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))


#1.3 Simulate Non-Response Dta
no.response <- as.logical(rbinom(ncust, size = 1, prob = cust.df$age/100)) # Assume the older, the higher probability to answer survey
sat.service[no.response] <- NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))

#1.4 FInalize Data Simulation
cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
summary(cust.df)



#2. Exploring Associations Between Variables with ScatterPlots
str(cust.df)

plot(x=cust.df$age, y = cust.df$credit.score,
     main = "Active Customer as of Feb 2017",
     xlab = "Age", ylab = "Credit in Store", col = "pink")
abline(h = mean(cust.df$credit.score), col = "salmon", lty=2)
abline(v = mean(cust.df$age), col = "salmon", lty=2)

plot(x = cust.df$store.spend, y = cust.df$online.spend,
     main = "Customers Spending as of Feb 2017", 
     xlab = "Prior 12 months in-store sales ($)",
     ylab= "Prior 12 months online sales ($)", 
     col = "pink",
     cex = 0.7)

hist(cust.df$store.spend,
     breaks = (0:ceiling(max(cust.df$store.spend)/10)) * 10,
     main = "Customer as of Feb 2017",
     xlab = "Prior 12 months online sales ($)",
     ylab="Count of customers",
     col = "pink",
     border = "white",
     xlim = c(0, 500),
     ylim = c(0, 800))
axis(1, col = "grey", col.axis = "dark grey", lwd = 0.7)
axis(2, col = "grey", col.axis = "dark grey", lwd = 0.7)

mycol <- c("cyan", "pink")
mypch <- c(3,17)
mycol[head(cust.df$email)]


# In-store sales against online sales on the scale of with or without email on file. 
plot(cust.df$store.spend, cust.df$online.spend,
     cex = 0.7,
     col = mycol[cust.df$email], pch = mypch[cust.df$email],
     main = "Customers as of Feb 2017",
     xlab= "Prior 12 months in-store sales ($)",
     ylab= "Prior 12 months online sales ($)")
axis(1, col = "grey", col.axis = "dark grey", lwd = 0.7)
axis(2, col = "grey", col.axis = "dark grey", lwd = 0.7)
legend(x="topright",legend = paste("email on file:", levels(cust.df$email)),
       col = mycol, pch = mypch,
       bty = "n", pt.cex = 1, text.col = mycol)

ggplot(cust.df, aes(x = store.spend, y = online.spend, shape = email, col = email)) + 
  geom_point(size = 3, alpha = 0.4) + 
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), plot.margin = unit(c(1,1,1,1), "cm"))+
  labs(title = "Customer as of Feb 2017", x = "Prior 12 months in-store sales ($)", y = "Prior 12 months online sales ($)")

# A common solution for such scatterplots with skewed data is to plot the data on a logarithmic scale.
tempx <- log(cust.df$store.spend + 1)
tempy <- log(cust.df$online.spend + 1)

ggplot(cust.df, aes(x=tempx, y=tempy, shape = email, col = email)) + geom_point(size = 2, alpha = 0.5) + 
  theme(plot.title = element_text(face="bold", size = 15, hjust = 0.5), plot.margin = unit(c(1,1,1,1), "cm"))+
  labs(title = "Customer as of Feb 2017", x = "Prior 12 months in-store sales ($)", y = "Prior 12 months online sales ($)")

# INSIGHTS: It now appears that there is little or no association between online and in-store sales; 
#           the scatterplot among customers who purchase in both channels shows no pattern. 
#           Thus, there is no evidence here to suggest that online sales have cannibalized in-store sales


# Understand the relationship between the distance to the store and the time customers spend online and instore. 
distancestore <- ggplot(cust.df, aes(x=log(distance+1), y=log(store.spend+1))) + 
  labs(title = "Store Spend", x = "distance", y = "Store Spend") + 
  geom_point(size = 2, alpha = 0.5) + 
  theme(plot.title = element_text(face="bold", size = 13, hjust = 0.5), plot.margin = unit(c(1,1,1,1), "cm"))
distanceonline <- ggplot(cust.df, aes(x=log(distance+1), y=log(online.spend+1))) + 
  labs(title = "Online Spend", x = "distance", y = "Online Spend") + 
  geom_point(size = 2, alpha = 0.5)+ 
  theme(plot.title = element_text(face="bold", size = 13, hjust = 0.5), plot.margin = unit(c(1,1,1,1), "cm"))
grid.arrange(distancestore, distanceonline, ncol = 2, nrow = 1)

# INSIGHTS: There may be a negative relationship between customers’ distances to the nearest store and in-store spending.
#           Customers who live further from their nearest store spend less in store. 
#           And there is no obvious relationship between distance and online spending. 


# Use Scatterplot Matrices to understand the assciation between numbers of variables.
pairs(formula = ~ age + credit.score + email + distance + online.visits + online.trans + online.spend + store.trans + store.spend, data = cust.df)
pairs(cust.df[, c(2:10)])
# The 2 lines above are alternatives for each other

scatterplotMatrix(formula = ~ age + credit.score + email + distance + online.visits + online.trans + online.spend+ store.trans + store.spend, 
                   data = cust.df, diagonal = "histogram")
gpairs(cust.df[, c(2:10)])
# gpairs()is useful or mar- keting data sets that include continuous and discrete variables.

# Scatterplot gave us many visual information about the relationships, but in order to really measure them, we can use covariance. 
# We often use Cohen’s Rules of Thumb,Cohen proposed that for correlations between variables describing peo- ple, 
#             r = 0.1 should be considered a small or weak association, 
#             r = 0.3 might be considered to be medium in strength, and
#             r = 0.5 or higher could be considered to be large or strong.
cor(cust.df$age, cust.df$credit.score)
cor.test(cust.df$age, cust.df$credit.score)
# r = 0.29 and p < 0.05(the confidence interval does not include 0) showing a statistically significant medium-size effect between age and credit in store

# Use correlation matrices to test correlations for more than 2 variables

corr <- cor(cust.df[, c(2,3,5:12)], use = "complete.obs")
corrplot.mixed(corr, upper = "square", tl.pos = "lt", tl.col = "black",
               col = colorpanel(100, "pink", "gray72", "paleturquoise1"))

# INSIGHT: age is positively cor- related with credit.score; 
#          distance to store is negatively correlated with store.trans and store.spend; 
#          online.visits, online.trans, and online.spend are all strongly correlated with one another, 
#          as are store. trans and store.spend, but not as strongly
#          In the survey items, sat.service is positively correlated with sat.selection.

# Correlation coefficient r measures the linear association between two variables. 
# If the relationship between two variables is not linear, it would be misleading to inter- pret r.
# Many relationships in marketing data are nonlinear.
# Because of the inverse square root relationship, someone who lives 1 mile from the nearest store will spend quite a bit more than someone who lives 5 miles away, yet someone who lives 20 miles away will only buy a little bit more than someone who lives 30 miles away.
# To review, it is important to consider transforming variables to approximate nor- mality before computing correlations or creating scatterplots

## Common transformation: 
# Unit sales, revenue, household income, price:           log(x)
# Distance:                                               1/x, 1/x2, log(x)
# Market or preference share based on a utility value:    exp(x)[(1+exp(x))]
# Right-tailed distributions (generally):                 √x or log(x) (watch out for log(x ≤ 0))
# Left-tailed distributions (generally):                  x2 (x squared)

## General transformation(Box-Cox Transformation): 
# Use the powerTransform(object=DATA) function to find out the value of lambda to make the variable as similar as possible to a normal distribution.
# If attempt to transform a variable that is already close to normally distributed, powerTransform() will report a value of lambda that is close to 1.

powerTransform(cust.df$distance)
# the value of lambda to make distance as similar as possible to a normal distribution is -0.0005. 
# To make it become a positive number, we use 1/cust.df$distance to replace cust.df$distance.
# To extract the value of lambda, use the coef() function
lambda <- coef(powerTransform(1/cust.df$distance))
newdistance <- bcPower(cust.df$distance, lambda)

par(mfrow=c(1,2))
hist(cust.df$distance, 
     xlab = "Distance to the Nearest Store",
     ylab = "Count of Customers",
     main = "Original Distribution")
hist(newdistance, xlab = "Box-Cox Transform of Distance", ylab = "Count of Customers",
     main = "Transformed Distribution")

# Original Correlation: 
cor(cust.df$distance, cust.df$store.spend)
# Value = -0.21

# Transformed Corrlation: 
l.dist  <- coef(powerTransform(cust.df$distance))
l.spend <- coef(powerTransform(cust.df$store.spend+1))
cor(bcPower(cust.df$distance, l.dist), 
    bcPower(cust.df$store.spend+1, l.spend))
# Value = -0.44

# TIPS: In practice, you could consider Box–Cox transformations on all variables with skewed distributions before computing correlations or creating scatterplots. 
#       This will increase the chances that you will find and interpret important associations between variables.

# Use jitter to make a plot of ordinal values more informative
## adding a small amount of random noise to each response. This moves the points away from each other and reveals how many responses occur at each combination of (x,y) values.
par(mfrow=c(1,2))
plot(cust.df$sat.service, cust.df$sat.selection,
     xlab="Customer Satisfaction with Service",
     ylab="Customer Satisfaction with Selection",
     main="Customers as of Feb 2017")
plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection),
     xlab="Customer Satisfaction with Service",
     ylab="Customer Satisfaction with Selection",
     main="Customers as of Feb 2017")

# Use polychoric correlation coefficient for ordinal responses
## The constrained observations from ratings scales affect assessment of correlation with metrics such as Pearson’s r because the number of available scale points con- strains the potential range and specificity of r. 
resp <- !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp])
# Value = 0.585
polychoric(cbind(cust.df$sat.service[resp], cust.df$sat.selection[resp]))
# Value = 0.64
