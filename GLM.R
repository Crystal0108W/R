###Logistic Regression
#Logistic regression is useful when you're predicting a binary outcome from a set of continuous and/or categorical predictor variables
install.packages("AER")
library(AER)
data(Affairs, package = "AER")
summary(Affairs)
table(Affairs$affairs)

##Step 1: Dummy Code
Affairs$yn [Affairs$affairs > 0] <-1
Affairs$yn [Affairs$affairs == 0] <- 0

##Step 2: Convert to categorical variable
Affairs$yn <- factor(Affairs$yn, 
                     levels = c(0,1),
                     labels = c("No", "Yes"))
table(Affairs$yn)

##Step 3: Modelling
fit.full<- glm(yn ~ gender + age + yearsmarried + children + 
                    religiousness + education + occupation + rating,
                    data = Affairs, family = binomial())
summary(fit.full)

#From the p value, we conclude that age, years of marriage, religousness and rating make a significant 
#contribution to the model. Next we need to examine whether reduced model fits the data as well.
fit.reduced<- glm(yn ~ age + yearsmarried + 
                 religiousness + rating, data = Affairs, family = binomial())
summary(fit.full)

##Step 3: Chi-square
anova(fit.reduced, fit.full, test = "Chisq")
# The non-significant chi-square value (p=0.21) suggests that the reduced model with four predictors fits as well as 
# the full model with all predictors, reinforcing the belief that gender, children, education and occupation don't
# add significantly to the prediction and we can base our interpretation on the simpler model.

##Step 4: Interpret the parameters in logistic regression
exp(coef(fit.reduced))
# The odds of an affair are increased by a factor of 1.106 for a one-year increase in years married (holding age, religiousness, and mar- ital rating constant)
# Conversely, the odds of an extramarital affair are multiplied by a factor of 0.965 for every year increase in age.

##Step 4: Predict from probabilities but rather odds
#Impact of ratings
testdata<- data.frame(rating=c(1,2,3,4,5), age = mean(Affairs$age),
                      yearsmarried = mean(Affairs$yearsmarried),
                      religiousness = mean(Affairs$religiousness))
testdata

testdata$prob <- predict(fit.reduced, newdata = testdata, type = "response")
testdata
?predict()
#Impact of ratings
testdata<- data.frame(rating=c(1,2,3,4,5), age = mean(Affairs$age),
                      yearsmarried = mean(Affairs$yearsmarried),
                      religiousness = mean(Affairs$religiousness))
testdata

testdata$prob <- predict(fit.reduced, newdata = testdata, type = "response")
testdata

#Impact of ages
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=seq(17, 57, 10),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata$prob <- predict(fit.reduced, newdata = testdata, type = "response")
testdata

##Step5: Test for over dispersion, if it occurs use quasibinominal instead
deviance(fit.reduced)/df.residual(fit.reduced)
# Compare the value with 1, if it's considerable larger than 1, you have evidence of dispersion.

###Poisson Regression
##Step1: Check summary statistics for your data

install.packages("robust")
library(robust)
data(breslow.dat, package = "robust")
names(breslow.dat)

summary(breslow.dat[c(6,7,8,10)])
opar<- par(no.readonly = TRUE)
par(mfrow = c(1,2))
par(las = 1)
attach(breslow.dat)
hist(sumY, breaks = 20, xlab = "Seisure Count",
     main = "Distribution of Seizures")
boxplot(sumY ~ Trt, xlab = "Treatment", main = "Group Comparisons")
par(opar)

##Step2: Fit the Poisson Regression
fit <- glm(sumY ~ Base + Age + Trt, data = breslow.dat, family = poisson())
summary(fit)

##Step3: interpret the model parameter
coef(fit)

# The regression patameter0.0227 for Age indicates that a one-year increase in age is associated 
# with a 0.03 increase in the log mean number of seizures, holding other two variables constant. 

# get the original scale of the dependent variable
exp(coef(fit))
# a one-year increase in age multiplies the expected number of seizures by 1.023, holding the other 
# variables constant

##Step4: Test for over dispersion, if it occurs use quasipoisson instead
install.packages("qcc")
library(qcc)
qcc.overdispersion.test(breslow.dat$sumY, type = "poisson")
# here p value is less than .05, suggesting the presence of overdispersion

fit.od<- glm(sumY ~ Base + Age + Trt, data = breslow.dat, family = quasipoisson())
summary(fit.od)