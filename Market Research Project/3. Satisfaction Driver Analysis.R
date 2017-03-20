# Identifying Drivers of Outcomes: Linear Models
# Satisfaction Driver Analysis, Advertising Response modeling, Customer Retention(churn) modelling and pricing analysis
# The model is applied to satisfaction driver analysis and to understand how advertising are related to sales (marketing mix modelling)
# Project Backgroud Intro: 
# We conduct a satisfaction drivers analysis using survey data for customers who have visited an amusement park. 
# In the survey, respondents report their levels of satisfaction with different aspects of their experience, and their overall satisfaction


# Step1: Simulate Survey Data
# Data Measures: 
# 1. whether the respondent visited on a weekend
# 2. the number of children brought
# 3. distance traveled to the park
# 4. expressed satisfaction overall 
# 5. satisfaction with the rides, games, waiting time, and cleanliness

set.seed(12345)
nresp <- 500 # number of survey respondents
halo <- rnorm(n=nresp, mean=0, sd=5)
summary(halo)
rides <- floor(halo + rnorm(n=nresp, mean=80, sd=3)+1) # Use floor() to make them integers
summary(rides)
games <- floor(halo + rnorm(n=nresp, mean=70, sd=7)+5)
summary(games)
wait <- floor(halo + rnorm(n=nresp, mean=65, sd=10)+9)
summary(wait)
clean <- floor(halo + rnorm(n=nresp, mean=85, sd=2)+1)
summary(clean)

distance <- rlnorm(n=nresp, meanlog=3, sdlog=1) # Use lognormal distribution to generate data for distance
num.child <- sample(x = 0:5, size = nresp, replace = TRUE, 
                    prob = c(0.3, 0.15, 0.25, 0.15, 0.1, 0.05)) # Use sample() to sample discrete distribution
weekend <- as.factor(sample(x = c("yes","no"), size = nresp, replace = TRUE, prob = c(0.5,0.5)))

overall <- floor(halo + 0.5* rides + 0.3*wait + 0.2*clean + 
                   0.03*distance + 5*(num.child==0) + 0.3*wait*(num.child>0) +
                   rnorm(n=nresp, mean=0, sd=7) - 51)

sat.df <- data.frame(weekend, num.child, distance, rides, games, wait, clean, overall)
rm(nresp, weekend, distance, num.child, halo, rides, games, wait, clean, overall)


# Fitting Linear Model with lm()
summary(sat.df)

# Before modeling, there are two important things to check: 
# 1. each individual variable has a reasonable distribution, 
install.packages("gpairs")
library(gpairs)
gpairs(sat.df)
# All of the satisfaction ratings are close to normal distribution but distance has a highly skewed distribution
sat.df$logdist <- log(sat.df$distance) # transform the data to a more natural distribution
gpairs(sat.df)
# 2. joint relationships among the variables are appropriate for modeling.
install.packages("corrplot")
library(corrplot)
corrplot.mixed(cor(sat.df[, c(2, 4:9)]), upper = "ellipse") # Select columns c(2, 4:9) to exclude the categorical variable weekend and the raw variable distance transformed as logdist.


# Linear Model with a Single Predictor
lm(overall~rides, data = sat.df) # we get coefficients from lm() function, but we need to use the coefficients manually

m1 <- lm(overall~rides, data = sat.df) # lm Object
summary(m1)
# F-statistic: provides a statistical test of whether the model predicts the data better than simply taking the average of the outcome variable and using that as the single prediction for all the observations.
# In essence, this test tells whether the model is better than a model that predicts overall satisfaction using no predictors.
#  In the present case, the F-statistic shows a p-value << .05, so we reject the null hypothesis that a model without predictors performs as well as model m1.

plot(overall ~ rides, data = sat.df,
     xlab = "Satisfaction with Rides", ylab = "Overall Satisfaction")
abline(m1, col = "salmon")

# Check Model Fit
# Assumption1: the relationship between the predictors and the outcomes is linear.
plot(overall ~ rides, data = sat.df)
# Assumption2: prediction errors(residules) are normally distributed and look like random noise with no pattern.
plot(ml$fitted.values, m1$residuals)
# Assumption3: the residuals follow a normal distribution
# All diagnostics in one plot
par(mfrow = c(2,2))
plot(m1)
# There is no obvious pattern between the fitted values for overall satisfaction and the residuals;this is consistent with the idea that the residuals are due to random error, and supports the notion that the model is adequate.
# A QQ plot helps you see whether the residuals follow a normal distribution
# The final plot in the lower right panel helps to identify potential outliers, observations that may come from a different distribution than the others.
# Then inspect the identified points by selecting those rows: 
sat.df[c(412, 307, 431), ] # In this case, none of the data points is obviously invalid, so we will still keep them all


# Linear Model with a Multiple Predictors
m2 <- lm(overall ~ rides + games + wait + clean, data = sat.df)
summary(m2)

# Check Model Fit
plot(m2)
# Examine the model coefficients
install.packages("coefplot", type = "source")
library(coefplot)
coefplot(m2,intercept = FALSE, outerCI = 1.96, lwdOuter = 2,
         ylab = "Rating of Feature", 
         xlab = "Association with Overall Satisfaction") # using outerCI=1.96, which corresponds to a 95% confidence interval) and to increase the size of the plotted lines slightly with lwdOuter=2
# The results show that the satisfaction with rides is estimated to be the most important features, followed by clean and wait, and game is estimated to be relatively less important
# One of the reasons we want to visualize the confidence interval of coefficients: 
# A typical satisfaction drivers survey might include dozens of different features. As you add predictors to a model, estimates of the coefficients become less precise due to both the number of effects and associations among the variables.
# This shows up in the lm() output as larger standard errors of the coefficients, indicating lower confidence in the estimates.

# Comparing Models
# Comapring Adjusted R squared, which control for the number of predictors in the model.
par(mfrow = c(1,1))
plot(sat.df$overall, fitted(m1), col = "pink",
     xlim = c(0, 100), ylim = c(0, 100),  pch = 4, 
     xlab = "Actual Overall Satisfaction",
     ylab = "Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col = "grey", pch = 5)
legend("topright", legend = c("model1", "model2"),
       col = c("pink", "grey"), pch = c(4,5))
# Since the grey cloud of points is more tightly clustered along the diagonal line, the m2 explains more of the variation in the data than m1

# Alternative: since the models are nested, we can use anova()
anova(m1, m2)
# The p-value < 0.001, indicates that m2 significantly improved the fit of the model 
# Through comparison between model1 and model2, we also discoveed that the coefficients for ride in 2 models differ, and that is because rides is not independent of all the other variables


# Use a Model to Make Predictions
coef(m2)%*%c(1,100,100,100,100) # for a customer who rated the four separate aspects as 100 points each, we predict that he/she will have 88.41 overall satisfaction
# ALternative: 
predict(m2, sat.df[1:10, ])
# ALternative: 
fitted(m2)[1:10] # The predictions for observations used to estimate the model are also stored in the model object, and can be accessed with fitted():

# Standardize the Predictors
# if the variables have different scales, then their coefficient values would not be directly comparable. 
sat.std <- sat.df[, -3] # remove the third column distance because we will use logdist instead
sat.std[, 3:8] <- scale(sat.std[, 3:8]) 
# If you want to interpret coefficients in terms of the original scales, then you would not standardize data first. However, in driver analysis we are usually more concerned with the relative contribution of different predictors and wish to compare them, and standardization assists with this. 
summary(sat.std)


# Using Factors as Predictors
m3 <- lm(overall ~ rides + games + wait + clean + 
           weekend + logdist + num.child, data = sat.std) # R handles weekend value by converting the data to a a numeric value where 1 is assigned to the value of yes and 0 to no.
summary(m3)
# An ANOVA model is a linear model with a factor as a predictor, lm() is a more flexible method and allows us to include both numeric and factor pre- dictors in the same model.
# Another problem with this model is the number of children, num.child is a numeric variable, ranging 0–5, but it doesn’t necessarily make sense to treat it as a number. In doing so, we implicitly assume that satisfaction goes up or down linearly as a function of the number of children, and that the effect is the same for each additional child. 
# Correct this by converting num.child to a factor and re-estimating the model, which means to change each level of num.child value to a dummy variable
sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + 
           weekend + logdist + num.child.factor, data = sat.std) 
summary(m4)
# The coefficient for num.child.factor2 is 1.031, meaning that people with two children rate their overall satisfaction on average a full standard deviation higher than those with no children.
# A striking thing about m4 is that the increase in overall satisfaction is about the same regardless of how many children there are in the party—about one standard deviation higher for any number of children. This suggests that we don’t actually need to estimate a different increase for each number of children. 
# In fact, if the increase is the same for one child as for five children, attempting to fit a model that scales increasingly per child would result in a less accurate estimate.
# Instead, we declare a new variable called has.child that is TRUE when the party has children in it and FALSE when the party does not have children.

sat.std$has.child <- factor(sat.std$num.child > 0)
m5 <- lm(overall ~ rides  + wait + clean + 
          has.child, data = sat.std) # Also drop weekend, games and distance since they dont seem to be significant 
summary(m5)


# Checking Interactions
m6 <- lm(overall ~ rides + games + wait + clean + 
           weekend + logdist + has.child + rides:has.child + games: has.child + wait: has.child + 
           clean: has.child + rides: weekend + games: weekend + wait: weekend + clean: weekend, data = sat.std)# a new model with interactions between the satisfaction ratings and two variables that describe the visit: no.child and weekend
summary(m6)
# 3 interactions are significant: wait:has.childTRUE, game:has.childTRUE, wait:weekendyes
# Drop the nonsignificant interactions and predictors, and create a new model 7
m7 <- lm(overall ~ rides + games + wait + 
           has.child + wait:has.child + games: has.child +  wait: weekend, data = sat.std)
summary(m7)

# Marketer Moment: 
# We identify several possible marketing interventions: 
# If we want to increase satisfaction overall, we could perhaps do so by trying to increase:the number of visitors with children
# Alternatively, if we want to appeal to visitors without children, we might engage in further research to understand why their ratings are lower.
# If we are allocating budget to personnel, the importance of cleanliness suggests continuing to allocate resources there (as opposed, say, to games). 
# We might also want to learn more about the association be- tween children and waiting time, and whether there are things we could do to make waiting less frequent or more enjoyable.
coefplot(m7,intercept = FALSE, outerCI = 1.96, lwdOuter = 2,
         ylab = "Rating of Feature", 
         xlab = "Association with Overall Satisfaction") # using outerCI=1.96, which corresponds to a 95% confidence interval) and to increase the size of the plotted lines slightly with lwdOuter=2
# Conclusion: 
# 1. it is especially important to consider standardizing the predictors when modeling interactions in order to have an interpretable and comparable scale for coefficients. 
# 2. one should always include main effects (such as x + y) when including an interaction effect (x:y). If you don’t estimate the main effects, you won’t know whether a purported interaction is in fact due to an interaction, or is instead due to one of the individual variables’ unestimated main effects.

# y∼x: y is a linear function of x
# y∼x-1: Omit the intercept
# y∼x+z: y is a linear combination of x and z
# y ∼ x:z: Include the interaction between x and z
# y ∼ x*z: Include x, z and the interaction be- tween them
# y ∼ (u + v + w)ˆ3: Include u, v, and w, and all interac- tions among them up to three-way (u:v:w)
# y∼ (u+v+w)ˆ3 - u:v: Include these variables and all inter- actions up to three-way, but remove the u:v interaction


# Overfitting Problem 
# One way to avoid overfitting is to keep a close eye on the standard errors for the coefficients; small standard errors are an indicator that there is sufficient data to estimate the model.
# Another approach is to select a subset of the data to hold out and not use to estimate the model. After fitting the model, use predict() on the hold out data and see how well it performs.

# General steps in creating a linear model:
# 1. Inspect the data to make sure it is clean and has the structure you expect
# 2. Check the distributions of the variables to make sure they are not highly skewed. If one is skewed, consider transforming it 
# 3. Examine the bivariate scatterplots and correlation matrix to see whether there are any extremely correlated variables(such as r > 0.9, or sev- eral with r > 0.8). If so, omit some variables or consider transforming them if needed
# 4. If you wish to estimate coefficients on a consistent scale, standardize the data with scale()
# 5. After fitting a model, check the residual quantiles in the output. The residuals show how well the model accounts for the individual observations
# 6. Check the standard model plots using plot(), which will help you judge whether a linear model is appropriate or whether there is nonlinearity, and will identify potential outliers in the data 
# 7. Try several models and compare them for overall interpretability and model fit by inspecting the residuals’ spread and overall R2. If the models are nested, you could also use anova() for comparison. 
# 8. Report the confidence intervals of the estimates with interpretation and recommendations. 


# Bayesian Linear Models with MCMCregress()
install.packages("MCMCpack")
library(MCMCpack)
m7.bayes <- MCMCregress(overall ~ rides + games + wait + clean + logdist + 
                          has.child + wait:has.child, data = sat.std)
summary(m7.bayes)
# Interpretation: the Bayesian output does not include statistical tests or p-values; 
# Instead, to determine whether a parameter is likely to be non-zero (or to compare it to any other value), check the 2.5 and 97.5 %’iles and directly interpret the credible interval: 
# If the interval includes value 0 then we conclude that the coefficient is not credibly different from 0 (in present case: games)
# In fitting models, it is not always the case that classical and Bayesian estimates are so similar, and when they differ, we are more inclined to trust the Bayesian estimates. 

# Extension: GLM framework
# poisson and binomial regression model for outcomes that are counts
# hazard regression for event occurrence (timing regression or survival modeling)
# logistic regression for binary outcomes 
# For ratings on a 5- or 7-point scale, an alternative is a cut-point model, such as an ordered logit or probit model. These models can be fit with the polr() function from the MASS package
# A more sophisticated model for ordinal ratings data is a Bayesian scale-usage het- erogeneity model. 
# Or a hierarchical model, in which the effect varies for different people, with both a group-level and an individual-level effect. 
# Finally, many data sets have variables that are highly correlated (known as collinear- ity), and this can affect the stability and trustworthiness of linear modeling


# Key Points: 
# lm() produces an object that can be used with plot(), summary(), predict(), and other functions to inspect the model fit and estimates of the coefficients.
# Check distribution, approximately normal distributions are generally preferred, data such as counts and revenue often need to be transformed.
# Standardize variables so that the coefficients are comparable with each other
# We can compare the fit of dif- ferent models using the R-squared value or, if models are nested by using the more formal ANOVA test (anova())
# It is recommended that coefficients are interpreted in terms of their estimated ranges, such as confidence intervals in the case of lm() or credible intervals from Bayesian estimates.
