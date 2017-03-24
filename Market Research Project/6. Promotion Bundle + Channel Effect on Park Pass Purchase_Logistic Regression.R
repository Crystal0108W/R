# Linear Models for Binary Outcomes: Logistic Regression
# Question: are customers more likely to purchase the season pass when it is offered in the bundle (with free parking), or not?
pass.df <- read.csv("http://goo.gl/J8MH6A")
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))
summary(pass.df)


# Process Data
pass.tab <- c(242, 639, 38, 359, 284, 27, 449, 223, 83, 278, 49, 485) # Create the cross-tab data table in R
dim(pass.tab) <- c(3, 2, 2) # Add dimensions to the vector
class(pass.tab) <- "table"  # Reformats it as a 3 × 2 × 2 array

dimnames(pass.tab) <- list(Channel = c("Mail", "Park", "Email"),
                           Promo=c("Bundle", "NoBundle"),
                           Pass=c("YesPass", "NoPass")) 

install.packages("vcdExtra")
library(vcdExtra)
pass.df <- expand.dft(pass.tab) # Converts a frequency table, given either as a table object or a data frame in frequency form to a data frame representing individual observations in the table.

table(pass.df$Pass, pass.df$Promo) # the factors in pass.df are alphabetized, NoBundle appears in the second column because it has a higher value - counter- intuitive.
# Change Nobundle to 0 and Bundle to 1
pass.df$Promo <- factor(pass.df$Promo, levels = c("NoBundle", "Bundle"))

# Fit a Logistic Rrgression Model - GLM
# GLM can handle dependent variables that are not normally distributed. 
# Thus, GLM can be used to model data counts (such as number of pur- chases) or time intervals (such as time spent on a website) or binary variables (e.g., did/didn’t purchase)
# The common feature of all GLM models is that they relate normally distributed predictors to a non-normal outcome using a function known as a link. 
# This means that they are able to fit models for many different distributions using a single, consistent framework.
# In glm() options: "family=" that specifies the distribution for the outcome variable.(binominal in present case)
#                   "link=": the defalt link function for a binomial model is the logit function. 

pass.m1 <- glm(Pass ~ Promo, data = pass.df, family = binomial)
summary(pass.ml) # confirms that the effect of bundle is statistically significant, (p-value < 0.05 and beta = 0.38879)
plogis(0.38879)/(1 - plogis(0.38879)) # = 1.475195
# ALTERNATIVE
exp(0.38879) # = 1.475195 
# This shows that the effect of Bundle is an estimated odds ratio of 1.475, meaning that customers are 1.475 times more likely to purchase the pass when it is offered in the bundle./ the bundle increases the pur- chase likelihood by 47.5 %
# ALTERNATIVE
exp(coef(pass.m1)) # odds ratio
exp(confint(pass.m1)) # confidence interval
# The odds ratio for the promotion bundle is estimated to be 1.28–1.70, a significant positive effect. 
# Under the assmption that the model Pass ∼ Promo is the one we want to interpret, the promotion is highly effective. 

# Reconsidering the Model
table(pass.df$Pass, pass.df$Channel)
# Visualize the table using Mosaic Plot
library(vcd)
doubledecker(table(pass.df)) # Simple Mosaic
mosaic(table(pass.df), shade = TRUE, legend = TRUE) #This implies that the model Pass ∼ Promo maybe inadequate and needs to account for the effect of Channel.
pass.m2 <- glm(Pass ~ Promo + Channel, data = pass.df, family = binomial) # baseline (omitted) email channel in these simulated data.
summary(pass.m2)
exp(coef(pass.m2)) # Promotion in this model became negatively correlated with pass purchase, cue for considering interaction between channels and promotion
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, data = pass.df, family = binomial)
summary(pass.m3) # The interaction of promotion with channel is statistically significant, and is strongly negative for the mail and in-park channels, as opposed to the baseline (omitted) email channel in these simulated data.
exp(coef(pass.m3)) # the promotion is only 2–11 % as effective through the mail and in-park channels as it is in email
exp(confint(pass.m3))

