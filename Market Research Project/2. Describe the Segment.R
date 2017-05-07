# Market Research: Compare the groups: Tables and Visualizations
# Step 1: simulating consumer segement data

# Define the structure of the data sets
segVars <- c("Age","Gender","Income","Kids","ownHome","Subscribe")
segVarTye <- c("norm","binom","norm","pois","binom","binom")
segName <- c("Suburb Mix","Urban hip","Travellers","Moving up")
SegSize <- c(100,50,80,70)

# Define the values of the data by specifying distributional parameters such as the meand for each variables within each segment
segMeans <- matrix(c(40, 0.5,55000,2,0.5,0.1,
                     24, 0.7,21000,1,0.2,0.2,
                     58,0.5,64000,0,0.7,0.05,
                     36,0.3,52000,2,0.3,0.2), ncol = length(segVars),byrow = TRUE)

# For normal variables, age and income, variance needs to be additionally specified
segSDs <- matrix(c(5,NA,12000,NA,NA,NA,
                   2,NA,5000,NA,NA,NA,
                   8,NA,21000,NA,NA,NA,
                   4,NA,10000,NA,NA,NA),ncol = length(segVars),byrow = TRUE)

# Generate Data
seg.df <- NULL
set.seed(12345)

for (i in seq_along(segName)) {
  cat(i,segName[i], "\n")
  
  #empty matrix to hold this particular segment's data
  this.seg <- data.frame(matrix(NA, nrow=SegSize[i], ncol = length(segVars)))
  
  # within segment, iterate over variables and draw appropriate random data
  for (j in seq_along(segVars)) { #and iterate over each variable
    if (segVarTye[j] == "norm") { # draw random normals
      this.seg[,j] <- rnorm(SegSize[i], mean = segMeans[1,j], sd = segSDs[i,j])
    } else if (segVarTye[j]=="pois"){ #draw counts
      this.seg[,j] <- rpois(SegSize[i], lambda = segMeans[i,j])
    } else if (segVarTye[j]=="binom") { # draw binomial
      this.seg[,j] <- rbinom(SegSize[i], size = 1, prob = segMeans[i,j])
    } else {
      stop("Bad segment data type: ", segVarTye[j])
    }
  }
  seg.df <- rbind(seg.df, this.seg)
}

names(seg.df) <- segVars # Make the data frame names match what we defined
seg.df$Segment <- factor(rep(segName, time = SegSize))
seg.df$ownHome <- factor(seg.df$ownHome, labels = c("ownNo","ownYes"))
seg.df$Gender <- factor(seg.df$Gender, labels = c("Female","Male"))
seg.df$Subscribe <- factor(seg.df$Subscribe, labels = c("subNo","subYes"))

summary(seg.df)
save(seg.df, file = "C:/Users/Crystal/Desktop/SegSimulate_data.csv")

#Finding Descriptive by Group
mean(seg.df$Income[seg.df$Segment == "Moving up"])
by(seg.df$Income, seg.df$Segment, mean)
by(seg.df$Income, list(seg.df$Segment,seg.df$Subscribe), mean)
seg.income.mean <- aggregate(seg.df$Income,list(seg.df$Segment),mean) # Aggregate works almost identical to by in its list from
# The first advantage of aggregate() is that the result is a data frame. You can save the results of aggregate to an object which you can then index, subject to further computation. 
seg.df$segIncome <- seg.income.mean[seg.df$Segment,2]

install.packages("car")
library(car)
some(seg.df) # some()does a random sample of rows 

# The second advantage of aggregate is that aggregate(y~x) means to aggregate y according to the levels of x
aggregate(Income~Segment, data = seg.df, mean) # Aggregate(formula, data, FUN)
agg.data <- aggregate(Income~Segment + ownHome, data = seg.df, mean)
#The aggregate command allows us to compute functions of continuous variables, for any combination of factors 

# Compute frequencies  using table(factor1, factor2...)
with(seg.df, table(Segment, ownHome))
with(seg.df, table(Kids, Segment)) # In this case we are treating kids as a factor and not a number
# Calculate the total number of kids in all households in one segment 
xtabs(Kids~Segment, data = seg.df) # Alternative 1
aggregate(Kids ~ Segment, data = seg.df, sum) # Alternative 2
seg.tab <- with(seg.df, table(Kids, Segment))
apply(seg.tab*0.7, 2, sum)
colSums(seg.tab*0.7)


# Visualize by Group - Discreet Data
# histogram()in lattice package understands formula notation including conditioning on a factor, which means to seperate the plot into multiple panes based on that factor

require(lattice)
histogram(~Subscribe | Segment, data = seg.df)
histogram(~Subscribe | Segment, data = seg.df, type = "count",
          layout = c(4,1), col = c("pink","salmon"))

histogram(~Subscribe | Segment + ownHome, data = seg.df)
# We can conclude the differences in subscription rate according to home ownership within segment are small. 
# An implication is that we should continue to market to both homeowners and non-homeowners

prop.table(table(seg.df$Subscribe, seg.df$Segment), margin =1)
barchart(prop.table(table(seg.df$Subscribe, seg.df$Segment),
                    margin = 2)[2, ], xlab = "Subscriber proportion by Segment", col = "pink")

# Visualize by Group - Continuous Data
# Alternative 1: aggregate() + barchart()
seg.mean <- aggregate(Income ~ Segment, data = seg.df, mean)
library(lattice)
barchart(Income ~ Segment, data = seg.mean, col = "pink")
seg.income.agg <- aggregate(Income ~ Segment + ownHome, data = seg.df, mean)
barchart(Income~Segment, data = seg.income.agg,
         groups = ownHome, auto.key = TRUE, 
         par.settings = simpleTheme(col = terrain.colors(2)))
# Alternative 2: boxplot or box-and-whiskers show more about the distributions of values
boxplot(Income ~ Segment, data = seg.df, yaxt = "n", ylab = "Income ($k)")
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(side = 2, at = ax.seq, labels = paste(ax.seq/1000, "k", sep = ""), las = 2)

bwplot(Segment~Income, data = seg.df, horizontal = TRUE, xlab = "Income")
bwplot(Segment~Income|ownHome, data = seg.df, horizontal = TRUE, xlab = "Income") # breakout home ownership


# TECHNIC KEY POINTS
#1. When writing for() loops, use seq along() instead of 1:length()
#2. Whencreatingadataobjectfromscratch,pre-populateitwithmissingdata(NA) and then fill it in, for speed and reliability
#3. The by() command can split up data and automatically apply functions such as mean() and summary()
#4. aggregate()isevenmorepowerful:itunderstandsformulamodelsandpro- duces a reusable, indexable object with its results
#5. Frequency of occurrence can be found with table(). For count data, espe- cially when using formulas, xtabs() is useful
#6. Charts of proportions and occurrence by a factor are well suited to the lattice package histogram() command
#7. Plots for continuous data by factor may use barchart(), or even better, box-and-whiskers plots with boxplot(). The lattice package extends such plots to multiple factors using formula specification and the bwplot() command


###########################################################################################################
# Market Research: Comapring Groups: Statistical Tests
# It looks different, but does it REALLY different?
# Inferential statistical procedures: chi-square, t-test, ANOVA

# Chi-Square test determines whether the frequencies(distribution) in cells are significantly different from what one would expect on the basis of their total counts
# chisq.test() operates on a TABLE
chisq.test(table(seg.df$Segment))
# the p-value of 0.0006 shows that there is a statistically different number of customers in each segment

table(seg.df$Subscribe, seg.df$ownHome)
chisq.test(table(seg.df$Subscribe, seg.df$ownHome))
# the p-value of 0.08 indicates that the home ownership is significantly independent of subscription status in the data
# Or to put it in another way, there is no significant relationship between subscription rate and home ownership
# 1. For 2*2 tables, chisq.test() defaults to using Yate's correction and if you want the results to match traditional calues such as calculation by hand
chisq.test(table(seg.df$Subscribe, seg.df$ownHome), correct = FALSE)

# 2. chisq.test() can calculate confidence intervals using a simulation method, where it compares the observed table to thousands of simulated tables with the same marginal counts, 
#    The p-value indicates the proportion of those simulations with differences between the cell counts and marginal proportions at lease as large as the ones in the observed table. 
chisq.test(table(seg.df$Subscribe, seg.df$ownHome), sim = TRUE, B = 10000)
# the p-value of 0.06 is slightly different from the previous p-values, but overall, they are indicating the same conclusion that there is no significant relationship between home ownership and subscription

# The observations are a random sample of a binomial value, binom.test(success, trials, probabilities) can be used to test the lokelihood of random observing # of successes out of # of trials in one direction, if the true likelihood is probability
binom.test(10, 30, p = 0.5)
# number of successes = 10, number of trials= 30, p-value = 0.09874
# alternative hypothesis: true probability of success is not equal to 0.5 95 percent confidence interval:0.1728742 0.5281200
# sample estimates: probability of success: 0.3333333
sum(dbinom(8:12, 20, 0.5)) #density estimate for a binomial distribution
# We would observe 8-12 successes out of 20 trials if the true probability is 0.5

# An exact binomial test may be overly conservative in its estimation of confidence level
# Alteratively, we can use binom.confit(, method = "agresti-counll")
install.packages("binom")
library(binom)
binom.confint(12,20,methods = "ac")
# with Agresti-Coull method, the confidence interval is slightly smaller but still includes 50%


# Testing Group Means: t.test()
# A t-test compares the mean of one sample against the mean of another sample (or agains a specific value such as 0)
# It compares the mean for EXACTLY 2 sets of data. 
# Before applying any statistical test or model, it is important to examine the ASSUMPTIONS and check for skew, discontinuities and outliers

# Assumption Test: Check for normal distribution with boxplot or histogram: 
hist(seg.df$Income)
with(seg.df, hist(Income[ownHome=="ownYes"]))
with(seg.df, hist(Income[ownHome=="ownNo"]))

# Test whether home ownership overall is related to differences in income, across all segments
t.test(Income ~ ownHome,data = seg.df)
# t statistics is -1.8871, with a p-value of 0.06, this means that the null hypothesis of no difference in income by home ownership is not rejected. 
# And we are 95% confident that the group difference is between -6062 to 127
# Fianlly, the mean income for ownNo is 54277.57 and ownYes is 57245.16

# ANOVA: test multiple group means
seg.aov.own <- aov(Income ~ ownHome, data = seg.df)
anova(seg.aov.own) # Or summary(seg.aov.own)

seg.aov.seg <- aov(Income ~ Segment, data = seg.df)
anova(seg.aov.seg)# Or summary(seg.aov.seg)

# If income varies by both home ownership adn segment, we can add both factors into the ANOVA model to test this:
anova(aov(Income ~ Segment + ownHome, data = seg.df))
# segment is a significant predictor but home ownership is not a significant predictor, yet the previous results said that it was significant.
# This is because that segment and home ownership are not independent and the effect is captured sufficiently by segment membership alone
# There is interaction effect. 
# In a model formula, "+" indicates taht variables should be modeled for main effects only. 
# Instead, we can write ":" for an interaction or "*" for both main effect and interaction

anova(aov(Income ~ Segment * ownHome, data = seg.df))
# The statistics shows that segment is a significant predictor while home ownership and the interaction of segment with homw ownership is not significant. 
# In other words, segments membership is again the best predictor on its own. 

# Model Comparison in ANOVA
# Another capability of the anova() command is to compare two or more models
anova(aov(Income ~ Segment, data = seg.df),
      aov(Income ~ Segment + ownHome, data = seg.df))

# The results(F = 1.35, p = 0.245) indicate that Model2, which includes both segment and home ownership is not significantly different in overall fit from Model1. 
# NOTE: the model comparison as performed by the anova() command ONLY makes sense in the case of nested models.
# Here Income ~ Segment is nested within Income ~ Segment + ownHome


# Visualizing Group Confidence Intervals
# A good way to visualize the results ofan ANOVA is to plot confidence intervals for the group means
install.packages("multcomp")
library(multcomp)
seg.aov <- aov(Income ~ -1 + Segment, data = seg.df) # remove the intercept by adding "-1" to the model formula:
glht(seg.aov)

par(mar=c(6,10,2,2))
plot(glht(seg.aov),
     xlab = "Income",
     main = "Average Income by Segment (95% CI)")

# Variable Selection in ANOVA: Stepwise Modeling
seg.aov.step <- step(aov(Income ~ ., data = seg.df))



# Bayes
# Bayesian analysis is a more direct way to tackle the probability questions (How likely is the difference?). It is very different from the traditional frequentist statistical model
install.packages("BayesFactor")
library(BayesFactor)
set.seed(12345)
seg.bf1 <- lmBF(Income ~ Segment, data = seg.df)
seg.bf2 <- lmBF(Income ~ Segment + ownHome, data = seg.df)
seg.bf1 / seg.bf2 # Comparing the two models 
# The results indicate that the ratio of Bayes Factors for model 1 vs. model 2 is 3.61. This means that the first model is the preferable model by a factor of 3.61

seg.bf.chain <- posterior(seg.bf1, 1, iterations = 10000) # To find the model parameters and their credible range. Draw 10000 samples of the possible parameters from model 1 
plot(seg.bf.chain[, 1:6]) # Inspect whether the draws converged to stable values such that the estimates are reliable
summary(seg.bf.chain)

head(seg.bf.chain)
seg.bf.chain[1:7, 1:5] # By indexing the chain, it is confirmed to be arranged as a matrix
seg.bf.chain.total <- seg.bf.chain[, 2:5] + seg.bf.chain[, 1]
seg.bf.ci <- t(apply(seg.bf.chain.total, 2, quantile, pr = c(0.025, 0.5, 0.975)))
seg.bf.ci

install.packages("ggplot2")
library(ggplot2)
seg.bf.df <- data.frame(seg.bf.ci)
seg.bf.df$Segment <- rownames(seg.bf.df)

summary(seg.bf.df)
ggplot(seg.bf.df, aes(x = Segment, y = X50., ymax = X97.5., ymin = X2.5.)) + 
  geom_point(size = 4) + 
  geom_errorbar(width = 0.2) + 
  labs(y = "Income", title = "95% CI for Mean Income by Segment") + coord_flip()

# The Bayesian results are not all that different from: 
  # par(mar=c(6,10,2,2))
  # plot(glht(seg.aov),
  #     xlab = "Income",
  #     main = "Average Income by Segment (95% CI)")

# TECHNIC KEY POINTS
#1. chisq.test() and binom.test() find confidence intervals and perform hypothesis tests on tables and proportion data, respectively. 
#   The binom package offers options such as Agresti–Coull and Bayesian versions of binomial tests that may be more informative and robust than standard exact binomial tests.
#2. A t.test() is a common way to test for differences between the means of two groups (or between one group and a fixed value)
#3. ANOVA is a more general way to test for differences in mean among several groups that are identified by one or more factors. The basic model is fit with aov() and common summary statistics are reported with anova()
#4. The anova() command is also useful to compare two or more ANOVA or other linear models, provided that they are nested models 
#5. Stepwise model selection with step() is one way to evaluate a list of variables to select a well-fitting model, although we recommend that it be used with caution as other procedures may be more appropriate
#6. Plotting a glht() object from the multcomp package is a good way to visualize confidence intervals for ANOVA models 
#7. A relatively straightforward starting point for Bayesian ANOVA and other linear models is the BayesFactor package
#8. Bayesian models should be evaluated for the stability and distribution of their estimated parameters using trace and density plots
#9. Credible intervals (and other types of intervals) may be plotted with the ggplot2 option to add geom errorbar() lines for groups
