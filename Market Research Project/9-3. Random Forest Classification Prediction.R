# Prediction: Classification 
# Problem 1: predicting segment membership
# Problem 2: predicting who is likely to subscribe to a service

# Steps: 
# 1, A data set is collected in which group membership for each observation is known or assigned
# (e.g., assigned by behavioral observation, expert rating, or clustering procedures).
# 2. The data set is split into a training set and a test set. A common pattern is to select 50–80 % of the observations for the training set (67 % seems to be particularly common), and to assign the remaining observations to the test set. 
# 3. A prediction model is built, with a goal to predict membership in the training data as well as possible.
# 4. The resulting model is then assessed for performance using the test data. Per- formance is assessed to see that it exceeds chance (base rate). Additionally one might assess whether the method performs better than a reasonable alternative (and simpler or better-known) model.

# Random Forest
# A random forest (RF) classifier does not attempt to fit a single model to data but instead builds an ensemble of models that jointly classify the data
# RF does this by fitting a large number of classification trees. In order to find an assort- ment of models, each tree is optimized to fit only some of the observations (in our case, customers) using only some of the predictors. The ensemble of all trees is the forest.

install.packages("randomForest")
library(randomForest)
set.seed(54321) # They select variables and sub- sets of data probabilistically. Thus, we use set.seed() before modeling
seg.rf <- randomForest(Segment ~ ., data = seg.df.train, ntree = 3000) #It is sometimes suggested to have 5–10 trees per observation for small data sets like the present one.
# In the confusion matrix, the Travelers and Urban hip segments fit well, while the Moving up and Suburb mix segments had 40 % error rates in the OOB data. This is an indicator that we may see similar patterns in our holdout data.

# Visualize the overall level
library(cluster)
seg.rf.class <- predict(seg.rf, seg.df.test)
clusplot(seg.df.test[, -7], seg.rf.class, color = TRUE, labels = 4, main = "Randome Forest classification, holdout data")

# Individual Level
seg.rf.class.all <- predict(seg.rf, seg.df.test, predict.all = TRUE)
apply(seg.rf.class.all$individual[1:5, ], 1, function(x) table(x)/3000)

seg.sum(seg.df.test, seg.rf.class)
seg.sum(seg.df.test, seg.df.test$Segment)

# Model Performance
mean(seg.df.test$Segment == seg.rf.class) # Raw agreement is 74.3%
table(seg.df.test$Segment, seg.rf.class)

adjustedRandIndex(seg.df.test$Segment, seg.rf.class) # 55.4% RF model performed substantially better than chance


# Use1: 
# Random Forest Variable Performance: Use to estimate the importance of classification variables.
# for each variable, it randomly permutes (sorts) the variable’s values, computes the model accuracy in OOB data using the permuted values, and compares that to the accuracy with the real data. 
# If the variable is important, then its performance will degrade when its observed values are randomly permuted. If, however, the model remains just as accurate as it is with real data, then the variable in question is not very important.
set.seed(54321)
seg.rf <- randomForest(Segment ~ ., data = seg.df.train, ntree = 3000, importance = TRUE)
importance(seg.rf) 
# MeanDecreaseAccuracy: the permutation measure of impact on accuracy; 
# MeanDecreaseGini, a measure of Gini impurity: an assessment of the variable’s ability to assist classification better than chance labeling
varImpPlot(seg.rf, main = "Variable Importance by Segment") 
install.packages("gplots")
library(gplots)
install.packages("RColorBrewer")
library(RColorBrewer)
heatmap.2(t(importance(seg.rf)[, 1:4]),
            col = brewer.pal(9, "Blues"),
            dend = "none", trace = "none", key = FALSE, 
            margins = c(10,10), main = "Variable importance by segment")



# Use2: 
#Prediction: Identifying Potential Customers
# An important business question—especially in high-churn categories such as mobile subscriptions—is how to reach new customers.

# Are subscribers in the training set well differentiated from non-subscribers?
library(cluster)
set.seed(54321)
train.prop <- 0.65
seg.df <- seg.raw[, -7]
train.cases <- sample(nrow(seg.df), nrow(seg.df)*train.prop)
sub.df.train <- seg.df[train.cases, ]
sub.df.test <- seg.df[-train.cases, ]

clusplot(sub.df.train[, -6], sub.df.train$subscribe, color = TRUE, labels = 4, main = "Subscriber clusters, training data")
# the subscribers and non-subscribers are not well differentiated when plotted against principal components

sub.rf <- randomForest(subscribe ~ ., data = sub.df.train, ntree = 3000)
# This demonstrates the class imbalance problem in machine learning. 
# When one category dominates the data, it is very difficult to learn to predict other groups. 
# This frequently arises with small-proportion problems, such as predicting the comparatively rare individuals who will purchase a product, who have a medical condition, who are security threats, and so forth.

# A general solution is to balance the classes by sampling more from the small group. In RF models, this can be accomplished by telling the classifier to use a balanced group when it samples data to fit each tree.
set.seed(546854)
sub.rf <- randomForest(subscribe ~., data = sub.df.train, ntree = 3000,
                       samplsize = c(31, 31)) # use sampsize=c(25, 25) to draw an equal number of subscribers and non-subscribers when fitting each tree (selecting N = 25 each because we have that many subscribers in the training data)
# Predict:
sub.rf.sub <- predict(sub.rf, sub.df.test)
table(sub.rf.sub, sub.df.test$subscribe)

# Another way to look at the result is this: those that the model said were non- subscribers were almost 90 % correct (79 correct out of 88). If the cost to target customers is high, it may be very useful to predict those not to target with high accuracy.

# Performance
adjustedRandIndex(sub.rf.sub, sub.df.test$subscribe) #the model is not better than change, confirm taht use cohen.kappa() in psych package
install.packages("psych")
library(psych)
cohen.kappa(cbind(sub.rf.sub, sub.df.test$subscribe))
