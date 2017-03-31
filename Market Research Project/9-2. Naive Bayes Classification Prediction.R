# Prediction: Classification 
# Problem 1: predicting segment membership
# Problem 2: predicting who is likely to subscribe to a service

# Steps: 
# 1, A data set is collected in which group membership for each observation is known or assigned
# (e.g., assigned by behavioral observation, expert rating, or clustering procedures).
# 2. The data set is split into a training set and a test set. A common pattern is to select 50–80 % of the observations for the training set (67 % seems to be particularly common), and to assign the remaining observations to the test set. 
# 3. A prediction model is built, with a goal to predict membership in the training data as well as possible.
# 4. The resulting model is then assessed for performance using the test data. Per- formance is assessed to see that it exceeds chance (base rate). Additionally one might assess whether the method performs better than a reasonable alternative (and simpler or better-known) model.

# Naive Bayes classifiers
install.packages("e1071")
library(e1071)

# Step1: split the data into training and test data, 65 % of the data to use for training
seg.raw <- read.csv("http://goo.gl/qw303p")
train.prop <- 0.65
train.cases <- sample(nrow(seg.raw), nrow(seg.raw)*train.prop)
seg.df.train <- seg.raw[train.cases, ]
seg.df.test <- seg.raw[-train.cases, ]

# Step2: train a naive Bayes classifier
seg.nb <- naiveBayes(Segment ~ ., data = seg.df.train)
# Findings: 
#  the estimated odds of membership before any other information is added—is 25.1 % for the Moving up segment, 30.2 % for the Suburb mix segment, and so forth

# Step3: Predict
seg.nb.class <- predict(seg.nb, seg.df.test)
prop.table(table(seg.nb.class))

# Step3-1: Predict the dds of membership in each segment: 
predict(seg.nb, seg.df.test, type = "raw")
# This kind of individual-level detail can suggest which individuals to target according to the difficulty of targeting and the degree of certainty

# Step4: Visualize
install.packages("cluster")
library(cluster)
clusplot(seg.df.test[, -7], seg.nb.class, color = TRUE,labels = 4, main = "Baive Bayes classification, holdout data")

# Model Performance
mean(seg.df.test$Segment == seg.nb.class) # rwa agreement rate 73.3%, including match by chance. 
install.packages("mclust")

library(mclust)
adjustedRandIndex(seg.nb.class, seg.df.test$Segment) # assess performance above chance

# Confusion Matrix: 
table(seg.nb.class, seg.df.test$Segment) # 

# Or compare to the summary values using the true membership
seg.sum <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}
seg.sum(seg.df.test, seg.nb.class)
seg.sum(seg.df.test, seg.df.test$Segment)
# Although assignment is not perfect on a case-by- case basis, the overall group definitions are quite similar.
