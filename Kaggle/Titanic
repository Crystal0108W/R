install.packages("rpart")
install.packages("rpart.plot")
install.packages("party")
install.packages("randomForest")
install.packages("e1071")
install.packages("dplyr")


#########################################LOGISTIC REGRESSION##########################################
# Prepare Data
library(dplyr)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
test <- as.data.frame(test)
validate <- read.csv("gender_submission.csv")
full <- bind_rows(train,test)
test["PassengerId"] <- NULL

df <- bind_cols(validate, test)
str(full)

full$Title <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[, ]')[[1]][3])  #Split the name to get title

full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])   #Split the name to get Family name. 



# Do Families sink or swim together? 
full$FamilySize <- full$SibSp + full$Parch + 1  # Create a family size variable
full$Family <- paste(full$Surname, full$FamilySize, sep = "_")



# Logistic Regression
fit.logit <- glm(Survived~Sex + Age + Pclass + Fare + Embarked, data = train, family = binomial())
summary(fit.logit)

prob <- predict(fit.logit, test, type = "response")
logit.pred <- factor(prob > .5, levels = c(FALSE, TRUE),
                     labels = c("NOT Survived","Survived"))
logit.perf <- table(validate$Survived, logit.pred, dnn = c("Actual", "Predicted"))
margin.table(logit.perf) 

# Refine Model
logit.fit.reduced <- step(fit.logit)
prob_reduced <-  predict(logit.fit.reduced, test, type = "response")
logit.pred_reduced <- factor(prob_reduced > .5, levels = c(FALSE, TRUE),
                             labels = c("NOT Survived","Survived"))
logit.perf_reduced <- table(validate$Survived, logit.pred_reduced, dnn = c("Actual", "Predicted"))
logit.perf_reduced




#########################################DECISION TREE##########################################
library(rpart)
set.seed(1234)
train$Sex <- as.numeric(train$Sex)
test$Sex <- as.numeric(test$Sex)
full$Sex <- as.numeric(full$Sex)
df$Sex <- as.numeric(df$Sex)

dtree <- rpart(Survived ~ Sex + Age + Pclass + Fare + Embarked, data = train, method = "class",
               parms = list(split = "information"))
summary(dtree)

dtree$cptable
plotcp(dtree)

dtree.pruned <- prune(dtree, cp = .016)
library(rpart.plot)
prp(dtree.pruned, type = 2, extra = 104, 
    fallen.leaves = TRUE, main = "Decision Tree")

dtree.pred <- predict(dtree.pruned, test, type = "class")
dtree.perf <- table(validate$Survived, dtree.pred, 
                    dnn = c("Actual", "Predicted"))
dtree.perf

####################################Random Forest#######################################
library(randomForest)
set.seed(1234)
fit.forest <- randomForest(Survived ~ Sex + Age + Pclass + Fare, data = train,
                           na.action = na.roughfix,
                           importance = TRUE)
fit.forest
importance(fit.forest, type = 2)
levels(df$PassengerId) <- levels(train$PassengerId)
forest.pred <- predict(fit.forest, df)
forest.pred
forest.perf <- table(test$Survived, forest.pred, dnn = c("Actual", "Predicted"))
forest.perf

####################################Support Vector Machines#######################################
library(e1071)
set.seed(1234)
fit.svm <- svm(Survived ~ Sex + Age + Pclass + Fare, data = train)
fit.svm
svm.pred <- predict(fit.svm, test)
svm.perf <- table(na.omit(df$Survived), svm.pred,
                  dnn = c("Actual", "Predicted"))
svm.perf


