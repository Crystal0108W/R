topsites <- read.csv("/Users/yw7986/Desktop/topsite.csv", stringsAsFactors = F)
ggplot(topsites, aes(x = PageViews, y = UniqueVisitors)) + geom_point()
ggplot(topsites, aes(x = log(PageViews))) + geom_density()
ggplot(topsites, aes(x = log(PageViews), y = log(UniqueVisitors))) + geom_point() +
  geom_smooth(method = "lm")

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors) + HasAdvertising + InEnglish, data = topsites)
summary(lm.fit)

# Methods to prevent overfitting: Cross-Validation(loop degree from 1 to the total number of variables) and Regularization (lambda)

oreilly <- read.csv("/Users/yw7986/Desktop/Oreilly.csv", stringsAsFactors = F)
install.packages("tm")
library("tm")
library(NLP)
documents <- data.frame(Text = oreilly$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, content_transformer(tolower)) # tolower step isn't a "canonical" transformation 
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
inspect(corpus)

dtm <- DocumentTermMatrix(corpus)
temp <- as.data.frame(findFreqTerms(dtm, 10))  

x <- as.matrix(dtm)
y <- rev(1:100)

set.seed(1)
install.packages("glmnet")
library(glmnet)

performance <- data.frame()
for (lambda in c(0.1, 0.25, 0.5, 1, 2, 5))
{
  for (i in 1:50)
  {
    indices <- sample(1:100, 80)
    training.x <- x[indices, ]
    training.y <- y[indices]
    
    test.x <- x[-indices, ]
    test.y <- y[-indices]
    
    glm.fit <- glmnet(training.x, training.y)
    predicted.y <- predict(glm.fit, test.x, s = lambda)
    rmse <- sqrt(mean((predicted.y - test.y)^2))
    
    performance <- rbind(performance, data.frame(Lambda = lambda, 
                                                 Iteration = 1, 
                                                 RMSE = rmse))
  }
}


install.packages("Hmisc")
library(Hmisc)
ggplot(performance, aes(x = Lambda, y = RMSE)) + 
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') + 
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') # Too general, not informing


# Predict if the book is in the top 50 or not, Logistic Regression
y <- rep(c(1,0), 50)
regularized.fit <- glmnet(x,y,family = 'binomial')
predict(regularized.fit, newx = x, s = 0.001)
install.packages("boot")
library(boot)

inv.logit(predict(regularized.fit, newx = x, s = 0.001)) # transfer it to probability

set.seed(1)
performance <- data.frame()
for (i in 1:250)
{
  indices <- sample(1:100, 80) 
  training.x <- x[indices, ] 
  training.y <- y[indices]
  
  test.x <- x[-indices, ] 
  test.y <- y[-indices]
  
  for (lambda in c(0.0001, 0.001, 0.0025, 0.005, 0.01, 0.025, 0.5, 0.1)) {
    glm.fit <- glmnet(training.x, training.y, family = 'binomial') 
    predicted.y <- ifelse(predict(glm.fit, test.x, s = lambda) > 0, 1, 0) 
    error.rate <- mean(predicted.y != test.y)
    performance <- rbind(performance, data.frame(Lambda = lambda,
                                                 Iteration = i,
                                                 ErrorRate = error.rate))
  }
}

ggplot(performance, aes(x = Lambda, y = ErrorRate)) + 
  stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar') + 
  stat_summary(fun.data = 'mean_cl_boot', geom = 'point') + 
  scale_x_log10()


