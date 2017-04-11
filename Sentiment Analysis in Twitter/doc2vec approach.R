install.packages("twitteR")
install.packages("ROAuth")
install.packages("tidyverse")
install.packages("text2vec", dependencies = TRUE)
install.packages("caret")
install.packages("glmnet")
install.packages("ggrepel")
library(twitteR)
library(ROAuth)
library(tidyverse)
library(text2vec)
library(caret)
library(glmnet)
library(ggrepel)

conv_fun <- function(x) iconv(x, "latin1", "ASCII", "")
tweets_classified <- read_csv("/Users/yw7986/Desktop/training.csv", col_names = c('sentiment', 'id', 'date', 'query', 'user', 'text')) %>%
  dmap_at('text', conv_fun) %>%
  mutate(sentiment = ifelse(sentiment == 0, 0 ,1))

# data split on train and test
set.seed(2340)
trainIndex <- createDataPartition(tweets_classified$sentiment, p = 0.8, list = FALSE, times = 1)
tweets_train <- tweets_classified[trainIndex, ]
tweets_test <- tweets_classified[-trainIndex, ]

#define processing functions and tokenization function in doc2vec
prep_fun <- tolower
tok_fun <- word_tokenizer

it_train <- itoken(tweets_train$text, 
                   preprocessor = prep_fun, 
                   tokenizer = tok_fun, 
                   ids = tweets_train$id,
                   progressbar = TRUE)

it_test <- itoken(tweets_test$text, 
                  preprocessor = prep_fun,
                  tokenizer = tok_fun,
                  ids = tweets_test$id,
                  progressbar = TRUE)

# create vocabulary adn document-term matrix
vocab <- create_vocabulary(it_train)
vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dtm_test <- create_dtm(it_test, vectorizer)

#define tf-idf model
tfidf <- TfIdf$new()
#fit the model to the train data and transform it with the fitted model
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
dtm_test_tfidf <- fit_transform(dtm_test, tfidf)

#train the model
t1 <- Sys.time()
glmnet_classifer <- cv.glmnet(x = dtm_train_tfidf, y = tweets_train[['sentiment']],
                              family = 'binomial',
                              alpha = 1, 
                              type.measure = "auc",
                              nfolds = 5,
                              thresh = 1e-3, 
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'mins'))

plot(glmnet_classifer)
print(paste("max AUC =", round(max(glmnet_classifer$cvm), 4)))



