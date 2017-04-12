# Sentimen Analysis with "Sentiment" package
library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud2)
library(RColorBrewer)
install.packages("devtools")
library(devtools)
install.packages("Rstem", repos = "http://www.omegahat.net/R")
install.packages("sentiment_0.2.tar.gz", repos = NULL, type = "source")

download.file(url = "http://curl.haxx.se/ca/cacert.pem",
              destfile = "cacert.pem")
setup_twitter_oauth('PlRTalrAQhW70S23Lw3ASi53C', # api key
                    'j7WGgEsyx3it70OwbwzFevJOZOo1kKeu1OZmxRpVUy0cd17bKn', # api secret
                    '1202804742-Qc3DnZBXNwTHmUtoAZ6CiKu2AoBlMcNT5ccgnHe', # access token
                    'aDvCRTYKW4ygye48c1nbeRnnyMMzPFlq91rksDfls9cCT' # access token secret
)

UA_tweets <- twListToDF(searchTwitter("#unitedAIRLINES", n=1500, 'en'))
UA_tweets_txt <- gettext(UA_tweets$text)
UA_tweets_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", UA_tweets_txt) # remove retweet entities
UA_tweets_txt <- gsub("@\\w+", "", UA_tweets_txt) #remove at people
UA_tweets_txt <- gsub("[[:punct:]]", "", UA_tweets_txt) #remove punctuations
UA_tweets_txt <- gsub("[[:digit:]]", "", UA_tweets_txt) # remove numbers
UA_tweets_txt <- gsub("http\\w+", "", UA_tweets_txt) # remove html links
UA_tweets_txt <- gsub("[ \t]{2,}", "", UA_tweets_txt)
UA_tweets_txt <- gsub("^\\s+|\\s+$", "", UA_tweets_txt) # remove spaces where there are more than 2
UA_tweets_txt <- UA_tweets_txt[!is.na(UA_tweets_txt)] 

# Classify Emotion
class_emo <- classify_emotion(UA_tweets_txt, algorithm = "bayes", prior=1.0)
