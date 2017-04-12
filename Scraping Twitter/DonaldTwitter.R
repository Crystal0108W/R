install.packages("twitteR")
install.packages("devtools")
install.packages("rjson")
install.packages("bit64")
install.packages("httr")
library(twitteR)
library(devtools)
library(rjson)
library(bit64)
library(httr)

consumer_key <- "PlRTalrAQhW70S23Lw3ASi53C"
consumer_secret <- "j7WGgEsyx3it70OwbwzFevJOZOo1kKeu1OZmxRpVUy0cd17bKn"

setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret)

donald_tweets <- userTimeline("realDonaldTrump", n = 3200)
tweetsc.df <- twListToDF(donald_tweets)
date <- Sys.Date()
data <- as.character(date)
name <- paste(data, ".RData")
save(tweetsc.df, file = name)

install.packages("taskscheduleR")
library(taskscheduleR)

taskscheduler_create(taskname = "taskdonald", rscript = donald_tweets, schedule = "DAILY", 
                     starttime = "11:30", startdate = format(Sys.Date(), "%d%m%y"))