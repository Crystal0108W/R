# Notes:

# 1. What's the typical span of time between first visit and second visit?




# Summary of dataset
Wali <- read.csv("C:/Users/Crystal/Desktop/Wali.csv")
Wali[, 8] <- NULL
summary(Wali)

# Looking at UserId - Times of Interaction
library(ggplot2)
UserId <- table(Wali$userId)
UserId <- as.data.frame(UserId)
UserId_Freq <- table(UserId$Freq)
UserId_Freq <- as.data.frame(UserId_Freq)
colnames(UserId_Freq) <- c("Times of Interaction", "Freq")
colnames(UserId) <- c("UserId", "Freq")
ggplot(data = UserId, aes(x = Freq)) + geom_histogram(binwidth = 2)
# Most people interact with our app for once(3776) or twice(1154) and then stopped.

# Looking at Type - What so customers use our app for?
UserType <- aggregate(x = Wali$type, by = list(Wali$userId, Wali$type), FUN = "sum")
UserType$y <- UserType$x / UserType$Group.2
UserType$x <- NULL
colnames(UserType) <- c('UserId', 'Type','Number of Usage')
# There is no record of type 3 customers in our system, meaning no customer has ever tried to place an order online.
# Is it because we don't have a online ordering function ready? Or is it because this function can not be easily found by users?
# Most customers(20367) use our app to get stamp
# With 1823 user getting rewards on it. 
# 1503 users editted user profile in store and 81 users removed their stamp. 

# Type4 - How many customers are editting their profile in store?
length(unique(Wali$userId[Wali$type == 4])) #1425 customers has editted their profile in store for at least once. 
length(unique(Wali$userId)) #6809 total unique number of customers
length(unique(Wali$userId[Wali$type == 4])) /length(unique(Wali$userId)) #20.9% of our total customers has at lease editted their profile in store. 
#type4custid <- Wali$userId[Wali$type == 4]
#Wali$pos <-  Wali$userId %in% type4custid
#type4cust <- subset(Wali, Wali$pos == TRUE)
#Wali$pos <- NULL
#type4cust$pos <- NULL

# Type5 - Why are people removing stamps? What happened before that? 
type5custid <- Wali$userId[Wali$type == 5]
Wali$pos <-  Wali$userId %in% type5custid
type5cust <- subset(Wali, Wali$pos == TRUE)
type5cust$pos <- NULL
ggplot(type5customer, aes(x = Date)) + geom_bar()+ theme(axis.ticks.x = element_blank(),
                                                                              axis.text.x=element_blank())
# Type1 - 
length(unique(Wali$userId[Wali$type == 1])) #6801 customers have used our app to get stamps. 
UserId_T1 <- subset(UserType, UserType$Type == 1)
ggplot(UserId_T1, aes(x = UserId_T1$`Number of Usage`)) + geom_histogram(binwidth = 5)
# Most Customer got stamp for once.








