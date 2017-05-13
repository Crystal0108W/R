# Notes:

# 1. What's the typical span of time between first visit and second visit?

# Summary of dataset
Wali <- read.csv("/Users/yw7986/Desktop/Wali.csv")
Wali[, 8] <- NULL

typelist <- c(1,2,3,4,5)
TUser <- c()
for (t in typelist) {
  values = length(unique(Wali$userId[Wali$type == t]))
  TUser[t] <- values
}
TUser <- as.data.frame(TUser)
TUser[6,] <- length(unique(Wali$userId))
rownames(TUser) <- c("Type1", "Type2", "Type3", "Type4", "Type5", "Total Unique Customers")
summary(TUser)

# There is no record of type 3 customers in our system, meaning no customer has ever tried to place an order online. Is it because we don't have a online ordering function ready? Or is it because this function can not be easily found by users?
# Most customers(6801) use our app to get stamp
# With 1823 user getting rewards on it. 
# 1503 users editted user profile in store and 81 users removed their stamp. 


# Looking at UserId - Times of Interaction
library(ggplot2)
UserId <- table(Wali$userId)
UserId <- as.data.frame(UserId)
UserId_Freq <- table(UserId$Freq)
UserId_Freq <- as.data.frame(UserId_Freq)
colnames(UserId_Freq) <- c("Times of Interaction", "Freq")
colnames(UserId) <- c("UserId", "Freq")
ggplot(data = UserId, aes(x = Freq)) + geom_histogram(binwidth = 2) + 
  labs(x = "Times of Interaction", title = "Histogram of Interaction\n") + 
  theme(plot.title = element_text(hjust = 0.5, size = 20))
# We have most consumers using our apps for once or twice for now. 

# Looking at Type - What does each customer use our app for?
UserType <- aggregate(x = Wali$type, by = list(Wali$userId, Wali$type), FUN = "sum")
UserType$y <- UserType$x / UserType$Group.2
UserType$x <- NULL
colnames(UserType) <- c('UserId', 'Type','Number of Usage')
UserType <- UserType[order(UserType$UserId),]
head(UserType)

# Type4 - How many customers are editting their profile in store?
length(unique(Wali$userId[Wali$type == 4])) /length(unique(Wali$userId)) #20.9% of our total customers has at lease editted their profile in store. 
#type4custid <- Wali$userId[Wali$type == 4]
#Wali$pos <-  Wali$userId %in% type4custid
#type4cust <- subset(Wali, Wali$pos == TRUE)
#Wali$pos <- NULL
#type4cust$pos <- NULL

# Type5 - Why are people removing stamps? What happened before that? 
type5customer <- subset(Wali, Wali$type == 5)
type5custid <- Wali$userId[Wali$type == 5]
Wali$pos <-  Wali$userId %in% type5custid
type5cust <- subset(Wali, Wali$pos == TRUE)
type5cust$pos <- NULL
ggplot(type5customer, aes(x = Date)) + geom_bar()+ theme(axis.ticks.x = element_blank(),
                                                         axis.text.x=element_blank())


# Type1 - How many stamps are people typically getting?   
User_T1 <- subset(UserType, UserType$Type == 1)
table(User_T1$`Number of Usage`)
ggplot(User_T1, aes(x = User_T1$`Number of Usage`)) + geom_bar() + labs(x = "Type 1 Customer", title = "Histogram for Frequency of Using Wali App to Get Stamps\n") + 
  theme(plot.title = element_text(hjust = 0.5))
# Most Consumer used our app to get stamp for once(4539). The highest count of usage is 245.


# Type2 - How many people are engaged with our app in a deeper level? (Those people tend to become loyal customers in the future)  
User_T2 <- subset(UserType, UserType$Type == 2) #876 entries
table(User_T2$`Number of Usage`)
ggplot(User_T2, aes(x = User_T2$`Number of Usage`)) + geom_bar() + labs(x = "Type 2 Customer", title = "Histogram for Frequency of Using Wali App to Get Rewarded\n")
# Most Consumer used our app to get rewarded for once(534). The highest number of reward one customer got is 27.

# Conversion Rates
# Goal: Use app to get rewarded
# Definition: Count(type2)/Count(type1)
(ConvR <- length(unique(Wali$userId[Wali$type == 2])) / length(unique(Wali$userId[Wali$type == 1])))
# 12.88% consumers who have used our app to get stamps has been successfully reached a certain amount of stamps to get reward for it. Out of the total 876 entries,
# 534 users only use reward for once, while 342 users (39%) got rewarded for more than once. That means once our customer tasted the benefit of getting rewarded,
# they tend to keep using our app to try to get more benefits on it. 

# Type1 to Type2
UserId_T1 <- User_T1$UserId
UserId_T2 <- User_T2$UserId
User_Conv <- subset(Wali, Wali$userId %in% UserId_T1 & Wali$userId %in% UserId_T2)
User_Conv$Date <- as.Date(User_Conv$Date, "%m/%d/%y")
length(unique(User_Conv$userId[User_Conv$type == 1]))
length(unique(User_Conv$userId[User_Conv$type == 2]))

timediff <- c()
for (id in unique(User_Conv$userId)) {
  df <- subset(User_Conv, User_Conv$userId == id)
  t1 = subset(df, df$type == 1)
  t2 = subset(df, df$type == 2)
  td <- min(t2[,4]) - min(t1[,4])
  timediff <- c(timediff, td)
}
timediff <- as.data.frame(timediff)

ggplot(data = timediff, aes(timediff)) + 
  geom_histogram(bins = 50, binwidth = 1, aes(y=..density..)) + 
  geom_density() + 
  scale_x_continuous(limits=c(0, 255), breaks=seq(0,255,10)) + 
  labs(x = "time difference", title = "Days between Getting First Stamp and First Reward") + 
  theme(plot.title = element_text(hjust = 0.5))
# It typically takes about 30 to 50 days for most customers to get their first reward. So for each customer, the next 1 to 2 months after they got their first stamp can be 
# crutial in terms of transfering them to a loyal user. That can work as an implication for future marketing efforts. 





















