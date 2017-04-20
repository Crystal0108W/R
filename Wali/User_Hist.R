# Prepare Data
user_hist_agg <- read.csv("/Users/yw7986/Desktop/users_history.csv")
user_hist_all <- user_hist_agg[,-1, drop = FALSE]
user_unknown <- subset(user_hist_all, user_hist_all$createdOn == "12/28/15")
user_createdtime <- subset(user_hist_all, user_hist_all$createdOn != "12/28/15")
rownames(user_createdtime) <- NULL

# Clean Data "user_createdtime"
user_createdtime$Date <- substr(user_createdtime$createdOn,1,10)
user_createdtime$Date <- as.Date(user_createdtime$Date)
user_createdtime$Time <- substr(user_createdtime$createdOn,12,19)
user_createdtime$DOW <- weekdays(as.Date(user_createdtime$Date), abbreviate = TRUE)
user_createdtime$TOD <- substr(user_createdtime$Time, 1,2)
user_createdtime$Month <- substr(user_createdtime$Date,6,7)
install.packages("zoo")
library(zoo)
yq <- as.yearqtr(as.yearmon(user_createdtime$Date) + 1/12)
user_createdtime$Season <- factor(format(yq, "%q"), levels = 1:4, 
                                  labels = c("winter", "spring", "summer", "fall"))
user_createdtime <- user_createdtime[, c("Date", "DOW", "Month", "Season", "Time", "TOD", "createdOn")] 
user_createdtime <- user_createdtime[order(Date, Time),]
user_createdtime$Date <- as.Date(user_createdtime$Date)
write.csv(user_createdtime, file = "/Users/yw7986/Desktop/User_CreatedTime.csv")

# Create Date "user_createdDate"
user_createdDate <- as.data.frame(table(user_createdtime$Date))
colnames(user_createdDate) <- c("Date", "Freq")
user_createdDate$CumFreq <- cumsum(user_createdDate$Freq)
user_createdtime$Date <- as.numeric(user_createdtime$Date)

# Check Patterns
install.packages("ggplot2")
library(ggplot2)
ggplot(user_createdDate, aes(Date, CumFreq, group = 1)) + geom_line()
  labs(title = "User Daily Growth Chart\n") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


# Linear Model
user_createdDate$Days <- rownames(user_createdDate)
user_createdDate$Days <- as.numeric(user_createdDate$Days)
lm1<- lm(CumFreq~Days, data = user_createdDate)
summary(lm1)
plot(user_createdDate$Days, user_createdDate$CumFreq)
abline(lm(user_createdDate$CumFreq ~ user_createdDate$Days))
# Assessing Outliers
plot(lm1)
user_createdDate_normal1 <- user_createdDate[1:30, ]
user_createdDate_normal2 <- user_createdDate[-c(1:32), ]
lm2 <- lm(CumFreq~Days, data = user_createdDate_normal1)
summary(lm2)
plot(lm2)

lm3 <- lm(CumFreq~Days, data = user_createdDate_normal2)
summary(lm3)
install.packages("car", type = "source")
library(car)
lm4 <- lm(logit(CumFreq)~Days, data = user_createdDate_normal2)


# Poisson Regression
m4 <- glm(CumFreq~Days, family = poisson(link = log), data = user_createdDate)
summary(m4)
plot(m4)

# Negative Binomial Model
install.packages("MASS")
library(MASS)
m5 <- glm.nb(CumFreq~Days, data = user_createdDate)
summary(m5)
#3M, 6M, 12M
Days <- c(414, 504, 869)
predict(lm3, newdata = data.frame(Days))
predict(m4, newdata = data.frame(Days))
predict(m5, newdata = data.frame(Days))









