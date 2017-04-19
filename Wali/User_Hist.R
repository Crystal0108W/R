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
yq <- as.yearqtr(as.yearmon(user_createdtime$Date) + 1/12)
user_createdtime$Season <- factor(format(yq, "%q"), levels = 1:4, 
                                  labels = c("winter", "spring", "summer", "fall"))
user_createdtime <- user_createdtime[, c(2,5,4,1,3)] 
user_createdtime <- user_createdtime[order(Date, Time),]

# Create Date "user_createdDate"
user_createdDate <- as.data.frame(table(user_createdtime$Date))
colnames(user_createdDate) <- c("Date", "Freq")
user_createdDate$CumFreq <- cumsum(user_createdDate$Freq)

?aggregate

# Check Patterns
ggplot(data = user_createdDate, aes(x = Date, y = Freq)) + geom_bar(stat = "identity")



