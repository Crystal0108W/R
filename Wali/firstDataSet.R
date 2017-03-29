WaliCust <- read.csv("/Users/yw7986/Desktop/firstDataSet.csv")
WaliCust$date <- as.character(WaliCust$date)
Date_Time <- strsplit(WaliCust$date, "T")
Date_Time <- as.data.frame(Date_Time)
Date_Time <- t(Date_Time)
Date_Time <- as.data.frame(Date_Time)
Date_Time$user.id <- row.names(Date_Time)
row.names(Date_Time) <- seq(1:nrow(Date_Time))
colnames(Date_Time)[1] <- c("Date")
colnames(Date_Time)[2] <- c("Time")
Date_Time <- Date_Time[, c(3,1,2)]

Date_Time$Time <- gsub(pattern = ".000Z", replacement = "", Date_Time$Time)
WaliCust <- cbind(WaliCust, Date_Time$Date, Date_Time$Time)
row.names(WaliCust) <- seq(1:nrow(WaliCust))
summary(WaliCust)
WaliCust.df <- WaliCust[, c(1,2,5,6,4)]
colnames(WaliCust.df)[3] <- c("Date")
colnames(WaliCust.df)[4] <- c("Time")
str(WaliCust.df)
WaliCust.df$type <- as.factor(WaliCust.df$type)

# Unique number of customers
length(table(WaliCust.df$userId)) # Unique number of customers = 558
unique(WaliCust.df$userId)   # 558 levels

# Sort WaliCust.df
WaliCust.df <- WaliCust.df[order(WaliCust.df$userId, WaliCust.df$userId), ]
WaliCust.Freq <- table(WaliCust.df$userId)
WaliCust.Freq <- as.data.frame(WaliCust.Freq)
WaliCust.Freq <- WaliCust.Freq[order(WaliCust.Freq$Var1),]
WaliCust.df <- cbind(WaliCust.Freq$Freq, WaliCust.df)

WaliCust.Freq_type <- table(WaliCust.df$userId, WaliCust.df$type)
WaliCust.Freq_type <- as.data.frame(WaliCust.Freq_type)
colnames(WaliCust.Freq_type) <- c("userid", "Type", "Freq")


# Visualize Type1 Frequency
install.packages("ggplot2")
library(ggplot2)

WaliCust.Freq_type1 <- subset(WaliCust.Freq_type, Type == "1")
WaliCust.Freq_type2 <- subset(WaliCust.Freq_type, Type == "2")
summary(WaliCust.Freq_type1)
ggplot(data = WaliCust.Freq_type1, aes(x = WaliCust.Freq_type1$Freq)) + 
  geom_histogram(aes(fill = ..count..), binwidth = 1) + 
  labs(x = "Frequency", title = "Histogram") + 
  scale_fill_gradient("Count", low = "pink", high = "red")



# Date Bar Chart
Date_Freq <- as.data.frame(table(WaliCust.df$Date))
colnames(Date_Freq) <- c("Date", "Freq")
ggplot(data = WaliCust.df, aes(x = WaliCust.df$Date)) + geom_bar(aes(fill = WaliCust.df$type )) + 
  theme(legend.position = c(0.1,0.9), legend.background = element_rect(fill = NULL)) + guides(fill=guide_legend(title="Type")) 


# Date Point
ggplot(data=Date_Freq, aes(x=Date_Freq$Date, y=Date_Freq$Freq, group=Date_Freq$Date)) +
  geom_point()


# Transaction Time
WaliCust.df$Time[1] - WaliCust.df$Time[2]





############################################################################################################################
install.packages("ggplot2")
library(ggplot2)
summary(WaliCust)
str(WaliCust)


# Transform Data Type
WaliCust$type <- as.factor(WaliCust$type)
WaliCust$DOW <- factor(WaliCust$DOW, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


# DOW Bar Chart
ggplot(data = WaliCust, aes(x = WaliCust$DOW, fill = WaliCust$type)) + geom_bar() + 
  labs(x = "Day of Week") + 
  scale_fill_discrete(guide = guide_legend(title = "Type"))

