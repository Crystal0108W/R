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



# Transaction Time






############################################################################################################################
install.packages("ggplot2")
install.packages("plotly")
library(ggplot2)
library(plotly)
summary(WaliCust)
str(WaliCust)


# Transform Data Type
WaliCust$type <- as.factor(WaliCust$type)
WaliCust$DOW <- factor(WaliCust$DOW, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))


# DOW Bar Chart
ggplot(data = WaliCust, aes(x = WaliCust$DOW, fill = WaliCust$type)) + geom_bar(width = 0.5) + 
  labs(x = "Day of Week", title = "Number of Customer in Day of Week\n") + 
  scale_fill_discrete(guide = guide_legend(title = "Type")) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "bottom")

# TOD Bar Chart
WaliCust$TOD <- substr(WaliCust$Time, 1,2)

ggplot(data = WaliCust, aes(x = WaliCust$TOD, fill = WaliCust$type)) + geom_bar(width = 0.5) + 
  labs(x = "Time of Day", title = "Number of Customer in Time Period\n") + 
  scale_fill_discrete(guide = guide_legend(title = "Type")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "bottom")

# Frequency of Type One
WaliCust.type1Freq <- subset(WaliCust, WaliCust$type == 1)
table(droplevels(WaliCust.type1Freq$type), WaliCust.type1Freq$userId) # use droplevels to drop unused level in Table
table(factor(WaliCust.type1Freq$type), WaliCust.type1Freq$userId) # Or use factor() to drop unused level in Table, none of them will change the levels in the dataframe
str(WaliCust$type) # none of them will change the levels in the dataframe

WaliCust.type1Freq <- as.data.frame(table(factor(WaliCust.type1Freq$type), WaliCust.type1Freq$userId))
WaliCust.type1Freq$Var1 <- NULL
names(WaliCust.type1Freq)[1] <- "userId"

ggplot(data = WaliCust.type1Freq, aes(x = WaliCust.type1Freq$Freq)) + 
  geom_histogram(aes(fill = ..count..)) + 
  scale_fill_gradient("count", low = "pink", high = "red") + 
  labs(title = "Frequency of Using Punch Card for Type1 Customer\n", x ="Frequency") + 
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        plot.margin = unit(c(1,1,1,1), "cm"))
table(WaliCust.type1Freq$Freq) # Freq Table


WaliCust.type1 <- subset(WaliCust, WaliCust$type == 1)
WaliCust.type2 <- subset(WaliCust, WaliCust$type == 2)

install.packages("reshape")
library(reshape)
WaliCust <- WaliCust[, c(1,3,4,5,6,7,8,2)]
WaliCust.wide <- cast(WaliCust, userId~ type, fill=FALSE)
colnames(WaliCust.wide) <- c("userId", "Type1", "Type2")
WaliCust.wide$nine <- (WaliCust.wide$Type1)/9
WaliCust.wide$ten <- (WaliCust.wide$Type1)/10

WaliCust.wide$Cluster <- 0
len <- nrow(WaliCust.wide)

i = 1
for (i in 1:558) {
if (WaliCust.wide[i,3] >= WaliCust.wide[i,5] & WaliCust.wide[i,2] > 8) {
  WaliCust.wide[i,6] = 1
} else if (WaliCust.wide[i,3] >= WaliCust.wide[i,5] & WaliCust.wide[i,2] < 8){
  WaliCust.wide[i,6] = 2
} else if (WaliCust.wide[i,3] < WaliCust.wide[i,5] & WaliCust.wide[i,2] > 8){
  WaliCust.wide[i,6] = 3
} else WaliCust.wide[i,6] = 4
}
WaliCust.wide$Cluster <- as.factor(WaliCust.wide$Cluster)
str(WaliCust.wide)

install.packages("wesanderson")
library(wesanderson)
ggplot(data = WaliCust.wide) + geom_point(aes(x = Type1, y =Type2, color = factor(Cluster))) + 
    geom_line(aes(x = Type1, y = ten), col = "grey", linetype = 2) + 
    theme(plot.title = element_text(hjust = 0.5, size = 15), 
          plot.margin = unit(c(1,1,1,1), "cm")) + 
  labs(x = "Count of type1 Usage", y = "Count of type2 usage", title = "Count of Type1 and Type2 Customer\n", color = "Cluster") + 
  scale_color_manual(values=wes_palette(n=3, name="Darjeeling")) + 
  geom_vline(xintercept = 8.5, col = "grey", linetype = 2)
