manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country <-c("US", "US", "UK","UK","UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32,45,25,39,99)
q1<- c(5,3,3,3,2)
q2<- c(4,5,5,3,2)
q3<- c(5,2,5,4,1)
q4<- c(5,5,5,NA,2)
q5<- c(5,5,2,NA,1)

leadership <- data.frame(manager, date, country, gender, age, q1,q2,q3,q4,q5, stringsAsFactors = FALSE)



###########################CREATE NEW VARIABLES#####################
mydata<-data.frame(x1 = c(1,2,3,4,5),
                   x2 = c(3,4,6,8,9))

mydata$sumx<- mydata$x1 + mydata$x2
mydata$meanx<- (mydata$x1 + mydata$x2)/2     #OPTION1

attach(mydata)
mydata$sumx <- x1+x2
mydata$meanx <- (x1+x2)/2
detach(mydata)                          #OPTION2


mydata<- transform(mydata, 
                   sumx = x1+x2,
                   meanx = (x1+x2)/2)   #OPTION3


###########################RECODING VARIABLES#####################

manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
country <-c("US", "US", "UK","UK","UK")
gender <- c("M", "F", "F", "M", "F")
age <- c(32,45,25,39,99)
q1<- c(5,3,3,3,2)
q2<- c(4,5,5,3,2)
q3<- c(5,2,5,4,1)
q4<- c(5,5,5,NA,2)
q5<- c(5,5,2,NA,1)

leadership <- data.frame(manager, date, country, gender, age, q1,q2,q3,q4,q5, stringsAsFactors = FALSE)

leadership$age[leadership$age==99] <-NA
leadership$age[leadership$age > 75] <- "Elder"
leadership$age[leadership$age >= 55 & leadership$age <=75] <- "Middle Aged"
leadership$age[leadership$age < 55] <- "Young"

leadership <- within(leadership,{
                     agecat <- NA
                     agecat[age>75]    <- "Elder1"
                     agecat[age>=55 & age<=75] <- "Middle Aged1"
                     agecat[age<55] <- "Young1" })


fix(leadership)   #RENAME VARIABLE
names(leadership)[2] <- "testDate"
names(leadership)[6:10] <- c("item1","item2","item3","item4","item5")

install.packages("plyr")
library(plyr)
leadership<- rename(leadership, c(managerID="manager", testDate="date"))
leadership

is.na(leadership[,6:10])

x <- c(1,2,NA,3)
y <- sum(x,na.rm = T)

newdata<- na.omit(leadership)  #na.omit delet rows/observations with missing data
newdata

mydate<- as.Date(c("2016-01-08", "2015-01-08"))
mydate

strDate<- c("01/08/1993", "01/02/1964")
dates<- as.Date(strDate, "%b/%d/%y")


Myformat<-c("%m-%d-%y")
leadership$date <- as.Date(leadership$date, Myformat)
leadership

today<-Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%a")
date()

today <- Sys.Date()
dob <- as.Date("1993-01-08")
ann <- as.Date("2013-12-27")
difftime(today,dob)
difftime(today,dob,units = "weeks")
format(dob, format="%a")

difftime(today,ann)
difftime(today,ann,units = "weeks")
A <- ann + 10000
A

strDates <- as.character(today)
typeof(strDates)

newdata<-leadership[order(leadership$age),]
newdata<-leadership[order(leadership$gender,leadership$age),]
newdata<-leadership[order(leadership$gender,-leadership$age),]
newdata


myvars<- paste("q", 1:5, sep = "")
newdata<-leadership[myvars]
newdata

ad<- read.csv("C:/Users/Crystal/Desktop/AdFX.csv")
hist(ad$past_sales)

newdata<- subset(leadership, age>=35 | age <24, select = c(item1,item2,item3,item4))
newdata

newdata<- subset(leadership, age>=24 & age <35, select = c(q1,q2,q3,q4))
newdata

install.packages("sqldf")

library(sqldf)
newdf<- sqldf



