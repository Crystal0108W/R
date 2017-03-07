x <- c(1,2,3,4,5,6,7,8)
mean(x)
sd(x)

n<-length(x)
meanx <- sum(x)/n
css <- sum((x-meanx)^2)
sdx <- sqrt(css/(n-1))
meanx
sdx


x <- pretty(c(-3,3),30)
y <- dnorm(x)
plot(x,y, 
     type = "l", 
     xlab = "Normal Deviate", 
     ylab = "Density",
     yaxs = "i"
)

a<- pnorm(1.96)

b<- qnorm(.9, mean = 500, sd = 100)

c<- rnorm(50, mean = 50, sd = 10)


mydata<-matrix(rnorm(30),nrow = 6)

apply(mydata, 2, mean, trim=0.2)

mydata


#################CHALLENGE######################
options(digits = 2)

Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Student, Math, Science, English,
                       stringsAsFactors=FALSE)


z <- scale(roster[,2:4])
score <- apply(z, 1, mean)
roster <- cbind(roster, score)

y <- quantile(roster$score, c(0.2, 0.4, 0.6, 0.8))

attach(roster)
roster$grade[score>=y[4]] <- "A"
roster$grade[score>=y[3] & score <y[4]] <- "B"
roster$grade[score>=y[2] & score <y[3]]<-"C"
roster$grade[score>=y[1] & score <y[2]]<-"D"
roster$grade[score <y[1]]<-"F"
detach()

name<-strsplit(roster$Student," ")
firstname <- sapply(name, "[", 1)
lastname <- sapply(name, "[", 2)
firstname
lastname
roster <- cbind(firstname, lastname, roster[-1])
roster <- roster[order(lastname, firstname),]
roster

###################################################
feelings <- c("sad","afraid")
for (i in feelings) 
  print(
    switch (i,
      happy = "I am glad you are happy",
      afraid = "There is nothing to fear",
      sad = "Cheer up!",
      angry = "Calm down now"
    )
  )
##################################################
cars<- mtcars[1:5,1:4]
cars

options(digits = 3)
attach(mtcars)
aggdata <- aggregate(mtcars, by = list(cyl,gear), FUN = mean, na.rm = TRUE)
aggdata

#################################################
install.packages("reshape2")
library(resshape2)

md <- melt(cars, id="hp")
 




