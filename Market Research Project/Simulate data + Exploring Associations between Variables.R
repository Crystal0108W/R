############ Market Analysis Project ############ 

#1. Simulate a data set that describes

#1.1 Simulate Customer Data
  # 2000 Customers age, 
  # 2000 Customers age credit in store 
  # 2000 Customers email (yes or no) 
  # 2000 Customers distance to store (use lognormal distribution to simulate distance)
set.seed(1234)
ncust <- 2000
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))

cust.df$age <- rnorm(n=ncust, mean = 35, sd = 5)
cust.df$credit.score <- rnorm(n=ncust, mean = 3*cust.df$age + 500, sd = 50)
cust.df$email <- factor(sample(c("yes","no"), size = ncust, replace = TRUE, prob = c(0.7, 0.3)))
cust.df$distance <- exp(rnorm(n=ncust, mean = 2, sd = 1.2)) 
summary(cust.df)


#1.2 Simulate Online and In-Store Sales Data
  # Online visits (negative binomial distribution to simulate the counts)
  # tranaction 
  # totle spendings

# model the mean of the negative binomial with a baseline value of 20.
# The size argument sets the degree of dispersion (variation) for the samples. 
# For customers with email on file, an average 15 more online visits are added
# Lastly, assume that younger customers tend to visit online store more than older customers.
cust.df$online.visits <- rnbinom(ncust, size = 0.3, 
                                 mu = 20 + ifelse(cust.df$email =="yes", 15, 0) - 0.8 * (cust.df$age-median(cust.df$age)))

# For each online visit that a customer makes, we assume there is a 20 % chance of placing an order and use rbinom() to create the variable online.trans.
cust.df$online.trans <- rbinom(ncust, size = cust.df$online.visits, prob = 0.2)
cust.df$online.spend <- exp(rnorm(ncust, mean = 3, sd = 0.1)) * cust.df$online.trans

# Repeat those steps for in-store sales
cust.df$store.trans <- rnbinom(ncust, size=5, mu=3 / sqrt(cust.df$distance))
cust.df$store.spend <- exp(rnorm(ncust, mean=3.5, sd=0.4)) * cust.df$store.trans
summary(cust.df)


#1.2 Simulate Satisfaction Survey Responses

#Assume that overall satisfaction is a psychological construct that is not directly observable.
sat.overall <- rnorm(ncust, mean=3.1, sd=0.7)
sat.overall
summary(sat.overall)
sat.service <- floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6))
summary(cbind(sat.service, sat.selection))

# Keep the satisfaction score between 1 to 5 
sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1
summary(cbind(sat.service, sat.selection))


#1.3 Simulate Non-Response Dta
no.response <- as.logical(rbinom(ncust, size = 1, prob = cust.df$age/100)) # Assume the older, the higher probability to answer survey
sat.service[no.response] <- NA
sat.selection[no.response] <- NA
summary(cbind(sat.service, sat.selection))

#1.4 FInalize Data Simulation
cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection
summary(cust.df)



#2. Exploring Associations Between Variables with ScatterPlots
str(cust.df)

plot(x=cust.df$age, y = cust.df$credit.score,
     main = "Active Customer as of Feb 2017",
     xlab = "Age", ylab = "Credit in Store", col = "pink")
abline(h = mean(cust.df$credit.score), col = "salmon", lty=2)
abline(v = mean(cust.df$age), col = "salmon", lty=2)

plot(x = cust.df$store.spend, y = cust.df$online.spend,
     main = "Customers Spending as of Feb 2017", 
     xlab = "Prior 12 months in-store sales ($)",
     ylab= "Prior 12 months online sales ($)", 
     col = "pink",
     cex = 0.7)

hist(cust.df$store.spend,
     breaks = (0:ceiling(max(cust.df$store.spend)/10)) * 10,
     main = "Customer as of Feb 2017",
     xlab = "Prior 12 months online sales ($)",
     ylab="Count of customers",
     col = "pink",
     border = "white",
     xlim = c(0, 500),
     ylim = c(0, 800))
axis(1, col = "grey", col.axis = "dark grey", lwd = 0.7)
axis(2, col = "grey", col.axis = "dark grey", lwd = 0.7)

mycol <- c("cyan", "pink")
mypch <- c(3,17)
mycol[head(cust.df$email)]

plot(cust.df$store.spend, cust.df$online.spend,
     cex = 0.7,
     col = mycol[cust.df$email], pch = mypch[cust.df$email],
     main = "Customers as of Feb 2017",
     xlab= "Prior 12 months in-store sales ($)",
     ylab= "Prior 12 months online sales ($)")
axis(1, col = "grey", col.axis = "dark grey", lwd = 0.7)
axis(2, col = "grey", col.axis = "dark grey", lwd = 0.7)
legend(x="topright",legend = paste("email on file:", levels(cust.df$email)),
       col = mycol, pch = mypch,
       bty = "n", pt.cex = 1, text.col = mycol)

ggplot(cust.df, aes(x = store.spend, y = online.spend, shape = email, col = email)) + 
  geom_point(size = 3, alpha = 0.5) + 
  theme(plot.title = element_text(face="bold", size = 20, hjust = 0.5), plot.margin = unit(c(1,1,1,1), "cm"))+
  labs(title = "Customer as of Feb 2017", x = "Prior 12 months in-store sales ($)", y = "Prior 12 months online sales ($)")

?theme
