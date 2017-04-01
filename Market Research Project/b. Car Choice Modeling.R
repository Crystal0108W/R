# Choice Modeling - Multinomial Logit Model 

cbc.df <- read.csv("http://goo.gl/5xQObB", colClasses = c(seat = "factor", price = "factor"))
install.packages("ChoiceModelR")
summary(cbc.df)

xtabs(choice~price, data = cbc.df)
xtabs(choice~cargo, data = cbc.df) # the choices were more balanced between the two options, suggesting that cargo was not as important to customers as price
xtabs(choice~carpool, data = cbc.df)
xtabs(choice~seat, data = cbc.df)
xtabs(choice~eng, data = cbc.df)


# Fitting Choice Models with mlogit()
install.packages("mlogit")
library(mlogit) 


# mlogit requires the choice data to be in a special data format
cbc.mlogit <- mlogit.data(data = cbc.df, choice = "choice", shape = "long", 
                          varying = 3:6, alt.levels = paste("pos", 1:3),
                          id.var = "resp.id") # alt.levels: “fixed” alternatives, in this case, it is position(left, right or middle)


m1 <- mlogit(choice ~ 0 + seat + cargo + eng + price, data = cbc.mlogit) # no intercept
summary(m1)
m2 <- mlogit(choice ~ seat + cargo + eng + price, data = cbc.mlogit) # with intercept
summary(m2)

# In m2 the intercept parameter estimates are non-significant and close to zero.
# This suggests that it was reasonable to leave them out of our first model, but we can test this formally using lrtest()
lrtest(m1, m2) # chi-square test, not significant 

m3 <- mlogit(choice ~ 0 + seat + cargo + eng + as.numeric(as.character(price)),data = cbc.mlogit) # If you use as.numeric without as.character first, price will be converted to the values 1, 2, and 3 due to the way R stores factors internally.
summary(m3) # now the output shows a single parameter for price
# The estimate is negative indicating that people prefer lower prices to higher prices.

lrtest(m1, m3) # no significant difference


# Report choice model with Willingness-To-Pay (WTP)
coef(m3)["cargo3ft"] / (-coef(m3)["as.numeric(as.character(price))"]/1000)
coef(m3)["seat7"] / (-coef(m3)["as.numeric(as.character(price))"]/1000) # when seat 7 cost $3084.38 less than seat6, people become different

# on average, customers would be equally divided between a minivan with 2 ft of cargo space and a minivan with 3 ft of cargo space that costs $2750.60 more.
# Another way to think of it is that $2750.60 is the price at which customers become indifferent between the two cargo capacity options. 

# Choice Shares to make share predictions.
# use the model to predict choice share for the company’s new minivan design against a set of key competitors.

#predict shares
predict.mn1 <- function(model, data) {
  data.model<- model.matrix(update(model$formula, 0~.), data = data)[, -1]
  utility <- data.model %*% model$coef
  share <- exp(utility) / sum(exp(utility))
  cbind(share, data)
}

# create new data design
new.data <- expand.grid(attrib)[c(8, 1, 3, 41, 49, 26), ] # what customers would choose if they had to pick among these six minivan alternatives
predict.mn1(m3, new.data) # the share would change if the competitive set were different.
predict.mn1(m1, new.data)


# Sensitivity Plot
# The model can be used to predict how share would change if different levels of the attributes were included (while keeping the competitive set fixed.)
sensitivity.mn1 <- function(model, attrib, base.data, competitor.data) {
  data <- rbind(base.data, competitor.data)
  base.share <- predict.mn1(model, data) [1,1]
  share <- NULL 
  for(a in seq_along(attrib)) {
    for (i in attrib[a]) {
      data[1, ] <- base.data
      data[1, a] <- i 
      share <- c(share, predict.mn1(model, data)[1,1])
    }
  }
  data.frame(level = unlist(attrib), share = share, increase = share - base.share)
}

base.data <- expand.grid(attrib)[c(8), ]
competitor.data <- expand.grid(attrib)[c(1,3,41,49,26), ]
tradeoff <- sensitivity.mn1(m1, attrib, base.data, competitor.data)


# Heterogeneous logit model
m1.rpar <- rep("n", length=length(m1$coef))
names(m1.rpar) <- names(m1$coef)
m1.hier <- mlogit(choice ~ 0 + seat + eng + cargo + price,
                  data = cbc.mlogit,
                  panel=TRUE, rpar = m1.rpar, correlation = FALSE)
summary(m1.hier)
# The standard deviation parameter estimates indicate that there is a lot of hetero- geneity in preference for 7 or 8 seats over 6 seats.
m2.hier <- update(m1.hier, correlation = TRUE)
# Model m1 assumed that there were no correlations between the random coefficients, meaning that if one person prefers 8 seats over 6, we would not expect that they also prefer 7 seats over 6. 
# Including correlations in the random coefficients allows us to determine, based on the data, whether people who like one attribute also tend to like another attribute.

cov2cor(cov.mlogit(m2.hier))


# Share Prediction for Heterogeneous Choice Models
predict.hier.mnl <- function(model, data, nresp=1000) {
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma <- cov.mlogit(model)
  coef.mu <- m2.hier$coef[1 : dim(coef.Sigma)[1]]
  draws <- mvrnorm(n = nesp, coef.mu, coef.Sigma)
  share <- matrix(NA, nrow = nresp, ncol = nrow(data))
  for (i in i:nresp) {
    utility <- data.model %*% draws[i,]
    share = exp(utility) / sum(exp(utility))
    shares[i, ] <- share
  }
  cbind(colMeans(shares), data)
}
predict.hier.mnl(m2.hier, data=new.data)
