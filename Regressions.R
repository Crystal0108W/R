#################Simple Regression#############

fit <- lm(weight ~ height, data = women)
summary(fit)

women$weight
fitted(fit)
residuals(fit)
coefficients(fit)
confint(fit)
anova(fit)
vcov(fit)
predict(fit)
plot(women$height, women$weight, 
     xlab = "Height (in inches)",
     ylab = "Weight (in pounds)")
abline(fit) #abline adds straight line to the plot

#################Polynominal Regression#############
fit2<- lm(weight ~ height + I(height^2), data = women)
summary(fit2)

plot(women$height, women$weight,
     xlab = "Height (in inches)",
     ylab = "Weight (in pounds)")
lines(women$height, fitted(fit2)) # lines is a generic function taking coordinates given in various ways and joiningthe corresponding points with line segments

fit3<- lm(weight ~ height + I(height^2) + I(height^3), data = women) 
plot(women$height, women$weight,
     xlab = "Height (in inches)",
     ylab = "Weight (in pounds)")
lines(women$height, fitted(fit3)) 

install.packages("car")
library(car)

scatterplot(weight ~ height, data = women,
            spread = FALSE, lty = 2 , pch = 18,
            col= palette()[1:3],
            main = "Women Age 30-39",
            xlab = "Height (inches)",
            ylab = "Weight (lbs.)")

#################Multiple Linear Regression#############
states<- as.data.frame(state.x77[, c("Murder","Population","Illiteracy",
                                     "Income", "Frost")])

cor(states)
scatterplot.matrix(states, spread = FALSE, lty = 2, main = "Scatter Plot Matrix")

fit4<- lm(Murder ~ Population + Illiteracy + Income + Frost, 
          data = states)
summary(fit4)


#################Multiple Linear Regression with Interaction#############
fit5<- lm(mpg ~ hp+wt+hp:wt, data = mtcars)
summary(fit5)

install.packages("effects")
library(effects)
plot(effect("hp:wt",fit5,list(wt=c(2.2,3.2,4.2))), multiline = TRUE)
plot(effect("hp:wt",fit5), multiline = TRUE)
fit6<- lm(mpg ~ hp+wt, data = mtcars)


#################Regression Diagnostics_Plot#############
par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(2,2))
plot(fit2)

par(mfrow=c(2,2))
plot(fit4)



#################Regression Diagnostics_Non-Plot#############
install.packages("gvlma")
library(gvlma)
library(car)
par(mfrow=c(1,1))
qqplot(fit4, labels = row.names(states), id.method = "identify", 
       simulate = TRUE, main = "Q-Q Plot")


qqPlot(fit4, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

residplot<- function(fit4, nbreaks = 10){
              z <- rstudent(fit4)
              hist(z, breaks = nbreaks, freq = FALSE, 
                   xlab = "Studentized Residual",
                   main = "Distribution of Errors")
              rug(jitter(z), col = "grey")
              curve(dnorm(x, mean = mean(z), sd=sd(z)),
                    add = TRUE, col = "pink", lwd = 1)
              lines(density(z)$x, density(z)$y,
                    col = "cyan", lwd = 2, lty = 2)
              legend("topright",
                     legend = c("Normal Curve", "Kernal Density Curve"),
                     lty = 1:2, col = c("pink","cyan"), cex = .7)
}

residplot(fit4)

z <- rstudent(fit4)
density(z)

