########ANOVA###############
install.packages("car")
install.packages("gplots")
install.packages("HH")
install.packages("rrcov")
install.packages("multcomp")
install.packages("effects")
install.packages("MASS")
install.packages("mvoutlier")

library(multcomp)
cholesterol
attach(cholesterol)
table(trt)

aggregate(response, by=list(trt), FUN = mean)
aggregate(response, by=list(trt), FUN = sd)
fit <- aov(response ~ trt)
summary(fit)

library(gplots)
plotmeans(response ~ trt, xlab = "Treatment", ylab = "Response",
          main = "Mean PLot\nwith 95% CI")
detach(cholesterol)

#####POST TEST 1#########
TukeyHSD(fit)
par(las = 1)
par(mar = c(5,8,4,2))
plot(TukeyHSD(fit))

#####POST TEST 2#########
par(mar = c(5,4,6,2))
tuk <- glht(fit, linfct = mcp(trt="Tukey"))
plot(cld(tuk, level = .05), col = "pink")
#here groups with the same letter don't have significantly different means


######ASSESSING TEST ASSUMPTION#######
#####Normality Test######
library(car)
qqPlot(lm(response ~ trt, data=cholesterol),
       simulate=TRUE, main="QQ Plot", labels=FALSE)
# The data falls within the 85% confidence envelope, suggesting that the normality assumption has been met fairly well.

#####Homogeneity Test#######
bartlett.test(response ~ trt, data = cholesterol)
#####Outlier test######
outlierTest(fit)

####################ANCOVA#######################
data(litter, package = "multcomp")
attach(litter)
table(dose)
aggregate(weight, by = list(dose), FUN = mean)
fit <- aov(weight~gesttime + dose)
summary(fit)
detach(litter)

# Since we are using a covariate, I want to obtain adjusted group means, that is the group means obtained after partically out the effects of the covariate.
library(effects)
effect("dose", fit)

#POST TEST
library(multcomp)
contrast <- rbind("no drug vs. drug"=c(3,-1,-1,-1))
summary(glht(fit, linfct = mcp(dose = contrast)))
contrast
help("mcp")

#Assumption Test(same as ANOVA)
fit2 <- aov(weight ~ gesttime*dose, data = litter)
summary(fit2)

####VISUALIZING THE RESULTS####
library(HH)
par(mfrow = c(1,2))
ancova(weight ~ gesttime + dose, data = litter)
ancova(weight ~ gesttime*dose, data = litter)

####Two-Way Factorial ANOVA#####
attach(ToothGrowth)
table(supp, dose)
aggregate(len, by = list(supp,dose), FUN = mean)
aggregate(len, by = list(supp,dose), FUN = sd)
dose <- factor(dose)
fit <- aov(len ~ supp*dose)
summary(fit)

par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 2))
interaction.plot(dose, supp, len, type = "b",
                 col = c("pink", "grey"), pch =c(16, 18),
                 main = "Interaction between Dose nd Supplement Type")

library(gplots)
plotmeans(len ~ interaction(supp, dose, sep = " "),
          connect = list(c(1,3,5), c(2,4,6)),
          col = c("salmon", "darkgreen"),
          main = "INteraction Plot with 95% CIs",
          xlab = "Treatment and Dose Combination")

library(HH)
interaction2wt(len~supp*dose)

##########REPEATED MEASURE OF ANOVA#######
CO2$conc <- factor(CO2$conc)
wlb1<- subset(CO2, Treatment == "chilled")
fit <- aov(uptake ~ conc*Type + Error(Plant/(conc)), wlb1)
summary(fit)

par(las = 2)
par(mar=c(10,4,4,2))
with(wlb1, interaction.plot(conc,Type,uptake,
                            type = "b", col = c("red", "blue"), pch = c(16, 18),
                            main = "Interaction Plot for Plant Type and Concentration"))
boxplot(uptake ~ Type*conc, data = wlb1, col = (c("gold", "green")),
        main = "Chilled Quebec and Mississippi Plants",
        ylab = "Carbon dioxide uptake rate (umo1/m^2 sec)")


##########MULTIVARIARE ANALYSIS OF VARIANCE#######
library(MASS)
attach(UScereal)
shelf <- factor(shelf)
y <- cbind(calories, fat, sugars)
aggregate(y, by=list(shelf), FUN = mean)
cov(y)
fit <- manova(y ~ shelf)
summary(fit)
summary.aov(fit)

##########ASUMPTION TEST#######
center <- colMeans(y)
n <- nrow(y)
p <- ncol(y)
cov <- cov(y)
d <- mahalanobis(y, center, cov)
coord <- qqplot(qchisq(ppoints(n), df = p),d)
abline(a=0, b=1)
identify(coord$x, coord$y, labels = row.names(UScereal))

library(mvoutlier)
outlier <- aq.plot(y)
outlier

##############ANOVA as REGRESSION##########
library(multcomp)
levels(cholesterol$trt)
fit.aov <- aov(response ~ trt, data = cholesterol)
summary(fit.aov)
fit.lm <- lm(response ~ trt, data = cholesterol)
summary(fit.lm)


