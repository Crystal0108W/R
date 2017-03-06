install.packages("gmodels")
library(gmodels)
install.packages("vcd")
library(vcd)


CrossTable(Arthritis$Treatment, Arthritis$Improved)
mytable<- xtabs(~Treatment + Sex + Improved, data = Arthritis)
mytable

ftable(mytable)

margin.table(mytable, 3)
ftable(prop.table(mytable, c(1,3)))

ftable(addmargins(prop.table(mytable, c(1,2)), 3))

####################################################
mytable<- xtabs(~Treatment+Improved, data = Arthritis)
chisq.test(mytable)

mytable <- xtabs(~Improved+Sex, data=Arthritis)
chisq.test(mytable)
fisher.test(mytable)


mytable<- xtabs(~Treatment+Improved, data = Arthritis)
assocstats(mytable)

####################################################
install.packages("psych")
install.packages("ggm")
library(psych)
library(ggm)


Mydata<- state.x77
cov(Mydata)
cor(Mydata)
cor(Mydata, method = "spearman")

x<- Mydata[,c("Population","Income", "Illiteracy", "HS Grad")]
y<- Mydata[,c("Life Exp","Murder")]
cor(x,y)

colnames(Mydata)
pcor(c(1,5,2,3,6), cov(Mydata))

#####Test the significance of correlation########

cor.test(Mydata[,3], Mydata[,5])

corr.test(Mydata, use = "complete")


###################################################

install.packages("MASS")
library(MASS)

crime<- UScrime
t.test(Prob ~ So, data = crime)

sapply(crime[c("U1","U2")], function(x)(c(mean = mean(x), sd = sd(x))))
with(crime, t.test(U1,U2, paired = TRUE))
t.test(crime$U1, crime$U2, paired = TRUE)









