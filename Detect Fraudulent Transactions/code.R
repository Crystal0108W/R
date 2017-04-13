# (1) outlier or anomaly detection, 
# (2) clustering, and also 
# (3) semi-supervised prediction models.

library(ggplot2)

load("/Users/yw7986/Desktop/sales.Rdata")

# First, take a peak at the data
summary(sales) 
nlevels(sales$ID) # 6016 levels of salespeople ID
nlevels(sales$Prod) # 4548 levels of Prod ID

# Check missing value in the dataset
length(which(is.na(sales$Quant) & is.na(sales$Val))) #888
nrow(subset(sales, is.na(Quant) & is.na(Val))) #888
sum(is.na(sales$Quant) & is.na(sales$Val)) #888

# Check the proportion of fraud of fraud reports
prop.table(table(sales$Insp))

totS <- table(sales$ID)
totP <- table(sales$Prod)

# Transaction per Salesperson Plot
barplot(totS, main = "Transaction per Salesperson", xlab = "Salespeople", ylab = "Amount")
temp <- as.data.frame(totS)
ggplot(data = temp, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")

ggplot(data = temp, aes(x = Var1, y = Freq)) + geom_bar(stat = "identity")
ggplot(data = temp, aes(x = Var1)) + geom_bar()
