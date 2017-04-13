# Detect Fraudulent Transactions

# (1) outlier or anomaly detection, 
# (2) clustering, and also 
# (3) semi-supervised prediction models.

library(ggplot2)

load("/Users/yw7986/Desktop/sales.Rdata")

# First, take a peak at the data
summary(sales) 
nlevels(sales$ID) # 6016 levels of salespeople ID
nlevels(sales$Prod) # 4548 levels of Prod ID
sales$Price <- sales$Val/sales$Quant


# Check the proportion of fraud of fraud reports
prop.table(table(sales$Insp))

totS <- table(sales$ID)
totP <- table(sales$Prod)

# Transaction per Salesperson Plot
barplot(totS, main = "Transaction per Salesperson", xlab = "Salespeople", ylab = "Amount", names.arg = "")
barplot(totP, main = "Transaction per Product", xlab = "Products", ylab = "Amount", names.arg = "")

# Check the Price sold for products
summary(sales$Price)
attach(sales)
upp <- aggregate(Price, list(Prod), median, na.rm = T)
topP <- sapply(c(T,F), function(o) upp[order(upp[,2], decreasing = o)[1:5],1])
colnames(topP) <- c("Expensive", "Cheap") # Get the 5 most expensive and 5 cheapeast product

tops<- sales[Prod %in% topP[1,], c("Prod", "Price")]
tops$Prod <- factor(tops$Prod)

# boxplot for the cheapest and the most exensive product 
boxplot(Price ~ Prod, data = tops, ylab = "Price", log = "y",
        names = c("p560-Cheapest", "p3689-Most Expensive"),
        main = "Boxplot for the cheapest and the most exensive product\n") #The scales of the prices of the most expensive and cheapest products are rather different. Because of this, we have used a log scale in the graph to avoid the values of the cheapest product becoming indistinguishable.

# Check the performance for each salesperson
vs <- aggregate(Val, list(ID), sum, na.rm = T)
scoreSs <- sapply(c(T,F), function(o) vs[order(vs[,2], decreasing = o)[1:5],1])
sum(vs[order(vs$x, decreasing = T)[1:100], 2]) / sum(Val, na.rm = T) # the top 100 salespeople accounted for 38% of totle income. 
sum(vs[order(vs$x, decreasing = F)[1:2000], 2]) / sum(Val, na.rm = T) # the botom 2000 salespeople accounted for 2% of total income.

# Check the pquantity sold for products
qs <- aggregate(Quant,list(Prod),sum,na.rm=T)
scorePs <- sapply(c(T,F), name <- function(o) qs[order(qs$x, decreasing = o)[1:5],1])
colnames(scorePs) <- c("Sold Most", "Sold Least")

sum(as.double(qs[order(qs$x, decreasing = T)[1:100], 2])) / sum(as.double(Quant), na.rm = T) # the top 100 products accounted for 74.6% of total sales volume
sum(as.double(qs[order(qs$x, decreasing = F)[1:4000], 2])) / sum(as.double(Quant), na.rm = T) # the botom 4000 products accounted for 8.9% of total sales volume


# Assumption: the unit price of any product should follow a near-normal distribution. 
# Idetify Outlier: The rule is that an observation should be tagged as an anomaly high (low) value if it is above (below) the high (low) whisker, defined as Q3 + 1.5 × IQR (Q1 − 1.5 × IQR)
out <- tapply(Price, list(Prod = Prod),
              function(x) length(boxplot.stats(x)$out)) # For each product, how many transaction(price point) are identified as outlier?
sum(out) # In total, 29,446 transactions are considered outliers
sum(out)/nrow(sales) # about 7% of the total transaction


# Deal with Missing Values. 

# Check missing value in the dataset
length(which(is.na(sales$Quant) & is.na(sales$Val))) #888 cases have both the value of Quant and Val missing.
nrow(subset(sales, is.na(Quant) & is.na(Val))) #888 cases have both the value of Quant and Val missing.
sum(is.na(sales$Quant) & is.na(sales$Val)) #888 cases have both the value of Quant and Val missing.

nas <- sales[which(is.na(Quant) & is.na(Val)), c("ID","Prod")]

# Removing all 888 cases may be problematic if this leads to removing most transactions of some product or salesperson.
propS <- 100 * table(nas$ID)/totS
propS[order(propS, decreasing = T)[1:10]]
