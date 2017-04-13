# Detect Fraudulent Transactions

# (1) outlier or anomaly detection, 
# (2) clustering, and also 
# (3) semi-supervised prediction models.

install.packages("ggplot2")
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

propP <- 100 * table(nas$Prod) / totP # There are several products that would have more than 20% of their transactions removed
propP[order(propP, decreasing = T)[1:10]]
# if we look at the similarity between the unit price distribution of the products, we will observe that these products are, in effect, rather similar to other products.
# In summary, the option of removing all transactions with unknown values on both the quantity and the value is the best option we have
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]

# analyze the remaining reports with unknown values in either the quantity or the value of the transaction
nnasQp <- tapply(sales$Quant,list(sales$Prod),function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]

# "p2442", "p2443" have all transaction missing, so remove them
sales <- sales[!sales$Prod %in% c("p2442", "p2443"), ]

# Are there salespeople with all transactions with unknown quantity?
nnasQs <- tapply(sales$Quant, list(sales$ID), function(x) sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs, decreasing = T)[1:10]]
# In effect, as long as we have other transactions of the same products reported by other salespeople, we can try to use this information to fill in these unknowns using the assumption that the unit price should be similar.


# Carry out the same analysis for transactions with unknown value
nnasVp <- tapply(sales$Val,list(sales$Prod),function(x) sum(is.na(x)))
propNAsVp <- nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing=T)[1:10]] # numbers are reasonable

# With respect to salespeople
nnasVs <- tapply(sales$Val, list(sales$ID), function(x) sum(is.na(x)))
propNAsVs <- nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs, decreasing = T)[1:10]]

# Fill-in the unknown values for the remaining dataset since they are reasonable

tPrice <- tapply(sales[sales$Insp != "fraud", "Price"],list(sales[sales$Insp != "fraud", "Prod"]), median, na.rm = T) # typical price of transactions that are not fraud

noQuant <- which(is.na(sales$Quant))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] / tPrice[sales[noQuant, "Prod"]]) # use the typical price as the price for that product and calculate the quantity

noVal <- which(is.na(sales$Val))
sales[noVal,'Val'] <- sales[noVal,'Quant'] * tPrice[sales[noVal,'Prod']]# use the typical price as the price for that product and calculate the quantity

sales$Price <- sales$Val/sales$Quant # Now the dataset is free of unknown values
sales <- as.data.frame(sales)
save(sales, file = "/Users/yw7986/Desktop/salesClean.Rdata")


# Detect Products with very few transactions
attach(sales)
notF <- which(Insp != "Fraud")

# Check for central tendency and spread; use the median as the statistics of centrality and the inter-quartile range(IQR) as the statistics of spread
ms <- tapply(Price[notF], list(Prod = Prod[notF]), function(x) {
  bp <- boxplot.stats(x)$stats
  c(median = bp[3], ipr = bp[4]-bp[2])
})
ms <- matrix(unlist(ms), length(ms), 2,
             byrow = T, dimnames = list(names(ms), c("median", "iqr")))

par(mfrow = c(1,2))
plot(ms[, 1], ms[, 2], xlab = "Median", ylab = "IQR", main = "with Linear Scale")
plot(ms[, 1], ms[, 2], xlab = "Median", ylab = "IQR", main = "with Log Scale", col = "grey", log = "xy")
smalls <- which(table(Prod) < 20)
points(log(ms[smalls, 1]), log(ms[smalls, 2]), pch = "+")
# This provides good indications of the similarity of their distributions of unit price.
dms <- scale(ms)
smalls <- which(table(Prod) < 20)
prods <- tapply(sales$Price, sales$Prod, list)
similar <- matrix(NA, length(smalls), 7, dimnames = list(names(smalls), c("Simil", "ks.stat", "ks.p", "medP", "iqrP", "medS", "iqrS")))
for (i in seq(along = smalls)) {
  d <- scale(dms, dms[smalls[i], ], FALSE)
  d <- sqrt(drop(d^2 %*% rep(1, ncol(d))))
  stat <- ks.test(prods[[smalls[i]]], prods[[order(d)[2]]])
  similar[i, ] <- c(order(d)[2], stat$statistic, stat$p.value, ms[smalls[i], ], ms[order(d)[2], ])
}




#################################################################################################################################################################
avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats))
    stop("Provide either the training data or the product stats")
  if (missing(stats)) {
    notF <- which(train$Insp != "fraud")
    stats <- tapply(train$Price[notF],
                    list(Prod = train$Prod[notF]),
                    function(x) {
                      bp <- boxplot.stats(x)$stats
                      c(median=bp[3], iqr = bp[4]-bp[2])
                    })
    stats <- matrix(unlist(stats),
                    length(stats), 2, byrow = T, 
                    dimnames = list(names(stats), c('median', 'iqr')))
    stats[which(stats[, 'iqr'] == 0), 'iqr'] <- stats[which(stats[, 'iqr'] == 0), 'median']
  }
  
  mdtp <- mean(abs(toInsp$Price - stats[toInsp$Prod, 'median']) / stats[toInsp$Prod, 'iqr'])
  return(mdtp)
}

evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds) {
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}

#################################################################################################################################################################
# Unsupervised Approaches
BPrule <- function(train,test) {
  notF <- which(train$Insp != 'Fraud')
  ms <- tapply(train$Price[notF], list(Prod = train$Prod[notF]),
               function(x) {
                 bp <- boxplot.stats(x)$stats
                 c(median = bp[3], iqr = bp[4]-bp[2])
               })
  ms <- matrix(unlist(ms),length(ms),2,byrow=T, dimnames=list(names(ms),c('median','iqr')))
  ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
  ORscore <- abs(test$Price-ms[test$Prod,'median']) /ms[test$Prod,'iqr']
  return(list(rankOrder=order(ORscore,decreasing=T),rankScore=ORscore))
}

notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Price[notF], list(Prod = sales$Prod[notF]),
                      function(x) {
                        bp <- boxplot.stats(x)$stats
                        c(median = bp[3], iqr = bp[4]-bp[2])
                      })
globalStats <- matrix(unlist(globalStats),length(globalStats),2,byrow=T,dimnames=list(names(globalStats),c('median','iqr')))
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- globalStats[which(globalStats[,'iqr']==0),'median']
ho.BPrule <- function(form, train, test, ...) {
  res <- BPrule(train, test)
  structure(evalOutlierRanking(test, res$rankOrder, ...),
            itInfo = list(preds = res$rankScore,
                          trues = ifelse(test$Insp == 'fraud', 1, 0)))
}


bp.res <- holdOut(learner('ho.BPrule',
                          pars = list(Threshold = 0.1, statsProds = globalStats)),
                  dataset(Insp~., sales),
                  hldSettings(3, 0.3, 1234, T),
                  itsInfo = TRUE)

summary(bp.res)

par(mfrow=c(1,2))
info <- attr(bp.res,'itsInfo')
PTs.bp <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)), c(1, 3, 2))
PRcurve(PTs.bp[,,1],PTs.bp[,,2],main='PR curve',avg='vertical')
CRchart(PTs.bp[,,1],PTs.bp[,,2],main='Cumulative Recall curve',avg='vertical')
