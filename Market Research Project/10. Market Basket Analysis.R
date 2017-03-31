# Market Basket Analysis - Association Rules Mining
# Association rule analysis attempts to find sets of informative patterns from large, sparse data sets.

# support: The support for a set of items is the proportion of all transactions that contain the set. Support is defined separately for every unique set of items.
# If {hot dogs, soda} appears in 10 out of 200 transactions, then support({hotdogs,soda}) = 0.05. - no direction

# Confidence: Confidence is the support for the co-occurrence of all items in a rule, conditional on the support for the left-hand set alone.
# confidence(X ⇒ Y ) = support(X∩Y)/support(X) - has direction

# Lift: the support of a set conditional on the joint support of each element,or 
# lift(X ⇒Y)=support(X∩Y)/(support(X)support(Y)). - no direction


install.packages("arules")
library(arules)
install.packages("arulesViz")
library(grid)
library(arulesViz)

data("Groceries")
summary(Groceries)

groc.rule <- apriori(Groceries, parameter = list(supp = 0.01, conf = 0.3, target = "rules"))
# First, check the number of items going into the rules, which is shown on the output line“sorting and recoding items ...”and in this case tells us that the rules found are using 88 of the total number of items.
# Next,check the number of rules found, as indicated on the “writing ...”line. In this case, the algorithm found 125 rules.
# if this number is too low, it suggests the need to lower the support or confidence levels; if it is too high (such as many more rules than items), you might increase the support or confidence levels.

inspect(subset(groc.rule, lift >3)) # combination is 3× more likely to occur together than one would expect from the individual rates of incidence alone.


# Supermarket Data
retail.raw <- readLines("http://goo.gl/FfjDAO")
head(retail.raw)
summary(retail.raw)

retail.list <- strsplit(retail.raw, " ")
names(retail.list) <- paste("Trans", 1:length(retail.list), sep = "")

str(retail.list)
install.packages("car")
library(car)
some(retail.list)

# convert from a list to transactions
retail.trans <- as(retail.list, "transactions")
summary(retail.trans)

# Finding and Visulizing Association Rules
retail.rules <- apriori(retail.trans, parameter = list(support = 0.001, conf = 0.4))
plot(retail.rules, interactive = TRUE)


# FInding and Plotting Subsets of Rules
retail.high <- head(sort(retail.rules, by = "lift"), 50) # 50 rules with highest lift
inspect(retail.high)

plot(retail.high, method = "graph", control = list(type = "items")) #The size (area) of the circle represents the rule’s support, and shade represents lift (darker indicates higher lift).



# Using Profit Margin Data with Transactions

# Simulate Margin and cost for each item
retail.itemnames <- sort(unique(unlist(as(retail.trans, "list"))))
set.seed(54321)
retail.margin <- data.frame(margin = rnorm(length(retail.itemnames), mean = 0.3, sd = 0.3))
rownames(retail.margin) <- retail.itemnames

retail.margsum <- function(items, itemMargins) {
if (class(items) == "rules") {
  tmp.items <- as(items(items), "list")
} else if (class(items) == "transactions") {
  tmp.items <- as(items, "list")
} else if (class(items) == "list") {
  tmp.items <- items
} else if (class(items) == "character") {
  tmp.items <- list(items)
} else {stop("Don’t know how to handle margin for class ", class(items))
}

good.items <- unlist(lapply(tmp.items, function (x) all(unlist(x) %in% rownames(itemMargins))))

if (!all(good.items)) {
  warning("Some items not found in rownames of itemMargins. ",
          "Lookup failed for element(s):\n",
          which(!good.items), "\nReturning only good values.")
  tmp.items <- tmp.items[good.items]
}
return(unlist(lapply(tmp.items, function(x) sum(itemMargins[x, ]))))
}



# Rules in Non-Transactional Data
seg.df <- read.csv("http://goo.gl/qw303p") 

# Association rules work with discrete data
seg.fac <- seg.df
seg.fac$age <- cut(seg.fac$age,
                   breaks=c(0,25,35,55,65,100),
                   labels=c("19-24", "25-34", "35-54", "55-64", "65+"),
                   right=FALSE, ordered_result=TRUE)

seg.fac$income <- cut(seg.fac$income,
                      breaks=c(-100000, 40000, 70000, 1000000),
                      labels=c("Low", "Medium", "High"),
                      right=FALSE, ordered_result=TRUE)

seg.fac$kids <- cut(seg.fac$kids,
                    breaks=c(0, 1, 2, 3, 100),
                    labels=c("No kids", "1 kid", "2 kids", "3+ kids"),
                    right=FALSE, ordered_result=TRUE)

seg.trans <- as(seg.fac, "transactions")
summary(seg.trans)

seg.rules <- apriori(seg.trans, parameter = list(support = 0.1, conf = 0.4, target = "rules"))
plot(seg.rules)

seg.hi <- head(sort(seg.rules, by = "lift"), 35)
inspect(seg.hi)
plot(seg.hi, method = "graph", control = list(type = "items"))
