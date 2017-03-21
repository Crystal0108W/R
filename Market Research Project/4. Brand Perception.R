# Reduce data complexity by discovering the underlying dimensions

# 1. Prepare Datasets: On a scale from 1 to 10—where 1 is least and 10 is most—how [ADJECTIVE] is [BRAND A]?
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
summary(brand.ratings)
str(brand.ratings)

# 1-1. Rescaling the data
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[,1:9])
summary(brand.sc)

# 1-2. Inspect bivariate relationship:
install.packages("corrplot")
library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order = "hclust") #the argument order="hclust" reorders the rows and columns according to variables’ similarity in a hierarchical cluster solution
#This visualization of the basic data appears to show three general clusters that comprise fun/latest/trendy,rebuy/bargain/value, and perform/leader/serious, respectively.

# 2. Aggregate Mean Ratings by Brand
brand.mean <- aggregate(. ~ brand, data = brand.sc, mean)
rownames(brand.mean) <- brand.mean[, 1] #use brand for the row name
brand.mean <- brand.mean[, -1]  #remove brand name column
brand.mean

# 2-1. Visualize brand mean
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("gplots")
library(gplots)
heatmap.2(as.matrix(brand.mean),
          col = brewer.pal(9, "GnBu"), trace = "none", key = FALSE, dend = "none",
          main = "\n\n\n\n\nBrand attributes")
# Brands f and g are similar with high ratings for rebuy and value but low ratings for latest and fun. Other groups of similar brands are b/c, i/h/d, and a/j.

# Principal component analysis (PCA): attempts to find uncorrelated linear dimensions that capture maximal variance in the data. + Perceptual Map
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)  
plot(brand.pc, type = "lines") # look for elbows
# In this visual, the elbow occurs at 3, so the first 3 components explain most of the variation. 
biplot(brand.pc)
# biplot() of the first two principal components by default

# But the plot of individual respondents' ratings is too dense, we can use aggregare ratings instead
brand.mu.pc <- prcomp(brand.mean, scale. = TRUE)
summary(brand.mu.pc)

# Exploratory factor analysis (EFA) also attempts to capture variance with a small number of dimensions while seeking to make the dimensions interpretable in terms of the original variables.
# Multidimensional scaling (MDS) maps similarities among observations in terms of a low-dimension space such as a two-dimensional plot. MDS can work with metric data and with non-metric data such as categorical or ordinal data.



