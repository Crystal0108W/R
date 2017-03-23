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
          main = "Brand attributes")
# Brands f and g are similar with high ratings for rebuy and value but low ratings for latest and fun. Other groups of similar brands are b/c, i/h/d, and a/j.

# Principal component analysis (PCA): attempts to find uncorrelated linear dimensions that capture maximal variance in the data. + Perceptual Map
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)  
plot(brand.pc, type = "lines") # look for elbows
# In this visual, the elbow occurs at 3, so the first 3 components explain most of the variation. 
biplot(brand.pc)
# biplot() of the first two principal components by default

# But the plot of individual respondents' ratings is too dense, we can use aggregare ratings instead
brand.mu.pc <- prcomp(brand.mean, scale = TRUE) # scale = TRUE to rescale the data
summary(brand.mu.pc)


# Perceptual Map of the brand
par(mar=c(5,1,4,1))
biplot(brand.mu.pc, main = "Brand Positioning", cex = c(1.5, 1))
# If the strategic goal is to be a safe brand that appeals to many consumers, then a relatively undifferentiated position like e(sitting in the middle) could be desirable
# If the strategic goal is for the brand to have a strong, differentiated perception, this finding would be unwanted. 

# CAUTION: the relationships are strictly relative to the product category and the brands and adjectives that are tested.
# One way to test sensitvity here is to run PCA and biplot on a few different samples from our data, such as 80% of observations and perhaps droppong an adjective each time. If the maps are similar across samples, we can feel more confident in their stability.
# CAUTION: The strength of a brand on a single adjective cannot be read directly from the chart.
#          The position of the brand is a constructed composites of all dimension. 


# Exploratory factor analysis (EFA) also attempts to capture variance with a small number of dimensions while seeking to make the dimensions interpretable in terms of the original variables.
# Exploratory Factor Analysis 
# Factor as latent variables that cannot be observed directly, but are imperfectly assessed through their relationship to other variables
# In psychometrics, composite factors such as "intelligence", "knowledge" and "anxiety" are abstract concepts taht are not directly observable in themselves. Instead, they are observed empirically through multiple behaviors.(manifest variables)
# EFA attempts to find the degree to which latent, composite factors account for the observed variance of those manifest variables.
# In marketing, composite factors such as customer satisfaction, purchase intent, price sensitivity, category involvement.

# Step1: determine the number of factors to estimate
install.packages("nFactors")
library(nFactors)
nScree(brand.sc[, 1:9]) # 3 out of 4 methods recommend 3 factors
eigen(cor(brand.sc[, 1:9])) # The first three eignvalues are greater than 1.0, although barely so for the third value. This suggests that 3 or possibly 2 factors.

# Step2: Test both 2 and 3 factors to see which model is more useful
factanal(brand.sc[, 1:9], factors = 2) # EFA model is estimated with factanal()
# Factor 1 loads strongly on "bargain" and "value" and therefore might be interpreted as a "value" factor
# Factor 2 loads on "leader" and "serious" and thus might be regarded as "category leader" factor

factanal(brand.sc[, 1:9], factors = 3)
# The 3 factor solution retains the "value" and "leader" factors and adds a cleat "latest" factor that loads strongly on "latest" and "trendy". 

# Step3: Rotation
# A factor analysis solution can be rotated to have new loadings that account for the same proportion of variance. 
# CONSIDERATION: Do we allow the factors to be correlated with one another or not?
# Default factanal() has 0 correlation (using a varimax rotation)
# Alternatively, with factanal() in GPArotation package, we can have oblique rotation, the latent constructs are correlated with each other (such as price and lead, for leaders in industry, they are entitled to charge premium)
install.packages("GPArotation")
library(GPArotation)
(brand.fa.ob <- factanal(brand.sc[, 1:9], factors = 3, rotation = "oblimin"))
# The result includes a factor correlation matrix showing the relationships between the estimated latent factors.
# Factor 1 is negatively associated with factor 2 and essentially uncorrelated with Factor3

# Step4: Visualize the item-factor relationships - Heatmap
library(gplots)
library(RColorBrewer)
typeof(brand.fa.ob$loadings)
par(cex.main = 0.8)
heatmap.2(brand.fa.ob$loadings,
          col = brewer.pal(9, "Greens"), trace = "none", key = FALSE, dend = "none",
          Colv = FALSE, cexCol = 1.2,
          main = "Factor loadings for brand adjectives")


# Step4: Visualize the item-factor relationships - Path Diagram
install.packages("semPlot", type = "source")
library(semPlot)
semPaths(brand.fa.ob, what = "est", residuals = FALSE,
         cut = 0.3, posCol = c("grey", "cyan"), negCol = c("grey", "salmon"),
         edge.label.cex = 0.75, nCharNodes = 7)

# Step5-1: Using Factor Scores for Brands
brand.fa.ob <- factanal(brand.sc[,1:9], factors = 3, rotation = "oblimin", scores = "Bartlett")
brand.scores <- data.frame(brand.fa.ob$scores)
brand.scores$brand <- brand.sc$brand
head(brand.scores)
# The score of each factor for each brand to reduce data complexity

brand.fa.mean <- aggregate(.~brand, data = brand.scores, mean) #find the overall position for the brand by aggregating the individual scores
rownames(brand.fa.mean) <- brand.fa.mean[, 1]
brand.fa.mean <- brand.fa.mean[, -1]
names(brand.fa.mean) <- c("Leader", "Value", "Latest")
brand.fa.mean

# Step5-2: Visualize the positioning/make up of factor scores
heatmap.2(as.matrix(brand.fa.mean),
          col = brewer.pal(9,"GnBu"), trace = "none", key = FALSE, dend = "none", 
          cexCol = 1.2, main = "\n\n\n\n\n\n Mean Factor Score by Brand")




# Multidimensional scaling (MDS) maps similarities among observations in terms of a low-dimension space such as a two-dimensional plot. MDS can work with metric data and with non-metric data such as categorical or ordinal data.
# Instead of extracting underlying components or latent factors, MDS works instead with distances (also known as similarities). 
# MDS attempts to find a lower-dimensional map that best preserves all the observed similarities be- tween items.

# For metric data(numeric)
# 1. Calculate Similarity 
brand.dist <- dist(brand.mean) #dist() calculate Eucleadian distances
# 2. Find MDS solution for a distance matris from metric data
(brand.mds <- cmdscale(brand.dist))
# 3. Plot them
plot(brand.mds, type = "n")
text(brand.mds, rownames(brand.mds), cex = 2) #text(x, labels) to add labels to symbols in the plot

# For non-metric data(rankings or categorica variables)
brand.rank <- data.frame(lapply(brand.mean, function(x) ordered(rank(x))))
brand.rank1 <- data.frame(lapply(brand.mean, rank))
# 1. Calculate Similarity
install.packages("cluster")
library(cluster)
install.packages("MASS")
library(MASS)
brand.dist.r <- daisy(brand.rank, metric = "gower") # In daisy(), we compute distance with the gower metric, which handles mixed numeric, ordinal, and nominal data
# 2. Find MDS solution for a distance matris from non-metric data
brand.mds.r <- isoMDS(brand.dist.r)
# 3. Plot them
plot(brand.mds.r$points, type = "n")
text(brand.mds.r$points, levels(brand.sc$brand), cex = 2)
# Still, the nearest neighbors of brands are largely consistent with the exception of brands h and i, which are separated quite a bit more than in the metric solution. 
# This occurs because the rank-order procedure loses some of the information that is present in the original metric data solution, resulting in a slightly different map.
# MDS is a valuable alternative to PCA when working with non-metric data. 

# Key Points PCA: 

# 1. PCA finds linear functions that explain maximal variance in observed data. 
#    A key concept is that such components are orthogonal (uncorrelated). 
#    The basic R command is prcomp()
# 2. A common use for PCA is a biplot of aggregate scores for brands or people to visualize relationships. 
#    When this is done for attitudinal data such as brand ratings it is called a perceptual map. 
#    This is created by aggregating the statistic of interest by entity and charting with biplot()
# 3. Because PCA components often load on many variables, the results must be inspected cautiously and in terms of relative position. 


# Key Points EFA: 

# 1. EFA models latent variables (factors) that are not observed directly but appear indirectly as observed manifest variables.
#    A key procedure is factanal()
# 2. A fundamental decision in EFA is the number of factors to extract.
#    Common criteria involve inspection of a scree plot and extraction of factors such that all eigenvalues are greater than 1.0. 
#    There are useful tools to determine the number of factors in nFactors
# 3. EFA uses rotation to adjust an initial solution to one that is mathematically equivalent but more interpretable according to one’s aims. 
#    Another key decision in EFA is whether one believes the underlying latent variables should be uncorrelated (calling for an orthogonal rotation such as varimax) or correlated (calling for an oblique rotation such as oblimin) 
# 4. After performing EFA, you can extract factor scores that are the best estimates for each observation (respondent) on each factor. These are present as $scores in factanal() objects if you request them with the scores argument


# Key Points MDS: 
# MDS is similar to PCA but is able to work with both metric and non-metric data. 
# MDS requires a distance score obtained from dist() for metric data or a procedure such as daisy() for non-metric data. 
# MDS scaling is then performed by cmdscale() for metric data or isoMDS() (or other options) for non-metric data. 
