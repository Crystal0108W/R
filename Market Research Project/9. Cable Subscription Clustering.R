# Clustering - Unsupervised
seg.raw <- read.csv("http://goo.gl/qw303p")
seg.df <- seg.raw[ , -7]
colnames(seg.raw)[colnames(seg.raw) == "Segment"] <-c("ValidateSegment")

# Step1 : Transform the data if needed for a particular clustering method; for instance, some methods require all numeric data (e.g., kmeans(), mclust()) or all categorical data
# Step2 : Compute a distance matrix if needed; some methods require a precomputed matrix of similarity in order to group observations 
# Step3 : Apply the clustering method and save its result to an object. For some methods this requires specifying the number (K) of groups desired (e.g., kmeans(), poLCA())
# Step4 : For some methods, further parse the object to obtain a solution with K groups (e.g., hclust()).
# Step5 : Examine the solution in the model object with regard to the underlying data, and consider whether it answers a business question.


# Classification - Supervised
seg.sum <- function(data,groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg.sum(seg.df, seg.raw$ValidateSegment) # Rough look at the mean of data

# Questions to think about: 
# Are there obvious differences in group means?
# Does the differentiation point to some underlying story to tell?
# Do we see immediately odd results such as a mean equal to the value of one data level?


# Hierarchical Clustering

# The hierarchical clustering method begins with each observation in its own cluster. 
# It then successively joins neighboring observations or clusters one at a time according to their distances from one another, and continues this until all observations are linked. 
# This process of repeatedly joining observations and groups is known as an agglomerative method.

d <- dist(seg.df[,c("age", "income", "kids")]) # calculating Eucleadian distance, but it can only calculate the distance between numeric variables
as.matrix(d)[1:5, 1:5]

# Since we have factor variables in our dataset, we will instead use daisy() function to work with mixed data types
install.packages("cluster")
library(cluster)

seg.dist <- daisy(seg.df)
as.matrix(seg.dist)

# Ivoke hclust() on the dissimilarity matrix: 
seg.hc <- hclust(seg.dist, method = "complete") # use the complete linkage method which evaluates the distance between every member

# Visualize using plot()
plot(seg.hc)
plot(cut(as.dendrogram(seg.hc), h = 0.5)$lower[[1]]) # coerce it to a dendrogram object (as.dendrogram(. . . )), cut it at a certain height (h=. . . ), and select the resulting branch that we want (. . . $lower[[1]]).

# Check Goodness-of-fit metrics: CPCC(cophenetic correlation coefficient)
cor(cophenetic(seg.hc),seg.dist)
# CPCC is interpreted similarly to Pearson’s r. In this case, CPCC > 0.7 indicates a relatively strong fit, meaning that the hierarchical tree represents the distances between customers well.


# Selecting the number of Clustering
install.packages("factoextra")
install.packages("ggplot2")
library(ggplot2)
library(factoextra)
fviz_nbclust(seg.df, hcut, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Groups from hclust()
plot(seg.hc)
rect.hclust(seg.hc, k=4, border = "pink")
seg.hc.segment <- cutree(seg.hc, k=4) # obtain the assignment vector for observations using cutree()
table(seg.hc.segment) # groups 1 and 2 dominate the assignment
seg.sum(seg.df, seg.hc.segment)
# INSIGHTS: 
# groups 1 and 2 are distinct from 3 and 4 due to subscription status
# group 1 is all male (gender=2 as in levels(seg.df$gender)) while group 1 is all female.
# Subscribers are differentiated into those who own a home (group 3) or not (group 4).

# Not helpful from business point of view because it's too broad, NOT Interesting!
plot(jitter(as.numeric(seg.df$gender)) ~ jitter(as.numeric(seg.df$subscribe)),  
     col = seg.hc.segment, yaxt = "n", xaxt = "n", ylab = "", xlab = "")
axis(1, at = c(1,2), labels = c("Subscribe: No", "Subscribe: Yes"))
axis(2, at = c(1,2), labels = levels(seg.df$gender))


# K-means Clustering
# Because it explicitly computes a mean deviation, k-means clustering relies on Eu- clidean distance. 
# Thus it is only appropriate for numeric data or data that can be reasonably coerced to numeric.

# Create a variant of seg.df
seg.df.num <- seg.df
seg.df.num$gender <- ifelse(seg.df.num$gender == "Male", 0, 1)
seg.df.num$ownHome <- ifelse(seg.df.num$ownHome == "ownNo", 0, 1)
seg.df.num$subscribe <- ifelse(seg.df$subscribe == "subNo", 0, 1)

# Run kmeans() algorithm
set.seed(54321)
seg.k <- kmeans(seg.df.num, centers = 4)
seg.sum(seg.df, seg.k$cluster) # the groups seem to vary by age, gender, kids, income, and home ownership

# Visualize the clusters
library(cluster)
clusplot(seg.df, seg.k$cluster, color = TRUE, shade = TRUE, labels = 4, lines = 0, main = "K-means cluster plot")
# clusplot() will perform dimensional reduction with principal components or multidimensional scaling as the data warrant, and then plot the observations with cluster membership identified 
fviz_cluster(seg.k, data = seg.df.num, geom = "point",
             stand = FALSE, ellipse = TRUE, ellipse.type = "norm", palette = "Set2", ggtheme = theme_minimal())
# A limitation of k-means analysis is that it requires specifying the number of clusters, and it can be difficult to determine whether one solution is better than another.


# Model-Based Clustering: Mclust()
#The key idea for model-based clustering is that observations come from groups with different statistical distributions (such as different means and variances). 
# The algorithms try to find the best set of such underlying distributions to explain the observed data.
# Such models are also known as “mixture models” because it is assumed that the data reflect a mixture of observations drawn from different populations, although we don’t know which population each observation was drawn from. We are trying to estimate the underlying population parameters and the mixture proportion. mclust models such clusters as being drawn from a mixture of normal (also known as Gaussian) distributions.
# because mclust models data with normal distributions, it uses only numeric data.
install.packages("mclust")
library(mclust)
seg.mc <- Mclust(seg.df.num)
summary(seg.mc) # the data are estimated to have 2 clusters
# We also see log-likelihood information, which we can use to compare models
# Force model try a 4-cluster solution
seg.mc4 <- Mclust(seg.df.num, G = 4)
summary(seg.mc4) # has a lower log likelihood


# Comparing Models with BIC()
BIC(seg.mc, seg.mc4) # the lower the value of BIC, on an infinite number line, the better; higher log-likelihood values are better 
seg.sum(seg.df, seg.mc$class)
library(cluster)
clusplot(seg.df, seg.mc$class, color = TRUE, labels = 4, main = "Model-Based Cluster Plot")



# Latent Class Analysis: poLCA()
# Latent class analysis (LCA) is similar to mixture modeling in the assumption that differences are attributable to unobserved groups that one wishes to uncover
# Whereas mclust and kmeans() work with numeric data, and hclust() de- pends on the distance measure, poLCA uses only categorical variables.

seg.df.cut <- seg.df
seg.df.cut$age <- factor(ifelse(seg.df$age < median(seg.df$age), 1, 2))
seg.df.cut$income <- factor(ifelse(seg.df$income < median(seg.df$income), 1, 2))
seg.df.cut$kids <- factor(ifelse(seg.df$kids < median(seg.df$kids),1,2))
summary(seg.df.cut)
seg.f <- with(seg.df.cut, cbind(age, gender, income, kids, ownHome, subscribe)~1)
install.packages("poLCA")
library(scatterplot3d)
library(MASS)
library(poLCA)
seg.LCA3 <- poLCA(seg.f, data = seg.df.cut, nclass = 3)
seg.LCA4 <- poLCA(seg.f, data = seg.df.cut, nclass = 4)

# compare which model is better: 
seg.LCA4$bic
seg.LCA3$bic 

seg.sum(seg.df, seg.LCA3$predclass)
seg.sum(seg.df, seg.LCA4$predclass)

table(seg.LCA3$predclass)
table(seg.LCA4$predclass)

# Visualize 
clusplot(seg.df, seg.LCA3$predclass, color = TRUE, labels = 4, main = "LCA plot (K=3)")
clusplot(seg.df, seg.LCA4$predclass, color = TRUE, labels = 4, main = "LCA plot (K=4)")


# Comparing cluster solutions
table(seg.LCA3$predclass,seg.LCA4$predclas)
# we use mapClass (a, b) and adjustedRandIndex(a, b) to compare agreement between the two solutions:
mapClass(seg.LCA3$predclass, seg.LCA4$predclass)$aTOb
adjustedRandIndex(seg.LCA3$predclass, seg.LCA4$predclass)
# The adjusted Rand index of 0.5117619 indicates that the match between the two assignment lists is better than chance. 
# From a business perspective, it also tells us that the 3-cluster and 4-cluster differ modestly from one another, which provides another perspective on choosing between them.


# compare the LCA 4-cluster solution to the true segments
table(seg.raw$ValidateSegment, seg.LCA4$predclass)
adjustedRandIndex(seg.raw$ValidateSegment, seg.LCA4$predclass)
# With a Rand index of 0.26, the LCA solution matches the true segment assignments moderately better than chance alone. 
