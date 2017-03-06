###########################CLUSTERING################################
# Step 1: Choose appropriate attributes
# Step 2: Scale the data
# Step 3: Screen for outliers
# Step 4: Calculate distances
# Step 5: Select a clustering algorithm
# Step 6: Obtain one or more cluster solutions
# Step 7: Determine the number of cluster present
# Step 8: Obtain a final clustering solution
# Step 9: Visualize the clusters
# Step 10: Validate the results

install.packages("cluster")
install.packages("NbClust")
install.packages("flexclust")
install.packages("fMultivar")
install.packages("ggplot2")
install.packages("RGtk2")
install.packages("rattle")

data(nutrient, package = "flexclust")  # Calculating distance
head(nutrient, 4) 
d <- dist(nutrient) # default of dist() is Euclidean
as.matrix(d)[1:4, 1:4]



######## Hierarchical Clustering ##########
data(nutrient, package = "flexclust")
row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient)
d <- dist(nutrient.scaled)

fit.average <- hclust(d, method = "average")
plot(fit.average, hang = -1, cex = 0.8, main = "Average Linkage Clustering")

#Selecting the number of clusters
library(NbClust)
nc <- NbClust(nutrient.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "average")
table(nc$Best.n[1,])

par(mfrow = c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 26 Criteria")
# pick one with the highest "vote"

#Obtaining the final cluster solution with 5 clusters
clusters <- cutree(fit.average, k=5) # Assign cases
table(cluster)

aggregate(nutrient, by = list(clusters), median) # Describe Clusters

aggregate(as.data.frame(nutrient.scaled), by=list(cluster=clusters), median)

plot(fit.average, hang = -1, cex = 0.8, 
     main = "Average Linkage Clustering\n5 Cluster Solution")
rect.hclust(fit.average, k = 5)



######## Partitioning Clustering ##########

###### K-Means Clustering ########
# Step 1: Select K centroids
# Step 2: Assign each data point to its closest centroid
# Step 3: Recalculate the centroids as the average of all data points in a cluster
# Step 4: Assign data points to their cloest centroids
# Step 5: Continue steps 3 and 4 until the observations aren't reassigned or the maximum number of iterations (10 as defalut in R)


wssplot <- function(data, nc = 15, seed = 1234){
                      wss <- (nrow(data)-1)*sum(apply(data,2,var))
                      for (i in 2:nc){
                        set.seed(seed)
                        wss[i] <- sum(kmeans(data, centers = i)$withiness)}
                      plot(i:nc, wss, type = "b", xlab = "Number of Clusters",
                           ylab = "Within groups sum of squares")
}

library(rattle)
data(wine, package = "rattle")
head(wine)
df <- scale(wine[-1]) 
wssplot(df)
library(NbClust)
set.seed(1234)
devAskNewPage(ask = TRUE)
nc <- NbClust(df,min.nc = 2, max.nc = 15, method = "kmeans")
table(nc$Best.n[1,])

par(mfrow = c(1,1))
devAskNewPage(ask = FALSE)
barplot(table(nc$Best.n[1,]),
        xlab = "Number of Clusters", ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(df,3,nstart = 25)
fit.km$size
fit.km$centers
ct.km<- table(wine$Type, fit.km$cluster)
ct.km


###### Partioning arount medoids ########
#Because it’s based on means, the k-means clustering approach can be sensitive to out- liers. 
#A more robust solution is provided by partitioning around medoids (PAM). 
#Rather than representing each cluster using a centroid (a vector of variable means), each cluster is identified by its most representative observation (called a medoid). 
#Whereas k-means uses Euclidean distances, PAM can be based on any distance mea- sure. 
#It can therefore accommodate mixed data types and isn’t limited to continuous variables.
# Step 1: Randomly select K observations (call each a medoid).
# Step 2: Calculate the distance/dissimilarity of every observation to each medoid.
# Step 3: Assign each observation to its closest medoid.
# Step 4: Calculate the sum of the distances of each observation from its medoid (total cost).
# Step 5: Select a point that isn’t a medoid, and swap it with its medoid.
# Step 6: Reassign every point to its closest medoid.
# Step 7: Calculate the total cost.
# Step 8: If this total cost is smaller, keep the new point as a medoid.
# Step 9: Repeat steps 5–8 until the medoids don’t change.

library(cluster)
set.seed(1234)
fit.pam <- pam(wine[-1], k=3,stand = TRUE)
fit.pam$medoids
clusplot(fit.pam, main = "Bivariate Cluster Plot")
ct.pam <- table(wine$Type, fit.pam$clustering) 






