# Item to Item
packages <- read.csv("https://raw.githubusercontent.com/Crystal0108W/ML_for_Hackers/master/10-Recommendations/data/installations.csv")

install.packages("reshape")
library(reshape)

user.package.matrix <- cast(packages, User~Package, value = "Installed")
row.names(user.package.matrix) <- user.package.matrix[,1]
user.package.matrix <- user.package.matrix[, -1]

similarities <- cor(user.package.matrix) # similarity is not distance, KNN needs distance, so we transfer similarity to distance
distances <- -log((similarities / 2) + 0.5)

k.nearest.neighbors <- function(i, distance, k = 25) 
{
  return(order(distance[i, ])[2:(k + 1)])
}

# Probability of installation as the mean of those neighbors
installation.probability <- function(user, package, user.package.matrix, distances, k = 25)
{
  neighbors <- k.nearest.neighbors(package, distances, k = k)
  return(mean(sapply(neighbors, function(neighbors) {user.package.matrix[user, neighbors]})))
}

neighbors <- k.nearest.neighbors(1, distances, k = 25)
installation.probability(1, 1, user.package.matrix, distances) # the probability of installing package1 for user1 is 0.76

most.possible.packages <- function(user, user.package.matrix, distances, k = 25)
{
  return(order(sapply(1:ncol(user.package.matrix),
                      function(package)
                        {
                        installation.probability(user, package, user.package.matrix, distances, k = k)
                      }),
               decreasing = T))
}

user <- 1 
listing <- most.possible.packages(user, user.package.matrix, distances)
colnames(user.package.matrix)[listing[1:10]] # the top10 recommended packages for user1 are those. 
