df <- read.csv("/Users/yw7986/Desktop/example.csv")

distance.matrix <- function(df) 
{
  distance <- matrix(rep(NA, nrow(df) ^ 2), nrow = nrow(df))
  for (i in 1:nrow(df)) 
    {
    for (j in 1:nrow(df)) 
      {
      distance[i, j] <- sqrt((df[i, 'X'] - df[j, 'X']) ^ 2 + (df[i, 'Y'] - df[j, 'Y']) ^ 2)
      } 
    }
  return(distance) 
}

k.nearest.neighbors <- function(i, distance, k = 5) 
{
  return(order(distance[i, ])[2:(k + 1)])
}

knn <- function(df, k = 5)
{
  distance <- distance.matrix(df)
  predictions <- rep(NA, nrow(df))
  
  for (i in 1:nrow(df)) {
    indices <- k.nearest.neighbors(i, distance, k = k)
    predictions[i] <- ifelse(mean(df[indices, 'Label']) > 0.5, 1, 0) }
  
  return(predictions) 
}


dist <- distance.matrix(df)
dist_order <- k.nearest.neighbors(100, dist, k = 5) # for case No.100, the list returns the closest 5 cases to it.
dist_order <- k.nearest.neighbors(1, dist, k = 5)
neighbor_prediction <- knn(df, k = 5)

df <- transform(df, KNNPredictions = knn(df, k = 5))
sum(with(df, Label != KNNPredictions)) #7 cases out of 100 cases are incorrect; 93% correct rate
