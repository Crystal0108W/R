# Clustering - Unsupervised
seg.raw <- read.csv("http://goo.gl/qw303p")
colnames(seg.raw)[colnames(seg.raw) == "Segment"] <-c("ValidateSegment")

# Step1 : Transform the data if needed for a particular clustering method; for instance, some methods require all numeric data (e.g., kmeans(), mclust()) or all categorical data
# Step2 : Compute a distance matrix if needed; some methods require a precomputed matrix of similarity in order to group observations 
# Step3 : Apply the clustering method and save its result to an object. For some methods this requires specifying the number (K) of groups desired (e.g., kmeans(), poLCA())
# Step4 : For some methods, further parse the object to obtain a solution with K groups (e.g., hclust()).
# Step5 : Examine the solution in the model object with regard to the underlying data, and consider whether it answers a business question.


# Classification - Supervised
