
#####Data Structure
# Data, functions, and just about everything else that can be stored and named are objects.
# Every object has attributes: meta-information describing the characteristics of the object.
# The class of an object can be read and set with the class() function.


#####There are two fundamental data types: atomic vectors and generic vectors: 
  ### Atomic vectors are arrays that contain a single data type. 

x <- c(1,2,3,4,5,6,7,8)
class(x)

# A matrix is an atomic vector that has a dimension attribute, dim, containing two ele- ments (number of rows and number of columns)
attr(x, "dim") <- c(2,4)
class(x)
attributes(x)

# Row and column names can be attached by adding a dimnames attribute
attr(x, "dimnames") <- list(c("A1", "A2"),
                            c("B1", "B2", "B3", "B4"))

# The matrix can be returned to a one-dimensional vector by removing the dim attribute:
attr(x, "dim") <- NULL
class(x)


  ### Generic vectors, also called lists, are collections of atomic vectors.

# Lists are collections of atomic vectors and/or other lists. 
# Data frames are a special type of list, where each atomic vector in the collection has the same length

head(iris)

#It has a names attribute (a character vector of variable names), a row.names attribute (a numeric vector identi- fying individual plants), and a class attribute with the value "data.frame". 
#Each vector represents a column (variable) in the data frame. This can be easily seen by printing the data frame with the unclass() function and obtaining the attributes with the attributes() function

unclass(iris)
attributes(iris)
#The str() function dis- plays the object’s structure, and the unclass() function can be used to examine the object’s contents directly.
#The length() function indicates how many components the object contains, and the names() function provides the names of these components. You can use the attributes() function to examine the attributes of the object.

#Executing sapply(iris, class) returns the class of each component in the object:
sapply(iris, class)

# INDEXING
 ## For Atomic vector: 
    ## Elements are extracted using object[index], where object is the vector and index is an integer vector
 ## For List: 
    ## For lists, components (atomic vectors or other lists) can be extracted using object[index], where index is an integer vector.
    ## Note that components are returned as a list. To get just the elements in the component, use object[[integer]]:

iris[2]
class(iris[2])
class(iris[[2]])

install.packages("reshape2")
library(reshape2)

set.seed(1234)
fit <- kmeans(iris[1:4], 3)
means <- fit$centers
dfm <- melt(means)
names(dfm) <- c("Cluster", "Measurement", "Centimeters")
dfm$Cluster <- factor(dfm$Cluster)
head(dfm)

library(ggplot2)
ggplot(dfm, aes(x=Measurement, y=Centimeters, group = Cluster)) + 
            geom_point(size = 3, aes(shape = Cluster, color=Cluster)) + 
            geom_line(size = 1, aes(color = Cluster)) + 
            scale_color_brewer(palette = "Pastel2") + 
            ggtitle("Profiles for Iris Clusters")

