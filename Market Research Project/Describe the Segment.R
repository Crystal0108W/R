# Market Research: Compare the groups: Tables and Visualizations

# Step 1: simulating consumer segement data

# Define the structure of the data sets
segVars <- c("Age","Gender","Income","Kids","ownHome","Subscribe")
segVarTye <- c("norm","binom","binom","pois","binom","binom")
segName <- c("Suburb Mix","Urban hip","Travellers","Moving up")
SegSize <- c(100,50,80,70)

# Define the values of the data by specifying distributional parameters such as the meand for each variables within each segment
segMeans <- matrix(c(40, 0.5,55000,2,0.5,0.1,
                     24, 0.7,21000,1,0.2,0.2,
                     58,0.5,64000,0,0.7,0.05,
                     36,0.3,52000,2,0.3,0.2), ncol = length(segVars),byrow = TRUE)

# For normal variables, age and income, variance needs to be additionally specified
segSDs <- matrix(c(5,NA,12000,NA,NA,NA,
                   2,NA,5000,NA,NA,NA,
                   8,NA,21000,NA,NA,NA,
                   4,NA,10000,NA,NA,NA),ncol = length(segVars),byrow = TRUE)

# Generate Data
seg.df <- NULL
set.seed(12345)

for (i in seq_along(segName)) {
  cat(i,segName[i], "\n")
  
  #empty matrix to hold this particular segment's data
  this.seg <- data.frame(matrix(NA, nrow=SegSize[i], ncol = length(segVars)))
  
  # within segment, iterate over variables and draw appropriate random data
  for (j in seq_along(segVars)) { #and iterate over each variable
    if (segVarTye[j] == "norm") { # draw random normals
      this.seg[,j] <- rnorm(SegSize[i], mean = segMeans[1,j], sd = segSDs[i,j])
    } else if (segVarTye[j]=="pois"){ #draw counts
      this.seg[,j] <- rpois(SegSize[i], lambda = segMeans[i,j])
    } else if (segVarTye[j]=="binom") { # draw binomial
      this.seg[,j] <- rbinom(SegSize[i], size = 1, prob = segMeans[i,j])
    } else {
        stop("Bad segment data type: ", segVarTye[j])
      }
  }
  seg.df <- rbind(seg.df, this.seg)
}

