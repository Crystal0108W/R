# Market Research: Compare the groups: Tables and Visualizations
# In this project, I first created random segment datasets using if() function and for() loop
# Here I segmented the population based on age, gender, income, how many kids in household, own a home or not and if they subscribe to the cable or not.
# What I am trying to describe here is how those variables vary for different segments
# Then I described discrete attributes in tables and visualize it in barchart and histogram chart
# Lastly, I visualized the continuous attributes in boxplot.


# Step 1: simulating consumer segement data
# Define the structure of the data sets
segVars <- c("Age","Gender","Income","Kids","ownHome","Subscribe")
segVarTye <- c("norm","binom","norm","pois","binom","binom")
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

names(seg.df) <- segVars # Make the data frame names match what we defined
seg.df$Segment <- factor(rep(segName, time = SegSize))
seg.df$ownHome <- factor(seg.df$ownHome, labels = c("ownNo","ownYes"))
seg.df$Gender <- factor(seg.df$Gender, labels = c("Female","Male"))
seg.df$Subscribe <- factor(seg.df$Subscribe, labels = c("subNo","subYes"))

summary(seg.df)
save(seg.df, file = "C:/Users/Crystal/Desktop/SegSimulate_data.csv")

#Finding Descriptive by Group
mean(seg.df$Income[seg.df$Segment == "Moving up"])
by(seg.df$Income, seg.df$Segment, mean)
by(seg.df$Income, list(seg.df$Segment,seg.df$Subscribe), mean)
seg.income.mean <- aggregate(seg.df$Income,list(seg.df$Segment),mean) # Aggregate works almost identical to by in its list from
# The first advantage of aggregate() is that the result is a data frame. You can save the results of aggregate to an object which you can then index, subject to further computation. 
seg.df$segIncome <- seg.income.mean[seg.df$Segment,2]

install.packages("car")
library(car)
some(seg.df) # some()does a random sample of rows 

# The second advantage of aggregate is that aggregate(y~x) means to aggregate y according to the levels of x
aggregate(Income~Segment, data = seg.df, mean) # Aggregate(formula, data, FUN)
agg.data <- aggregate(Income~Segment + ownHome, data = seg.df, mean)
#The aggregate command allows us to compute functions of continuous variables, for any combination of factors 

# Compute frequencies  using table(factor1, factor2...)
with(seg.df, table(Segment, ownHome))
with(seg.df, table(Kids, Segment)) # In this case we are treating kids as a factor and not a number
# Calculate the total number of kids in all households in one segment 
xtabs(Kids~Segment, data = seg.df) # Alternative 1
aggregate(Kids ~ Segment, data = seg.df, sum) # Alternative 2
seg.tab <- with(seg.df, table(Kids, Segment))
apply(seg.tab*0.7, 2, sum)
colSums(seg.tab*0.7)


# Step2: Visualize by Group - Discreet Data
# histogram()in lattice package understands formula notation including conditioning on a factor, which means to seperate the plot into multiple panes based on that factor

require(lattice)
histogram(~Subscribe | Segment, data = seg.df)
histogram(~Subscribe | Segment, data = seg.df, type = "count",
          layout = c(4,1), col = c("pink","salmon"))

histogram(~Subscribe | Segment + ownHome, data = seg.df)
# We can conclude the differences in subscription rate according to home ownership within segment are small. 
# An implication is that we should continue to market to both homeowners and non-homeowners

prop.table(table(seg.df$Subscribe, seg.df$Segment), margin =1)
barchart(prop.table(table(seg.df$Subscribe, seg.df$Segment),
                    margin = 2)[2, ], xlab = "Subscriber proportion by Segment", col = "pink")


# Visualize by Group - Continuous Data
# Alternative 1: aggregate() + barchart()
seg.mean <- aggregate(Income ~ Segment, data = seg.df, mean)
library(lattice)
barchart(Income ~ Segment, data = seg.mean, col = "pink")
seg.income.agg <- aggregate(Income ~ Segment + ownHome, data = seg.df, mean)
barchart(Income~Segment, data = seg.income.agg,
         groups = ownHome, auto.key = TRUE, 
         par.settings = simpleTheme(col = terrain.colors(2)))
# Alternative 2: boxplot or box-and-whiskers show more about the distributions of values
boxplot(Income ~ Segment, data = seg.df, yaxt = "n", ylab = "Income ($k)")
ax.seq <- seq(from = 0, to = 120000, by = 20000)
axis(side = 2, at = ax.seq, labels = paste(ax.seq/1000, "k", sep = ""), las = 1)

bwplot(Segment~Income, data = seg.df, horizontal = TRUE, xlab = "Income")
bwplot(Segment~Income|ownHome, data = seg.df, horizontal = TRUE, xlab = "Income") # breakout home ownership

