# Hierarchical Linear Model - Determine Individual Level Effect - Estimate the values in the model for each respondent
# Roller Coaster Conjoint Analysis: levels of maximum speed, height, construction type , and theme.

# Simulating Ratings-Based Conjoint Data
set.seed(12345)
resp.id <- 1:200
nques <- 16
speed <- sample(as.factor(c("40", "50", "60", "70")), size = nques, 
                replace = TRUE)
height <- sample(as.factor(c("200", "300", "400")), size=nques, replace=TRUE)
const <- sample(as.factor(c("Wood", "Steel")), size= nques, replace=TRUE)
theme <- sample(as.factor(c("Dragon", "Eagle")), size=nques, replace=TRUE)
profiles.df <- data.frame(speed, height, const, theme)
profiles.model <- model.matrix(~ speed + height + const + theme, data = profiles.df) # converts the list of design attributes into coded variables;
install.packages("MASS")
library(MASS)
weights <- mvrnorm(length(resp.id),
                   mu = c(-3, 0.5, 1, 3, 2, 1, 0, -0.5),
                   Sigma = diag(c(0.2, 0.1, 0.1, 0.1, 0.2, 0.3, 1, 1))) # draw unique preference weights for each respondent.

# Simulate Conjoint Data
conjoint.df <- NULL 
for(i in seq_along(resp.id)) {
  utility <- profiles.model %*% weights[i, ] + rnorm(16) # add noise in by rnorm()
  rating <- as.numeric(cut(utility, 10))  # put on a 10-point scale
  conjoint.resp <- cbind(resp.id = rep(i, nques), rating, profiles.df)
  conjoint.df <- rbind(conjoint.df,conjoint.resp)
}


# Regular Linear Modelling 
summary(conjoint.df)
by(conjoint.df$rating, conjoint.df$height, mean)
ride.lm <- lm(rating ~ speed + height + const + theme, data = conjoint.df)
summary(ride.lm)
# The highest rated roller coaster on average would have a top speed of 70 mph, a height of 300 ft, steel construction, and the dragon theme
# BUT, The coefficients are estimated on the basis of designs that mostly combine both desirable and undesirable attributes, and are not as reliable at the extremes of preference. 
# Additionally, it could happen that few people prefer that exact combination even though the individual features are each best on average.
