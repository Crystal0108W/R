####################Principal Components Analysis#####################
install.packages("psych")
library(psych)

### Pick Number of factors
# Example1
fa.parallel(USJudgeRatings[,-1], fa="pc", n.iter = 100,
            show.legend = TRUE, main = "Scree plot with parallel analysis")

# the plot displays the scree test based on the observed eigenvalues (as straignt line), the 
# mean eigenvalues derived from 100 rrandom data matrices, and the eigenvalyes greater than 1 criteria. 
# criteria 1: Components with eigenvalues less than 1 explain less variance than contained in a single variable
# criteria 2: components above the elbow are retained
# criteria 3: If an eigenvalue based on real data is larger than the average corresponding eigenvalues from a set of random data matrices, that component is retained.
pc<- principal(USJudgeRatings[,-1], nfactors = 1)


#Example2
fa.parallel(Harman23.cor$cov, n.obs = 302, fa = "pc", n.iter = 100,
            show.legend = FALSE, main = "Scree plot with parallel analysis")
pc <- principal(Harman23.cor$cov, nfactors = 2, rotate = "none")
pc

### Rotation
install.packages("GPArotation")
library(GPArotation)
rc <- principal(Harman23.cor$cov, nfactors = 2, rotate = "varimax")
rc

### Obtaining principal components scores
library(psych)
pc<- principal(USJudgeRatings[,-1], nfactors = 1, score = TRUE)
head(pc$scores)
cor(USJudgeRatings$CONT, pc$scores)
rc <- principal(Harman23.cor$cov, nfactors = 2, rotate = "varimax")
round(unclass(rc$weights),3)

####################Exploratory Factor Analysis#####################
###Step1
options(digits = 2)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
correlations

###Step2: Deside Number of Factors
fa.parallel(correlations, n.obs = 112, fa = "both", n.iter = 100,
            main = "Scree plots with parallel analysis")

###Step3: extracting common factors
fa <- fa(correlations, nfactors = 2, rotate = "none", fm ="pa")
fa

###Step3: rotating factors
fa.varimax <- fa(correlations, nfactors = 2, rotate = "promax", fm = "pa")
fa.varimax

factor.plot(fa.varimax, labels = rownames(fa.varimax$loadings))
fa.diagram(fa.varimax, simple = FALSE)

###Step3: Factor Scores
fa <- fa(correlations, nfactors = 2, rotate = "none", fm ="pa", score = TRUE)
fa
fa.varimax$weights
