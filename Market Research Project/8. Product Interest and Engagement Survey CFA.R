# Structural Equation Modeling(SEM)
# Motivation:  With SEM, it is feasible to do several things that improve our models:
# - to include multiple influences, 
# - to posit unobserved concepts that underlie the observed indicators (i.e., constructs such as brand preference, likelihood to purchase, and satisfaction), to specify how those concepts influence one another, 
# - to assess the model’s overall congruence to the data, 
# - and to determine whether the model fits the data better than alternative models.

# structural model: The set of relationships among the latent variables
# measurement model: the linkage between those elements and the observed, manifest variables

# Structural models differ from linear regression models in these regards: 
# 1. They assess the relationships among many variables, with models that may be more complex than simply predictors and outcomes.
# 2. Those relationships allow for latent variables that represent underlying constructs that are thought to be manifested imperfectly in the observed data. 
# 3. The models allow relationships to have multiple “downstream” effects.

# Approaches: 
# 1. covariance based (CB-SEM) approach, (lavaan package)
# 2. partial least squares (PLS-SEM) approach


# Scale Assesment: Confirmatory Factor Analysis (CFA)
# Question: Is the PIES scheme a good model for some set of survey responses for the product? 
# If we confirm that PIES is a good model, we will be much more confident in using this survey data to draw inferences about product involvement than if we had not assessed the model. 
# use a particular application of SEM known as CFA to address the question.

# PIES scale: (confirm that this is a good model)
# • General scale
#   ¶ ITEM NAME(digital camera) are not very important to me.
#   ¶ I never think about ITEM NAME(digital camera)
#   ¶ I am very interested in ITEM NAME(digital camera)
# • Feature scale
#   ¶ In choosing a ITEM NAME(digital camera) I would look for some specific features or options.
#   ¶ If I chose a new ITEM NAME(digital camera)  I would investigate the available choices in depth.
#   ¶ Some ITEM NAME(digital camera) are clearly better than others.
# • Image scale
#   ¶ When people see someone’s ITEM NAME(digital camera) they form an opinion of that person.
#   ¶ You can learn a lot about a person by seeing the person’s ITEM NAME(digital camera).
#   ¶ It is important to choose a ITEM NAME(digital camera) that matches one’s image.



# Simulate PIES
install.packages(c("lavaan", "semTools", "semPlot"))
install.packages("data.table", type = "source")
library(lavaan)
library(semTools)
library(semPlot)

piesModel <- " General =~ i1 + i2 + i3
               Feature =~ i4 + i5 + i6  + i7
               Image   =~ i8 + i9 + i10 + i11
               PIES =~ General + Feature + Image "

#“=∼” symbol as “is manifested by,”

piesDataModel <- " General =~ 0.9*i1 + 0.7*i2 + 0.5*i3
                   Feature =~ 0.3*i3 + 0.7*i4 + 0.9*i5 + 0.5*i6  + 0.9*i7
                   Image   =~ 0.2*i3 + 0.8*i8 + 0.9*i9 + 0.8*i10 + 0.7*i11 
                   PIES =~ 0.7*General + 0.8*Feature + 0.8*Image"

set.seed(12345)
piessimData.norm <- simulateData(piesDataModel, sample.nobs = 3600)

# Estimate the PIES CFA Model
piesSimData <- read.csv("http://goo.gl/yT0XwJ")

# 1. Define the model taht we wish to evaluate
library(lavaan)
pies.fit <- cfa(piesModel, data = piesSimData) # fit this model to data using cfa(MODEL, data=DATA)
summary(pies.fit, fit.measures = TRUE)
# fit indices are strong (e.g., CFI = 0.975) and residuals are low (e.g., RMSEA = 0.041).
# The lower part of the summary shows that model parameters for the paths of latent variables to items
# the PIES hierarchical model fits well and—because the factor-item loadings are around 1.0

install.packages("semPlot")
library(semPlot)
semPaths(pies.fit, what = "est")
