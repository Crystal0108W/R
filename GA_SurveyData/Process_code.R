SurveyList <- unique(PP$Source)  # Check for unique values for Survey Sources
SurveyList <- as.list(SurveyList)

# extract and arrange survey data for each survey in the survey list
for (i in 1:6) {
  Name <- subset(PP, Source == SurveyList[i])
  Name <- Name[, c(9, 8, 1, 6)]
  filename <- paste('/Users/yw7986/Desktop/', SurveyList[i], '.csv', sep = "")
  write.csv(Name, file = filename)
}
# Use for loop to repeat steps that operate on the same codes
# Use paste to help write and save csv files with different names within for loop
