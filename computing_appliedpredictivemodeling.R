# 3.8 computing
# instal package
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
# load data
data(segmentationOriginal)
# inspect data
str(segmentationOriginal)
# focus on train set
segData <- subset(segmentationOriginal, Case == "Train")
# class and cell saved into seprate vector 
cellID <- segData$Cell
class <- segData$Class
case <- segData$Case
# Now remove the columns
segData <- segData[, -(1:3)]
# The original data contained several "status" columns which were binary versions of the predictors
statusColNum <- grep("Status", names(segData))
statusColNum
segData <- segData[, -statusColNum]
