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

# Make transformations
# using function skweness from e1071 package
library(e1071)
# skweness example
skewness(segData$AngleCh1)
# apply skweness on all columns
skewValues <- apply(segData, 2, skewness)
head(skewValues)

# example trans <- preProcess(segData,method = c("BoxCox", "center", "scale", "pca"))

# Filtering
# indicated which columns should be removed
nearZeroVar(segData)

correlations <- cor(segData)
dim(correlations)
correlations[1:4, 1:4]

# visualize
library(corrplot)
corrplot(correlations, order = "hclust")
