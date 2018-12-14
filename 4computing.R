rm(list = ls())

# load lib
if (!require("pacman")) suppressPackageStartupMessages(install.packages("pacman"))
pacman::p_load("e1071","ipred","MASS")
library(AppliedPredictiveModeling)
library(caret)

# load data
data(twoClassData)

# check data
str(predictors)
str(classes)

# create stratified random splits of the data based on the classes : createDataPartition
set.seed(1)
trainingRows <- createDataPartition(classes,
                                    p = .80,
                                    list=FALSE)
head(trainingRows)

# set training set
trainPredictors <- predictors[trainingRows, ]
trainClasses <- classes[trainingRows]
# set test set
testPredictors <- predictors[-trainingRows, ]
testClasses <- classes[-trainingRows]

str(trainPredictors)
str(testPredictors)

# createDataParition could be used again with times to generate multiple splits
repeatedSplits <- createDataPartition(trainClasses, 
                                      p = .80,
                                      times = 3)
str(repeatedSplits)

# k-fold cross-valdiation
cvSplits <- createFolds(trainClasses,
                         k=10,
                         returnTrain = T)
str(cvSplits)
fold1 <- cvSplits[[1]]
cvPredictors1 <- trainPredictors[fold1,]
cvClasses <- trainClasses[fold1]
nrow(trainPredictors)
nrow(cvPredictors1)

# Basic model building
trainPredictors <- as.matrix(trainPredictors)
trainPredictors
knnFit <- knn3(x=trainPredictors,y=trainClasses, k =5)
knnFit
testPredictions <- predict(knnFit, newdata = testPredictors,
                           type = "class")
head(testPredictions)
str(testPredictions)

# Model tuning
data("GermanCredit")
set.seed(1056)
# remove zero variance columns
GermanCredit <- GermanCredit[, -nearZeroVar(GermanCredit)]
GermanCredit$CheckingAccountStatus.lt.0 <- NULL
GermanCredit$SavingsAccountBonds.lt.100 <- NULL
GermanCredit$EmploymentDuration.lt.1 <- NULL
GermanCredit$EmploymentDuration.Unemployed <- NULL
GermanCredit$Personal.Male.Married.Widowed <- NULL
GermanCredit$Property.Unknown <- NULL
GermanCredit$Housing.ForFree <- NULL

## Split the data into training (80%) and test sets (20%)
set.seed(100)
inTrain <- createDataPartition(GermanCredit$Class, p = .8)[[1]]
# create test and training set
GermanCreditTrain <- GermanCredit[ inTrain, ]
GermanCreditTest <- GermanCredit[-inTrain, ]

# tunelength = cost values evaluated
# traincontrol = cross validation
svmFit <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "svmRadial",
                preProc = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5))
svmFit
plot(svmFit, scales = list(x = list(log = 2)))
predictedClasses <- predict(svmFit, GermanCreditTest)
str(predictedClasses)
# Use the "type" option to get class probabilities
predictedProbs <- predict(svmFit, newdata = GermanCreditTest, type = "prob")
head(predictedProbs)


logisticReg <- train(Class ~ .,
                data = GermanCreditTrain,
                method = "glm",
                trControl = trainControl(method = "repeatedcv",
                                         repeats = 5))
logisticReg
# To compare these two models based on their cross-validation statistics,
# the resamples function can be used with models that share a common set of
# resampled data sets.
resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg))
summary(resamp)

modelDifferences <- diff(resamp)
summary(modelDifferences)
