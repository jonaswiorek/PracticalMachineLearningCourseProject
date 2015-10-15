#1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

summary(predictors)
summary(diagnosis)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

#or

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(mixtures$Superplasticizer)

hist(log10(mixtures$Superplasticizer))

# There are values of zero so when you take the log() transform those values will 
# be -Inf.

# NOT
# The log transform produces negative values which can not be used by some 
# classifiers.

#3 
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

preProcess(il, method = "pca", thresh = 0.8)
# Created from 251 samples and 12 variables
# 
# Pre-processing:
# - centered (12)
# - principal component signal extraction (12)
# - scaled (12)
# 
# PCA needed 7 components to capture 80 percent of the variance

preProcess(il, method = "pca", thresh = 0.9)
# Created from 251 samples and 12 variables
# 
# Pre-processing:
#         - centered (12)
# - principal component signal extraction (12)
# - scaled (12)
# 
# PCA needed 9 components to capture 90 percent of the variance

#4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[, c(1, 58:69)]
testingIL <- testing[, c(1, 58:69)]

# Method 1 the IL predictors as they are
modelFit <- train(trainingIL$diagnosis ~ ., method = "glm", data = trainingIL)
confusionMatrix(testingIL$diagnosis, predict(modelFit, testingIL))

# Accuracy : 0.6463 

# Method 2 using PCA with 7 principal components to capture 80% of the variance
preProc7 <- preProcess(trainingIL, method = "pca", pcaComp = 7)
trainPC7 <- predict(preProc7, trainingIL)
modelFit7 <- train(trainingIL$diagnosis ~ . , method = "glm", data = trainPC7)

testPC7 <- predict(preProc7, testingIL)
confusionMatrix(testingIL$diagnosis, predict(modelFit7, testPC7))

# Accuracy : 0.7195  







# library(dplyr)
# training[,58:69] %>% names()
# il <- training[,58:69]
# ilTest <- testing[,58:69]
# 
# M <- abs(cor(il))
# diag(M) <- 0
# which(M > 0.7, arr.ind = TRUE)
# 
# plot(il[,6], il[,3])
# il63 <- il[,c(6,3)]
# prComp63 <- prcomp(il63)
# plot(prComp63$x[,1], prComp63$x[,2])
# prComp63$rotation
# 
# prComp <- prcomp(il)
# prComp$rotation
# 
# preProc7 <- preProcess(il, method = "pca", pcaComp = 7)
# trainPC7 <- predict(preProc7, il)
# modelFit7 <- train(training$diagnosis ~ . , method = "glm", data = trainPC7)
# 
# testPC7 <- predict(preProc7, ilTest)
# confusionMatrix(testing$diagnosis, predict(modelFit7, testPC7))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# preProc7 <- preProcess(training[,58:69], method = "pca", pcaComp = 7)
# preProc8 <- preProcess(training[,58:69], method = "pca", pcaComp = 8)
# preProc9 <- preProcess(training[,58:69], method = "pca", pcaComp = 9)
# preProc10 <- preProcess(training[,58:69], method = "pca", pcaComp = 10)
# 
# 
# predict7 <- predict(preProc7, training)
# 
# modelFit <- train(training$diagnosis ~ ., method = "glm", data = training)
# modelFit7 <- train(training$diagnosis ~ ., method = "glm", data = predict7)
# 
# 
# 
# confusionMatrix(predict7, training$diagnosis)
# 
# 
# smallAdData <- training[,58:69]
# prComp <- prcomp(smallAdData)
# prComp$sdev
# 
# 
# preProc7 <- preProcess(training[,58:69], method = "pca", pcaComp = 7)
