# Q1
library(caret)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
lapply(vowel.train, class)
lapply(vowel.test, class)

set.seed(33833)

modFitVowel1 <- train(y ~ ., model = "rf", data = vowel.train)
modFitVowel2 <- train(y ~ ., model = "gbm", data = vowel.train)

predictVowel1 <- predict(modFitVowel1, newdata = vowel.test)
predictVowel2 <- predict(modFitVowel2, newdata = vowel.test)

print(confusionMatrix(predictVowel1, vowel.test$y))
# Accuracy : 0.6061
# Accuracy : 0.6082 (Mac)
print(confusionMatrix(predictVowel2, vowel.test$y))
# Accuracy : 0.5996
# Accuracy : 0.5779 (Mac)

print(confusionMatrix(predictVowel1[which(predictVowel1 == predictVowel2)], vowel.test$y[which(predictVowel1 == predictVowel2)]))
#confusionMatrix(predictVowel2[which(predictVowel1 == predictVowel2)], vowel.test$y[which(predictVowel1 == predictVowel2)])
# Accuracy : 0.6183
# Accuracy : 0.6178 (Mac)

#Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
# random forest
modFitAdRf <- train(diagnosis ~ ., model = 'rf', data = training)
# gradient boosting machine
modFitAdGbm <- train(diagnosis ~ ., model = 'gbm', data = training)
# linear discriminant analysis
modFitAdLda <- train(diagnosis ~ ., model = 'lda', data = training)

predictAdRf <- predict(modFitAdRf, newdata = testing)
predictAdGbm <- predict(modFitAdGbm, newdata = testing)
predictAdLda <- predict(modFitAdLda, newdata = testing)

print(confusionMatrix(predictAdRf, testing$diagnosis))
# Accuracy : 0.7683
print(confusionMatrix(predictAdGbm, testing$diagnosis))
# Accuracy : 0.7927
print(confusionMatrix(predictAdLda, testing$diagnosis))
# Accuracy : 0.7805
# Accuracy : 0.7561 /(Mac)

predDF <- data.frame(predictAdRf, predictAdGbm, predictAdLda, diagnosis = testing$diagnosis)

modFitCombo <- train( diagnosis ~ ., model = 'rf', data = predDF)
predictCombo <- predict(modFitCombo, predDF)

print(confusionMatrix(predictCombo, predDF$diagnosis))
# Accuracy : 0.7927

#3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
modFitConcreteLasso <- train(CompressiveStrength ~ ., method = 'lasso', data = training) 

