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
#  Accuracy : 0.6061
print(confusionMatrix(predictVowel2, vowel.test$y))
# Accuracy : 0.5996

print(confusionMatrix(predictVowel1[which(predictVowel1 == predictVowel2)], vowel.test$y[which(predictVowel1 == predictVowel2)]))
#confusionMatrix(predictVowel2[which(predictVowel1 == predictVowel2)], vowel.test$y[which(predictVowel1 == predictVowel2)])
# Accuracy : 0.6183


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
modFitAdRf <- train(diagnosis ~ ., model = 'rf', data = training)
modFitAdGbm <- train(diagnosis ~ ., model = 'gbm', data = training)
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

predDF <- data.frame(predictAdRf, predictAdGbm, predictAdLda, diagnosis = testing$diagnosis)

modFitCombo <- train( diagnosis ~ ., model = 'rf', data = predDF)
predictCombo <- predict(modFitCombo, data = predDF)

print(confusionMatrix(predictCombo, predDF$diagnosis))
# 
