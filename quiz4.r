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
plot(modFitConcreteLasso$finalModel)

object <- enet(as.matrix(training %>% select(-CompressiveStrength)), training$CompressiveStrength, lambda = 0)
plot(object)


modFitConcreteEnet <- train(CompressiveStrength ~ ., method = 'enet', data = training) 
plot(modFitConcreteEnet$finalModel)


modFitConcreteLars <- train(CompressiveStrength ~ ., method = 'lars', data = training) 
plot(modFitConcreteLars$finalModel)
plot(modFitConcreteLars$finalModel, xvar = 'step')
x <- names(training %>% select(-CompressiveStrength))
legend("topleft", x, pch=8, lty=1:length(x), col=1:length(x), cex = 0.6)

# Prints plot that shows the progression of the coefficients as they are set to zero one by one
# Cement is the last coefficient to be set to zero as the penalty increases

#4
library(forecast)

library(lubridate)  # For year() function below
#dat = read.csv("~/../Desktop/gaData.csv") #PC
dat = read.csv("~/Desktop/gaData.csv") #Mac
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr)

plot(tstrain)
# Moving Average
lines(ma(tstrain,order = 3), col = 'red')
lines(ma(tstrain,order = 5), col = 'blue')
lines(ma(tstrain,order = 10), col = 'green')

# Library forecast
# Exponential smoothing state space model
# ets()
# BATS model (Exponential smoothing state space model with 
# Box-Cox transformation, ARMA errors, Trend and Seasonal components)
# bats()

modFitDat <- bats(tstrain)
fcast <- forecast(modFitDat, h = length(tstest), level = c(95,99))
plot(fcast)
lines(dat$visitsTumblr, col = 'red')
lines(tstrain, col = 'black')

# Confidence level for prediction intervals.
# Default level=c(80,95)
length(which((tstest < fcast$upper[,1]) & (tstest > fcast$lower[,1])))/length(tstest)
# 0.9617021

# Q5
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)

modFitSvm <- svm(CompressiveStrength ~ ., data= training)
predSvm <- predict(modFitSvm, newdata = testing)

rmse <- sqrt(1/length(predSvm)*sum((predSvm-testing$CompressiveStrength)^2))
rmse
# 6.715009