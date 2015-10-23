#1 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rattle)

set.seed(125)
inTrain <- createDataPartition(y=segmentationOriginal$Case, list = FALSE)
training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]

modFit <- train(Class ~ ., method = "rpart", data = training)
modFit$finalModel
fancyRpartPlot(modFit$finalModel)

summary(modFit)

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

# INCORRECT!
# a. TotalIntench2 < 45323.5 => PS
# b. TotalIntench2 > 45323.5, FiberWidthCh1 > 9.673245 => WS
# c. TotalIntench2 > 45323.5, FiberWidthCh1 < 9.673245 => PS
# d. FiberWidthCh1 < 9.684201 => PS

# a. TotalIntench2 < 45323.5 => PS
# b. TotalIntench2 > 45323.5, FiberWidthCh1 > 9.673245 => WS
# c. TotalIntench2 > 45323.5, FiberWidthCh1 < 9.673245 => PS
# d. FiberWidthCh1 < 9.684201 => Not Possible to predict


#2
# If K is small in a K-fold cross validation is the bias in the estimate 
# of out-of-sample (test set) accuracy smaller or bigger?
# The bias is larger 

# If K is small is the variance in the estimate of out-of-sample (test set) 
# accuracy smaller or bigger?
# The variance is smaller

# Is K large or small in leave one out cross validation?
# Under leave one out cross validation K is equal to the sample size.


#3
library(pgmm)
data(olive)
olive = olive[,-1]

modFitOlive <- train(Area ~ ., method = 'rpart', data = olive)

newdata = as.data.frame(t(colMeans(olive)))

predOlive <- predict(modFitOlive, newdata = newdata)
predOlive

# 2.783. There is no reason why this result is strange. !INCORRECT

# 2.783. It is strange because Area should be a qualitative variable - 
# but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata

#4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFitSA <- train(chd ~ age + alcohol + obesity + tobacco + typea +ldl, 
                  method = 'glm', family = binomial , data = trainSA)



missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, predict(modFitSA, newdata = trainSA))
# 0.2727273
missClass(testSA$chd, predict(modFitSA, newdata = testSA))
# 0.3116883

#confusionMatrix(trainSA$chd, predict(modFitSA, newdata = trainSA))

#5
library(dplyr)
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
lapply(vowel.train, class)
lapply(vowel.test, class)

set.seed(33833)

modFitVowel <- train( y ~ ., model='rf', data= vowel.train)
modFitVowel

varImp(modFitVowel)
# rf variable importance
# 
# Overall
# x.1  100.000
# x.2   99.357
# x.5   45.096
# x.6   29.394
# x.8   21.831
# x.4   10.049
# x.9    6.487
# x.3    5.124
# x.7    2.707
# x.10   0.000

