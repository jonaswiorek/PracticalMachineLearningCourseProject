#1 
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

set.seed(125)
inTrain <- createDataPartition(y=segmentationOriginal$Case, p = 0.7, list = FALSE)
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,]

modFit <- train(Case ~ ., method = "rpart", data = training)
modFit$finalModel

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2

a <- c("TotalIntenCh2" = 23000,"FiberWidthCh1" = 10, "PerimStatusCh1" = 2)
b <- c("TotalIntenCh2" = 50000,"FiberWidthCh1" = 10, "VarIntenCh4" = 100)
c <- c("TotalIntenCh2" = 57000,"FiberWidthCh1" = 8, "VarIntenCh4" = 100)
d <- c("FiberWidthCh1" = 8, "VarIntenCh4" = 100, "PerimStatusCh1" = 2)
a;b;c;d


test <- testing[1:4,c("FiberWidthCh1")]

predA <- predict(modFit, )
