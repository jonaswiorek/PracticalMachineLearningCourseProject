library(caret)
library(dplyr)
library(rpart)
library(rattle)

if(!file.exists("data")){
        dir.create("data")
}

fileUrlTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
#download.file(fileUrlTraining, destfile="./data/pml-training.csv", method="curl") #OSx
download.file(fileUrlTraining, destfile="./data/pml-training.csv") #Windows
trainingValidation <- read.csv("./data/pml-training.csv")
set.seed(1717)
inTrain <- createDataPartition(y = trainingValidation$classe, p = 0.7, list = FALSE)
training <- trainingValidation[inTrain,]
validation <- trainingValidation[-inTrain,]

fileUrlTesting <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(fileUrlTesting, destfile="./data/pml-testing.csv", method="curl") #OSx
download.file(fileUrlTesting, destfile="./data/pml-testing.csv") #Windows
testing <- read.csv("./data/pml-testing.csv")

# Remove zero covariates
# Remove variates with nzv = TRUE
nzv <- nearZeroVar(training[,-160])
trainingNzv <- training[,-nzv]
validationNzv <- validation[,-nzv]

#sapply(trainingNzv, class)
#preProc <- preProcess(trainingNzv %>% select(-c(X, user_name, cvtd_timestamp, classe)), method = "pca", thresh = 0.9)
#trainingNzvPc <- predict(preProc, trainingNzv%>% select(-c(X, user_name, cvtd_timestamp, classe)))

# Remove covariate X
trainingX <- training %>% select(-X)
validationX <- validation %>% select(-X)
trainingNzvX <- trainingNzv %>% select(-X)
validationNzvX <- validationNzv %>% select(-X) 
#trainingX <- training %>% select(-X) 

# Remove coulms with too many NAs
nasNzvX <- as.vector(which(apply(trainingNzvX, 2, function(x) sum(is.na(x))/length(x)>.9)))
nasX <- as.vector(which(apply(trainingX, 2, function(x) sum(is.na(x))/length(x)>.9)))

trainingXNa <- trainingX[,-nasX]
validationXNa <- validationX[,-nasX]
trainingNzvXNa <- trainingNzvX[,-nasNzvX]
validationNzvXNa <- validationNzvX[,-nasNzvX]

# Remove additional covariates
trainingNzvXNaAdd <- trainingNzvXNa %>% select(-c(raw_timestamp_part_1, 
                                             raw_timestamp_part_2,
                                             cvtd_timestamp,
                                             num_window))
validationNzvXNaAdd <- validationNzvXNa %>% select(-c(raw_timestamp_part_1, 
                                                 raw_timestamp_part_2,
                                                 cvtd_timestamp,
                                                 num_window)) 

modelFitXNa <- train(classe ~ ., method = "rpart", data = trainingXNa)
modelFitNzvXNa <- train(classe ~ ., method = "rpart", data = trainingNzvXNa)
#modelFitNzvXNaAddRf <- train(classe ~ ., method = "rf", data = trainingNzvXNaAdd)
modelFitNzvXNaAddRf <- randomForest(classe ~ ., data = trainingNzvXNaAdd, importance = TRUE) 
modelFitNzvX <- train(classe ~ ., method = "rpart", data = trainingNzvX)
#modelFitX <- train(classe ~ ., method = "rpart", data = trainingX)
#modelFitNzvPc <- train(trainingNzv$classe ~ ., method = "rpart", data = trainingNzvPc)

print(modelFitNzvX$finalModel)

fancyRpartPlot(modelFitXNa$finalModel)
fancyRpartPlot(modelFitNzvXNa$finalModel)
fancyRpartPlot(modelFitNzvXNaAddRf$finalModel)
fancyRpartPlot(modelFitNzvX$finalModel)
#fancyRpartPlot(modelFitX$finalModel)
#fancyRpartPlot(modelFitNzvPc$finalModel)

predictionXNa <- predict(modelFitXNa, newdata = validationXNa)
predictionNzvXNa <- predict(modelFitNzvXNa, newdata = validationNzvXNa)
predictionNzvXNaRf <- predict(modelFitNzvXNaRf, newdata = validationNzvXNa)
predictionNzvXNaAddRf <- predict(modelFitNzvXNaAddRf, newdata = validationNzvXNaAdd)
predictionNzvX <- predict(modelFitNzvX, newdata = validationNzvX)

table(predictionXNa, validation$classe)

confusionMatrix(predictionXNa, validation$classe)
confusionMatrix(predictionNzvXNa, validation$classe)
confusionMatrix(predictionNzvXNaRf, validation$classe)
confusionMatrixNzvXNaAddRf <- confusionMatrix(predictionNzvXNaAddRf, 
                                              validation$classe)



#qplot(classe, predictionNzvX, data = validationNzvX)




#nsapply(training, class)

#preProc <- preProcess(trainingNzv %>% select(-c(user_name, cvtd_timestamp, classe)), method = "rpart", thresh = 0.8)



#Convert factor variables to indicator variables
#dummies <- dummyVars(classe ~ ?, data = training)
#head(predict(dummies, newdata = training))




NzvXNaAddRf