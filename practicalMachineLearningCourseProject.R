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
trainingNzvX <- trainingNzv %>% select(-X)
validationNzvX <- validationNzv %>% select(-X) 
#trainingX <- training %>% select(-X) 

modelFitNzvX <- train(classe ~ ., method = "rpart", data = trainingNzvX)
#modelFitX <- train(classe ~ ., method = "rpart", data = trainingX)
#modelFitNzvPc <- train(trainingNzv$classe ~ ., method = "rpart", data = trainingNzvPc)

#summary(modelFitNzvX)

fancyRpartPlot(modelFitNzvX$finalModel)
#fancyRpartPlot(modelFitX$finalModel)
#fancyRpartPlot(modelFitNzvPc$finalModel)


predictionNzvX <- predict(modelFitNzvX, newdata = validationNzvX, type = 'raw')


#qplot(classe, predictionNzvX, data = validationNzvX)




#nsapply(training, class)

#preProc <- preProcess(trainingNzv %>% select(-c(user_name, cvtd_timestamp, classe)), method = "rpart", thresh = 0.8)



#Convert factor variables to indicator variables
#dummies <- dummyVars(classe ~ ?, data = training)
#head(predict(dummies, newdata = training))




