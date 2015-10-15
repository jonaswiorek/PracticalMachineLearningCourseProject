library(caret)

if(!file.exists("data")){
        dir.create("data")
}

fileUrlTraining <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrlTraining, destfile="./data/pml-training.csv", method="curl")
training <- read.csv("./data/pml-training.csv")

fileUrlTesting <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileUrlTesting, destfile="./data/pml-testing.csv", method="curl")
testing <- read.csv("./data/pml-testing.csv")

#Convert factor variables to indicator variables
#dummies <- dummyVars(classe ~ ?, data = training)
#head(predict(dummies, newdata = training))

# Remove zero covariates
# Remove variates with nzv = TRUE
nzv <- nearZeroVar(training[,-160], saveMetrics = TRUE)
nzv

trainingNzv <- training[, c('new_window', 
                           'kurtosis_roll_belt', 
                           'kurtosis_picth_belt',
                           'kurtosis_yaw_belt',
                           'skewness_roll_belt',
                           'skewness_roll_belt.1',
                           'skewness_yaw_belt',
                           'max_yaw_belt',
                           'min_yaw_belt',
                           'amplitude_yaw_belt',
                           'avg_roll_arm',
                           'stddev_roll_arm',
                           'var_roll_arm',
                           'avg_pitch_arm',
                           'stddev_pitch_arm',
                           'var_pitch_arm',
                           'avg_yaw_arm',
                           'stddev_yaw_arm',
                           'var_yaw_arm',
                           'kurtosis_roll_arm',
                           'kurtosis_picth_arm',
                           'kurtosis_yaw_arm',
                           'skewness_roll_arm',
                           'skewness_pitch_arm',
                           'skewness_yaw_arm',
                           'max_roll_arm',
                           'min_roll_arm',
                           'min_pitch_arm',
                           'amplitude_roll_arm',
                           'amplitude_pitch_arm',
                           'kurtosis_roll_dumbbell',
                           'kurtosis_picth_dumbbell',
                           'kurtosis_yaw_dumbbell',
                           'skewness_roll_dumbbell',
                           'skewness_pitch_dumbbell',
                           'skewness_yaw_dumbbell',
                           'max_yaw_dumbbell',
                           'min_yaw_dumbbell',
                           'amplitude_yaw_dumbbell',
                           'kurtosis_roll_forearm',
                           'kurtosis_picth_forearm',
                           'kurtosis_yaw_forearm',
                           'skewness_roll_forearm',
                           'skewness_pitch_forearm',
                           'skewness_yaw_forearm',
                           'max_roll_forearm',
                           'max_yaw_forearm',
                           'min_roll_forearm',
                           'min_yaw_forearm',
                           'amplitude_roll_forearm',
                           'amplitude_yaw_forearm',
                           'avg_roll_forearm',
                           'stddev_roll_forearm',
                           'var_roll_forearm',
                           'avg_pitch_forearm',
                           'stddev_pitch_forearm',
                           'var_pitch_forearm',
                           'avg_yaw_forearm',
                           'stddev_yaw_forearm',
                           'var_yaw_forearm')]


preProcess(training[,-160], method = "pca", thresh = 0.9)

