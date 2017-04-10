#Get and load train data.
trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(trainURL,"pml-training.csv")
train <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!"))

#Get and load the final quiz testing data.
testURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(testURL,"pml-testing.csv")
quiz <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!"))

#Load packages
library(caret) #loads lattice and ggplot2
library(ISLR)

#Set seed for reproducibility and split training data into a train and test set for cross validation.
set.seed(913)
inTrain <- createDataPartition(train$classe, p = 3/4)[[1]]
training <- train[inTrain,]
testing <- train[-inTrain,]

#Explore data and determine which variables to use in a prediction model
nzv <- nearZeroVar(training, saveMetrics = TRUE)
nzv$naratio <- apply(training, 2, function(col)sum(is.na(col))/length(col))
nzvind <- nearZeroVar(training)

naind <- nzv[,5] > 0.95 
naind <- which(naind == TRUE)

combind <- c(naind,nzvind)

trainingsub <- training[,-combind]
trainingsub <- subset(trainingsub, select = -c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp))

summary(subset(trainingsub, select = -c(classe)))

#Train variety of models
rf <- train(classe ~ ., data = trainingsub, method = "rf")
gbm <- train(classe ~ ., data = trainingsub, method = "gbm", verbose = FALSE)
lda <- train(classe ~ ., data = trainingsub, method = "lda")

#Assess Accuracy Using Cross Validation Data Set
predrf <- predict(rf, testing)
predgbm <- predict(gbm, testing)
predlda <- predict(lda, testing)

confusionMatrix(predrf, testing$classe)$overall[1]
confusionMatrix(predgbm, testing$classe)$overall[1]
confusionMatrix(predlda, testing$classe)$overall[1]

#Create Ensemble Model and Assess Accuracy
combrf <- data.frame(predrf, predgbm, predlda, classe = testing$classe)
combModFit <- train(classe ~ ., method = "rf", data = combrf)
combPred <- predict(combModFit, combrf)
confusionMatrix(combPred, testing$classe)$overall[1]


#Apply final prediction model to the quiz data set
predquiz <- predict(combModFit, quiz)
