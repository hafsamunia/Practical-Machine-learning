setwd("/Users/you-can-do-it/Desktop/Coursera_r_projrct/machine learning assign")
# load the packages
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
# read the data
training_csv<-read.csv('./pml-training.csv', header=T)
testing_csv<-read.csv('./pml-testing.csv', header=T)
# data partition
set.seed(12345)
inTrain<-createDataPartition(y=training_csv$classe, p=0.6, list=FALSE)
training<-training_csv[inTrain, ]
testing<-training_csv[-inTrain, ]
#Identification on Non-Zero Data
all_zero_colnames <- sapply(names(testing_csv), function(x) all(is.na(testing_csv[,x])==TRUE))
nznames <- names(all_zero_colnames)[all_zero_colnames==FALSE]
nznames <- nznames[-(1:7)]
nznames <- nznames[1:(length(nznames)-1)]
nznames
# Model building
# The three model types I’m going to test are:
# Decision trees with CART (rpart)
# Stochastic gradient boosting trees (gbm)
# Random forest decision trees (rf)
fitControl <- trainControl(method='cv', number = 3)

# Decision trees with CART (rpart)
mod_CART <- train(classe ~ ., data = training[, c('classe', nznames)], trControl=fitControl,method = "rpart")
predCART <- predict(mod_CART, newdata=testing)
cmCART <- confusionMatrix(predCART,factor(testing$classe))
fancyRpartPlot(mod_CART$finalModel)
plot(mod_CART)
save(mod_CART, file='./ModelFitCART.RData')

#Stochastic gradient boosting trees (gbm)
mod_gbm <- train(classe ~ ., data = training[, c('classe', nznames)], trControl=fitControl,method = "gbm")
predgbm <- predict(mod_gbm, newdata=testing)
cmgbm <- confusionMatrix(predgbm,factor(testing$classe))
plot(mod_gbm)

#Random forest decision trees (rf)

mod_rf <- train(classe ~ ., data = training[, c('classe', nznames)], trControl=fitControl,method = "rf")
predrf <- predict(mod_rf, newdata=testing)
cmrf <- confusionMatrix(predrf,factor(testing$classe))
plot(mod_rf)

#Model assesment
AccuracyResults <- data.frame(
  Model = c('CART', 'GBM', 'RF'),
  Accuracy = rbind(cmCART$overall[1], cmgbm$overall[1], cmrf$overall[1])
)
print(AccuracyResults)

# rf is the champion model with accuracy level 99%

#Prediction
#As a last step in the project, I’ll use the validation data sample 
#(‘pml-testing.csv’) to predict a classe for each of the 20 observations 
#based on the other information we know about these observations contained 
#in the validation sample.

predValidation <- predict(mod_rf, newdata=testing_csv)
ValidationPredictionResults <- data.frame(
  problem_id=testing_csv$problem_id,
  predicted=predValidation
)
print(ValidationPredictionResults)
##
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(combPred)

