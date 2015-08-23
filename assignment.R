library(caret)
library(e1071)
library(gbm)
library(adabag)
library(C50)


convert.magic <- function(x, y=NA) {
  for(i in 1:length(y)) { 
    if (y[i] == "integer") { 
      x[i] <- as.integer(x[[i]])
    }
    if (y[i] == "factor")
      x[i] <- as.factor(x[[i]])
    if (y[i] == "numeric")
      x[i] <- as.numeric(x[[i]])
  }
  return(x)
}

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}


data=read.csv(file="pml-training.csv")

head(data)

data$classe<-as.factor(data$classe)


set.seed(1234)
inTraining <- createDataPartition(data$classe, p = .6, list = FALSE)
training <- data[ inTraining,]
testing  <- data[-inTraining,]

clear_nzv=nearZeroVar(training, saveMetrics=TRUE)

remove=clear_nzv$nzv
remove[1:7]=TRUE

for(i in 1:length(remove)){
  if(sum(is.na(training[,i]))/length(training[,i])>0.9){
    remove[i]=TRUE
  }
}



training_clear=training[,!remove]
testing_clear=testing[,!remove]



Fit <- rpart(classe ~ ., data=training_clear, method="class")
# 
# 
# predictions_test <- predict(Fit, testing_clear, type = "class")
# confusionMatrix(predictions_test, testing_clear$classe)

classes=lapply(training_clear,class)

data_real_test=read.csv(file="pml-testing_1.csv")
data_real_test=convert.magic(data_real_test[,!remove], classes)

predictions_real <- predict(Fit, data_real_test, type = "class")


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 3,
  ## repeated ten times
  repeats = 3)


gbmFit1 <- train(classe ~ ., data = training_clear,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)




C5.0Fit1 <- train(classe ~ ., data = training_clear,
                 method = "C5.0",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)



gbm_predictions_test <- predict(gbmFit1, newdata = testing_clear)
confusionMatrix(gbm_predictions_test, testing_clear$classe)
C5.0predictions_test <- predict(C5.0Fit1, newdata = testing_clear)
confusionMatrix(C5.0predictions_test, testing_clear$classe)

predict(gbmFit1, newdata = data_real_test)
predict(C5.0Fit1, newdata = data_real_test)
