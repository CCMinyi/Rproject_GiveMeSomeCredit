set.seed(5566)
library(rpart)
library(e1071)
library(caret)
#read parameters
#Rscript my_model.R --fold 10 --train Data/training.csv --test Data/test.csv --report performance.csv --predict predict.csv
args = commandArgs(trailingOnly = TRUE)
if (length(args)==0){
  stop("Rscript my_model.R --fold n --train Data/train.csv --test Data/test.csv --report performance.csv --predict predict.csv",call. = FALSE)
}

#parse parameters
i <- 1
trainflag <- FALSE
testflag <- FALSE
while(i<length(args)){
  if (args[i]=="--fold"){
    fold_k <- as.numeric(args[i+1])
    i <- i+1
  }else if (args[i]=="--train"){
    df_train <- read.table(args[i+1],sep = ',',header = T)
    i <- i+1
    trainflag <- TRUE
  }else if (args[i]=="--test"){
    df_test<- read.table(args[i+1],sep = ',',header = T)
    i <- i+1
    testflag <- TRUE
  }else if (args[i]=="--report"){
    performance <- args[i+1]
    i <- i+1
  }else if (args[i]=="--predict"){
    predict <- args[i+1]
    i <- i+1
  }else{
    stop(paste("Unknown flag",args[i]),call. = FALSE)
  }
  i <- i+1
}
if(trainflag == FALSE){
  stop(paste("missing --train"))
}
if(testflag == FALSE){
  stop(paste("missing --test"))
}

#split_data
#Randomly shuffle the data
df_train<-df_train[sample(nrow(df_train)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(df_train)),breaks=fold_k,labels=FALSE)
#set vector
setting_vec_1 <- c()
setting_vec_2 <- c()
setting_vec_3 <- c()
training_vec_1 <- c()
training_vec_2 <- c()
training_vec_3 <- c()
vali_vec_1 <- c()
vali_vec_2 <- c()
vali_vec_3 <- c()
testing_vec_1 <- c()
testing_vec_2 <- c()
testing_vec_3 <- c()
y <- 'SeriousDlqin2yrs'
x <- c('RevolvingUtilizationOfUnsecuredLines','age',
       'NumberOfTime30.59DaysPastDueNotWorse','DebtRatio',
       'MonthlyIncome','NumberOfOpenCreditLinesAndLoans',
       'NumberOfTimes90DaysLate','NumberRealEstateLoansOrLines','MonthlyIncome',
       'NumberOfTime60.89DaysPastDueNotWorse','NumberOfDependents')
formula <- paste(y, paste(x, collapse="+"), sep="~")
for(i in 1:fold_k){
  if(i!=fold_k){
    testIndexes <- which(folds==i)
    testData <- df_train[testIndexes, ]
    validIndexes <- which(folds==i+1)
    valiData <- df_train[validIndexes,]
    trainData <- df_train[-c(testIndexes,validIndexes), ]
    #model_naive bayes
    model_1 <- naiveBayes(as.factor(SeriousDlqin2yrs)~RevolvingUtilizationOfUnsecuredLines+age
                          +NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome
                          +NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate
                          +NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse
                          +NumberOfDependents,data = trainData)
    # make confusion matrix tabel <- training
    resultframe_training <- table(truth=trainData$SeriousDlqin2yrs,
                                  pred=predict(model_1,newdata=trainData))
    err_rate_training <- (resultframe_training[1,1]+resultframe_training[2,2])/dim(trainData)[1]
    training_vec_1 <- c(training_vec_1,err_rate_training)
    #model_rpart
    model_2 <- rpart(formula,data=trainData,method='class')
    
    # make confusion matrix tabel <- training
    resultframe_training <- table(truth=trainData$SeriousDlqin2yrs,pred=predict(model_2,trainData,type = 'class'))
    err_rate_training <- (resultframe_training[1,1]+resultframe_training[2,2])/dim(trainData)[1]
    training_vec_2 <- c(training_vec_2,err_rate_training)
    #model_lm
    model_3 <- lm(formula,data=trainData)
    # make confusion matrix tabel <- training
    probabilities <- predict(model_3,trainData, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
    resultframe_training <- table(truth=trainData$SeriousDlqin2yrs,
                                  pred=predicted.classes)
    err_rate_training <- (resultframe_training[1,1]+resultframe_training[2,2])/dim(trainData)[1]
    training_vec_3 <- c(training_vec_3,err_rate_training)
    
    #testing validation model_1
    resultingframe_validation <- table(truth=valiData$SeriousDlqin2yrs,
                                       pred=predict(model_1,newdata=valiData))
    err_rate_validation <- (resultingframe_validation[1,1]+resultingframe_validation[2,2])/dim(valiData)[1]
    vali_vec_1 <- c(vali_vec_1,err_rate_validation)
    #testing validation model_2
    resultingframe_validation <- table(truth=valiData$SeriousDlqin2yrs,
                                       pred=predict(model_2,valiData,type = 'class'))
    err_rate_validation <- (resultingframe_validation[1,1]+resultingframe_validation[2,2])/dim(valiData)[1]
    vali_vec_2 <- c(vali_vec_2,err_rate_validation)
    #testing validation model_3
    probabilities <- predict(model_3,valiData, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
    resultingframe_validation <- table(truth=valiData$SeriousDlqin2yrs,
                                       pred=predicted.classes)
    err_rate_validation <- (resultingframe_validation[1,1]+resultingframe_validation[2,2])/dim(valiData)[1]
    vali_vec_3 <- c(vali_vec_3,err_rate_validation)
    #testinging model
    #testing model 1
    resultframe_test <- table(truth=testData$SeriousDlqin2yrs,
                              pred=predict(model_1,newdata=testData))
    err_rate_testing <- round((resultframe_test[1,1]+resultframe_test[2,2])/dim(testData)[1],digits = 2)
    testing_vec_1 <- c(testing_vec_1,err_rate_testing)
    #testing model 2
    resultframe_test <- table(truth=testData$SeriousDlqin2yrs,
                              pred=predict(model_2,testData,type = 'class'))
    err_rate_testing <- round((resultframe_test[1,1]+resultframe_test[2,2])/dim(testData)[1],digits = 2)
    testing_vec_2 <- c(testing_vec_2,err_rate_testing)
    #testing model 3
    probabilities <- predict(model_3,testData, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
    resultframe_test <- table(truth=testData$SeriousDlqin2yrs,
                              pred=predicted.classes)
    err_rate_testing <- round((resultframe_test[1,1]+resultframe_test[2,2])/dim(testData)[1],digits = 2)
    testing_vec_3 <- c(testing_vec_3,err_rate_testing)
    
    setting_vec_1 <- c(setting_vec_1,paste("fold",i))
    setting_vec_2 <- c(setting_vec_2,paste("fold",i))
    setting_vec_3 <- c(setting_vec_3,paste("fold",i))
    i <- i+1
  }else if(i == fold_k){
    testIndexes <- which(folds==i)
    testData <- df_train[testIndexes, ]
    validIndexes <- which(folds==1)
    valiData <- df_train[validIndexes,]
    trainData <- df_train[-c(testIndexes,validIndexes), ]
    #model_naivebayes
    model_1 <- naiveBayes(as.factor(SeriousDlqin2yrs)~RevolvingUtilizationOfUnsecuredLines+age+NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,data=trainData)
    # make confusion matrix tabel <- training
    resultframe_training <- table(truth=trainData$SeriousDlqin2yrs,
                                  pred=predict(model_1,newdata=trainData))
    err_rate_training <- (resultframe_training[1,1]+resultframe_training[2,2])/dim(trainData)[1]
    training_vec_1 <- c(training_vec_1,err_rate_training)
    #model_rpart
    model_2 <- rpart(formula,
                     data=trainData,method='class')
    # make confusion matrix tabel <- training
    resultframe_training <- table(truth=trainData$SeriousDlqin2yrs,
                                  pred=predict(model_2,trainData,type = 'class'))
    err_rate_training <- (resultframe_training[1,1]+resultframe_training[2,2])/dim(trainData)[1]
    training_vec_2 <- c(training_vec_2,err_rate_training)
    #model_lm
    model_3 <- lm(formula,data=trainData)
    # make confusion matrix tabel <- training
    probabilities <- predict(model_3,trainData, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
    resultframe_training <- table(truth=trainData$SeriousDlqin2yrs,
                                  pred=predicted.classes)
    err_rate_training <- (resultframe_training[1,1]+resultframe_training[2,2])/dim(trainData)[1]
    training_vec_3 <- c(training_vec_3,err_rate_training)
    
    #testing validation model_1
    resultingframe_validation <- table(truth=valiData$SeriousDlqin2yrs,
                                       pred=predict(model_1,newdata=valiData))
    err_rate_validation <- (resultingframe_validation[1,1]+resultingframe_validation[2,2])/dim(valiData)[1]
    vali_vec_1 <- c(vali_vec_1,err_rate_validation)
    #testing validation model_2
    resultingframe_validation <- table(truth=valiData$SeriousDlqin2yrs,
                                       pred=predict(model_2,newdata=valiData,type = 'class'))
    err_rate_validation <- (resultingframe_validation[1,1]+resultingframe_validation[2,2])/dim(valiData)[1]
    vali_vec_2 <- c(vali_vec_2,err_rate_validation)
    #testing validation model_3
    probabilities <- predict(model_3,valiData, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
    resultingframe_validation <- table(truth=valiData$SeriousDlqin2yrs,
                                       pred=predicted.classes)
    err_rate_validation <- (resultingframe_validation[1,1]+resultingframe_validation[2,2])/dim(valiData)[1]
    vali_vec_3 <- c(vali_vec_3,err_rate_validation)
    
    #testinging model
    #testing model 1
    resultframe_test <- table(truth=testData$SeriousDlqin2yrs,
                              pred=predict(model_1,newdata=testData))
    err_rate_testing <- round((resultframe_test[1,1]+resultframe_test[2,2])/dim(testData)[1],digits = 2)
    testing_vec_1 <- c(testing_vec_1,err_rate_testing)
    #testing model 2
    resultframe_test <- table(truth=testData$SeriousDlqin2yrs,
                              pred=predict(model_2,newdata=testData,type = 'class'))
    err_rate_testing <- round((resultframe_test[1,1]+resultframe_test[2,2])/dim(testData)[1],digits = 2)
    testing_vec_2 <- c(testing_vec_2,err_rate_testing)
    #testing model 3
    probabilities <- predict(model_3,testData, type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
    resultframe_test <- table(truth=testData$SeriousDlqin2yrs,
                              pred=predicted.classes)
    err_rate_testing <- round((resultframe_test[1,1]+resultframe_test[2,2])/dim(testData)[1],digits = 2)
    testing_vec_3 <- c(testing_vec_3,err_rate_testing)
    
    setting_vec_1 <- c(setting_vec_1,paste("fold",i))
    setting_vec_2 <- c(setting_vec_2,paste("fold",i))
    setting_vec_3 <- c(setting_vec_3,paste("fold",i))
  }
}  
out_data_1 <- data.frame(set=setting_vec_1,training=round(training_vec_1,digits = 2),validation=round(vali_vec_1,digits = 2),test=round(testing_vec_1,digits = 2),stringsAsFactors = F)
out_data_2 <- data.frame(set=setting_vec_2,training=round(training_vec_2,digits = 2),validation=round(vali_vec_2,digits = 2),test=round(testing_vec_2,digits = 2),stringsAsFactors = F)
out_data_3 <- data.frame(set=setting_vec_3,training=round(training_vec_3,digits = 2),validation=round(vali_vec_3,digits = 2),test=round(testing_vec_3,digits = 2),stringsAsFactors = F)

ave_data_1 <- data.frame(set="ave.",training=round(mean(out_data_1$training),digits = 2),validation=round(mean(out_data_1$validation),digits = 2),test=round(mean(out_data_1$test),digits = 2),stringsAsFactors = F)
ave_data_2 <- data.frame(set="ave.",training=round(mean(out_data_2$training),digits = 2),validation=round(mean(out_data_2$validation),digits = 2),test=round(mean(out_data_2$test),digits = 2),stringsAsFactors = F)
ave_data_3 <- data.frame(set="ave.",training=round(mean(out_data_3$training),digits = 2),validation=round(mean(out_data_3$validation),digits = 2),test=round(mean(out_data_3$test),digits = 2),stringsAsFactors = F)
pick_model <- c(mean(out_data_1$validation),mean(out_data_2$validation),mean(out_data_3$validation))
pick_model <- which.max(pick_model)

if (pick_model==1){
  combine_data <- rbind(out_data_1,ave_data_1)
  write.table(combine_data, file=performance, row.names = F, quote = F,sep=",")
  #model_1
  model_1 <- naiveBayes(as.factor(SeriousDlqin2yrs)~RevolvingUtilizationOfUnsecuredLines+age+NumberOfTime30.59DaysPastDueNotWorse+DebtRatio+MonthlyIncome+NumberOfOpenCreditLinesAndLoans+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,data=df_train)
  pred <- predict(model_1,newdata=df_test,type='raw')
  pred <- data.frame(pred[,2])
  preproc <- preProcess(pred,method = c('range'))
  pred_norm <- predict(preproc,pred)
  names(pred_norm) <- "Probability"
  
  resultframe_predict <- data.frame(Id=df_test$X,
                                    Probability=pred_norm)
}else if(pick_model==2){
  combine_data <- rbind(out_data_2,ave_data_2)
  write.table(combine_data, file=performance, row.names = F, quote = F,sep=",")
  #model_rpart
  model_2 <- rpart(formula,data=df_train,method = 'class')
  # make confusion matrix tabel <- training
  pred <- predict(model_2,df_test,type = 'prob')
  resultframe_predict <- data.frame(Id=df_test$X,
                                    Probability=pred[,2])
}else if (pick_model==3){
  combine_data <- rbind(out_data_3,ave_data_3)
  write.table(combine_data, file=performance, row.names = F, quote = F,sep=",")
  #model_lm
  model_3 <- lm(formula,data=df_train)
  # make confusion matrix tabel <- training
  probabilities <- data.frame(predict(model_3,df_test, type = "response"))
  preproc <- preProcess(probabilities,method = c('range'))
  pred_norm <- predict(preproc,probabilities)
  names(pred_norm) <- "Probability"
  resultframe_predict <- data.frame(Id=df_test$X,
                                    Probability=pred_norm)
}
print(pick_model)
write.table(resultframe_predict, file=predict, row.names = F, quote = F,sep=",")
