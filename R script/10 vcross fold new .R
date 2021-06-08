


# ########################## 10 cross fold validation 

set.seed(400)

ctrl  <- trainControl(method="repeatedcv", number=10, repeats=3)


knnFit <- train(activity ~ ., data = data, method = "knn", trControl = ctrl, tuneLength = 20)

Knnfit <- knnFit$bestTune

pre_sv <- predict(knnFit,newdata = tdata)

cf <- confusionMatrix(pre_sv,tdata$activity)
cf





# 10 cross validation SVM

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

svm1 <- train(activity ~., data = data, method = "svmRadial", trControl = train_control,tuneLength = 10)


print(svm1)

pre_sv <- predict(svm1,newdata = tdata)

cf <- confusionMatrix(pre_sv,tdata$activity)

cf








#----------------------------------10 cross validation Randonforest model for the training datasets

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)


#    10 folds repeat 3 times

control <- trainControl(method='repeatedcv',number=10,repeats=3)


#Metric compare model is Accuracy

metric <- "Accuracy"

#Number randomely variable selected is mtry

mtry <- sqrt(ncol(data))

rf_default <- train(activity~., data = data, method='rf', metric = 'Accuracy', trControl = control, tunelength = 20)


print(rf_default)


print(svm1)

pre_sv <- predict(rf_default,newdata = tdata)

cf <- confusionMatrix(pre_sv,tdata$activity)

cf

































