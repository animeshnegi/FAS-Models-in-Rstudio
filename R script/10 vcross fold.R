

data$activity <- as.factor(data$activity)


levels(data$activity) <- c('inhibitor','non-inhibitor')



##########################################


split <- trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = T, summaryFunction = twoClassSummary)

model <- train(activity~.,data = data,trControl=split, method = "gbm",preProcess= c("center","scale"), metric = "ROC")





#                    NEW 



data$activity <- as.factor(data$activity)

levels(data$activity) <- c("non-inhibitor","inhibitor")

str(data$activity)



tuned = tune.svm(activity~., data = data, gamma = 10^-2, cost = 10^2, tunecontrol=tune.control(cross=10))


summary(tuned)

svmfit = tuned$best.model
sum <- table(data[,c("activity")], predict(svmfit))

confusionMatrix(sum)


