

library("randomForest")
iris.rf <- tune.randomForest(activity ~ ., data=data, importance=TRUE,
                        proximity=TRUE,tunecontrol=tune.control(cross=10),cost = 10^2)

iris.rf <- iris.rf$best.model

predaa=predict(iris.rf,newdata=tdata)

sum <- table(data[,c("activity")], predict(iris.rf))

confusionMatrix(sum)




################## FOR knn WITH 10 CROSSS FOLD Validation 


library("class")
cl = factor(data$activity)
label <- factor(tdata$activity)


pred <- tune.knn(x = data[c(2:24)], y = tdata[c(2:24)],
                 cl , k=3, tunecontrol= tune.control(cross = 10),cost = 10^2 )

pred


# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)


# Train the model
model <- svm(activity ~ ., 
    data = data, 
    type = 'C-classification', 
    kernel = 'linear', trControl = train.control)

model <- train(activity ~., data = data, method = "knn",
               trControl = train.control)

# Summarize the results
print(model)

model <- model$finalModel
model
