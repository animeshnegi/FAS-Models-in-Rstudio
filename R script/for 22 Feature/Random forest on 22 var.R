

library("randomForest")
iris.rf <- randomForest(activity ~ ., data=data, importance=TRUE,
                        proximity=TRUE)
iris.rf

predaa=predict(iris.rf,newdata=tdata)

com <- table(predaa,Actual=tdata$activity)
com

confusionMatrix(com)

MDSplot(iris.rf, tdata$activity)

################################# with train 

pred=predict(iris.rf,newdata=data)
comat <- table(pred,Actual=data$activity)
comat
confusionMatrix(com)


