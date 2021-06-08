library("randomForest")
iris.rf <- randomForest(activity ~ ., data=data, importance=TRUE,
                        proximity=TRUE)
iris.rf

predaa=predict(iris.rf,newdata=tdata)

com <- table(predaa,Actual=tdata$activity)
com

confusionMatrix(com)

MDSplot(iris.rf, tdata$activity)

############################ TRAIN




predaa=predict(iris.rf,newdata=data)

com <- table(predaa,Actual=data$activity)
com

confusionMatrix(com)

MDSplot(iris.rf, tdata$activity)









