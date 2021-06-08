library(class)

cl = factor(data$activity)
label <- factor(tdata$activity)


pred <- knn(train = data[c(2:24)], test = tdata[c(2:24)], cl , k=3)
pred

knn <- table(pred,Actual= label)

confusionMatrix(knn)


# ########################## FOR TRAIN


cl = factor(data$activity)
label <- factor(tdata$activity)


pred <- knn(train = data[c(2:24)],data[c(2:24)] , cl , k=3)
pred

knn <- table(pred,Actual= cl)

confusionMatrix(knn)
