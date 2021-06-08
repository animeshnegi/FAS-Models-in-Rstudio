
library(neuralnet)

a <- as.data.frame(data$activity)
convert <- c(1)

a[,convert] <- data.frame(apply(a[convert], 1, as.factor))


da <- data

str(da$activity)

nn=neuralnet(da$activity~ .,data = da[2:103], hidden=3,act.fct = "logistic",
             linear.output = FALSE)


nn


plot(nn)

pred=predict(nn,newdata=data)
comat <- table(pred,Actual=a$`data$activity`)
comat





library(neuralnet)

a <- as.data.frame(data$activity)
convert <- c(1)

a[,convert] <- data.frame(apply(a[convert], 1, as.factor))


da <- data

str(da$activity)

nn=neuralnet(activity~ .,data = data, hidden=3,act.fct = "logistic",stepmax = 1e7,
             linear.output = FALSE)


nn


plot(nn)

pred=predict(nn,tdata)
pred
comat <- table(pred)
comat


library(neuralnet)

# Binary classification
nn <- neuralnet(Species == "setosa" ~ Petal.Length + Petal.Width, iris, linear.output = FALSE)
## Not run: print(nn)
## Not run: plot(nn)

# Multiclass classification
nn <- neuralnet(Species ~ Petal.Length + Petal.Width, iris, linear.output = FALSE)
## Not run: print(nn)
## Not run: plot(nn)

# Custom activation function
softplus <- function(x) log(1 + exp(x))
nn <- neuralnet((Species == "setosa") ~ Petal.Length + Petal.Width, iris, 
                linear.output = FALSE, hidden = c(3, 2), act.fct = softplus)
## Not run: print(nn)
## Not run: plot(nn)























































