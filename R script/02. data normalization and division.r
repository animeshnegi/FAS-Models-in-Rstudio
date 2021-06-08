
#_____________________________IMPORTING DATA SET

one <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/one.csv")
one <- one[-c(1)]

DATASET <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/DATASET.csv")


fin <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/fin.csv")
fin <- fin[-c(1)]


# pre processing

one$activity <- DATASET$activity

one <- cbind(one,fin)
data <- na.omit(one)

# 138 molecules were removed  

#_______________________________________ Zero Variance fun 

zeroVar <- function(data, useNA = 'ifany') {
  out <- apply(data, 2, function(x) {length(table(x, useNA = useNA))})
  which(out==1)
}



#  ____________________________________ REMOVING Variable have zero variance 

data <- data[,-zeroVar(data, useNA = 'no')]
data <- data %>% select(-activity)


# removed  247 attributes have zero variance


# one is from 2:1445 and 1446 is activity and after 1446 is fingerprint






normalize <- function(x){
  num <- x - min(x)
  denom <- min(x) - max(x)
  return(num/denom)
}

#---------------------  converting into factor

a <- select(one,activity)
a <- as.data.frame(a)

convert <- c(1)

a[,1] <- data.frame(apply(a[1], 1, as.factor))
str(a)


fin <- data[c(1215:1363)]
data <- data[c(1:1214)]
149-166
1214-1444
# removed 17 fingerprint and 230 dis




# ------------------------------- Normalization


data <- lapply(data, normalize)
data <- as.data.frame(data)






data <- cbind(a,data)

data <- cbind(data,fin)

# attributes remians to 1382 

dis <- na.omit(data)

data <- as_tibble(data)

str(dis$activity)

#____________________________________________ Saving data  


write.csv(dis,"C:/Users/animesh negi/Desktop/dis/fresh/full normalized data.csv")






##### ______________________________________Dividing data sets 

set.seed(1234)

ind <- sample(3,nrow(data),replace = TRUE,prob = c(0.7,0.20,0.10))

train.data <- data[ind==1,] # 1348 mol

test.data <- data[ind==2,]  # 341 mol

vali.data <- data[ind==3,]  # 196 mol



#____________________________________________ Saving data 


write.csv(train.data,"C:/Users/animesh negi/Desktop/dis/fresh/train data.csv")
write.csv(test.data,"C:/Users/animesh negi/Desktop/dis/fresh/test data.csv")
write.csv(vali.data,"C:/Users/animesh negi/Desktop/dis/fresh/validatin data.csv")











#---------------------  converting into factor

a <- select(train.data,Name,activity)
a <- as.data.frame(a)

convert <- c(2)

a[,2] <- data.frame(apply(a[2], 1, as.factor))
str(a)

train.data <- train.data[-c(1,2)]
train.data <- cbind(a,train.data)

