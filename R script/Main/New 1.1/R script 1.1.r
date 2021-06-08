

# 1 --------------------------------------- READING  DATA FILE  or LOADING DATA 


data <- read.delim("C:/Users/animesh negi/Desktop/dis/data.tsv")


# 2 ------------------------------------------------------ DATA CLEANING 

#------------- REMOVING NA's   removed 32 mol


library(tidyverse)

na_data <- data %>% filter(is.na(Standard.Value)==TRUE)  #contain 32 observations
data <- data %>% filter(is.na(Standard.Value)==FALSE)


ki_data <- filter(data,Standard.Type=="Ki")          #  contain 20 observations
inhi <- filter(data,Standard.Type=="Inhibition")     #  contain 53 observations
data <- filter(data,Standard.Type=="IC50")

# Data remains to 3132 and Removed 73  observation have Ki value


ugml_data <- filter(data,Standard.Units=="ug.mL-1")    #  contain 45 onservations  
data <- filter(data,Standard.Units=="nM")

# Data remains to 3087 removed observations have ug.mL-1 and %  units 45 mol removed



data <- filter(data,is.na(Molecule.ChEMBL.ID)==FALSE)

write.csv(ugml_data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/ug ml data .csv")


# ----------Importing data set which are converted into nM --------

nm <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/nm.csv")


# joining daa set 
nm <- nm[-c(1)]

data <- rbind(data,nm)   # 45 observations are then added to the data set which are converted into nm 


#------------------- Saving refined data 
write.csv(data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/Refind dataset.csv")

#------------------- Saving validation data set  
valid <- rbind(inhi,ki_data)
valid <- rbind(valid,na_data)  # contain 105 observation  
write.csv(valid,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/Vadldation set.csv")


# 3 --------------------------------------------------- HISTOGRAM OF 3132 Ic50  VALUE

library(ggplot2)

final <-  select(data,Molecule.ChEMBL.ID,Standard.Value)

final <- arrange(final,desc(Molecule.ChEMBL.ID))
final <- final[-c(3076,109,1116,1463),]
final$row <- seq.int(nrow(final))

final %>% 
  arrange(final,desc(Molecule.ChEMBL.ID)) %>% 
  ggplot(aes(x=row,y = Standard.Value))+
  geom_histogram(fill = "blue",stat = "identity",width = 9)+
  labs(y = "IC50 Value",
       x = "Number of molecule",
       title = "Range of IC50 values of data" 
  )+
  scale_x_continuous(breaks = seq(1, 3083, 126), 
                     limits=c(1,3083))




# ----------------------------- DUPLICATES REMOVING 

data <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/Refind dataset.csv")
data <- data[-c(1)]
#---------------Removing retundancy from the data set-------

a <-  data

ni <- filter(data,data$Standard.Value>1000)
ac <- filter(data,data$Standard.Value<=1000)


# removing duplicates 
ac <-ac %>%  distinct(Molecule.ChEMBL.ID,.keep_all = TRUE)     # 914 obs after removing duplicates 845
ni <-ni %>%  distinct(Molecule.ChEMBL.ID,.keep_all = TRUE)     # 2218 obs after removing duplicates 1258

# finding duplicates in b/w both dat set 
new <- intersect(ac$Smiles,ni$Smiles)  # 27 duplicates amoung 2 tables 

new

for (i in 1:27) {
  for (j in 1:845  ) {
    if (new[i] == ac$Smiles[j]) {
      print(j)
    }
  } 
}



for (i in 1:27) {
  for (j in 1:1258  ) {
    if (new[i] == ni$Smiles[j]) {
      print(j)
    }
  } 
}

# removing those rows from the dataset

ac <- ac[-c( 1,
               11
              , 25
              , 36
              , 37
              , 40
              , 41
              , 46
              , 47
              , 56
              , 66
              , 106
              , 155
              , 166
              , 185
              , 187
              , 213
              , 233
              , 335
              , 336
              , 415
              , 424
              , 520
              , 563
              , 612
              , 613,845 ),]      # active remains to 818 from 845 removed 27 duplicates 

ni <- ni[-c(288
             , 830
             , 895
             , 911
             , 1067
             , 1113
             , 46
             , 344
             , 818
             , 841
             , 753
             , 588
             , 43
             , 783
             , 1028
             , 1024
             , 593
             , 1109
             , 1134
             , 1051
             , 1041
             , 369
             , 900
             , 1111
             , 769
             , 419
             , 26 ),]           #  Inactive remains to 1231 from 1258 removed 27 duplicates 


                                # data set contain 2049 observations till now 




                                      # --------- SKIP IT  ----------
# 5 -------- Data set is then divided into active and inactive according to the median of pIC50 value 


inactive <- data %>%  
  filter(pIC50<5.549)

# for active molecule cut of value is 160-20,000 nM

active <- data %>%  
  filter(pIC50>=5.549)

#Removed molecule have IC50 > 1,00,000 nM beacuse they are outside the typical range as given in literary articals 
inactive <- inactive %>%  
  filter(Standard.Value<=100000)


#------------------------ 1231 inactive and 818 active


inactive <-inactive %>%  distinct(Molecule.ChEMBL.ID, .keep_all = TRUE ) 

active <-active %>%  distinct(Molecule.ChEMBL.ID, .keep_all = TRUE ) 


#---------------- Checking NA's
active <- filter(active,is.na(Smiles)==FALSE)

inactive <- filter(inactive,is.na(Smiles)==FALSE)



# 4 ----------------------------------Assigning  Activity 

ac$activity <- "Inhibitor"

ni$activity <- "Non-Inhibitor"

# 6 ------------------------------------------Converting IC50 into PIC50 value  

pic <- function(x){
  y = 9-log10(x)
  return(y)
}

set.seed(123)
data <- rbind(ac,ni) 

pIC50 <- data$Standard.Value
pIC50 <- as.data.frame(pIC50)

b <- lapply(pIC50, pic)
b <- as.data.frame(b) 

data <- cbind(b,data)

summary(data$pIC50)

summary(data$Standard.Value)

str(data$activity)

# Converting { chr } into { Factor } for further analysis 

convert <- c(43)
di <- data

di[,convert] <- data.frame(apply(di[convert], 1, as.factor))


str(di$activity)


# 7 ------------------------------------- Saving the Processed dataset for discriptor calculation 

write.csv(di,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/final.csv")


##### ______________________________________Dividing data sets into traning, test and validation set 

set.seed(123)

ind <- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.20))

train.data <- data[ind==1,] # 1645 mol

test.data <- data[ind==2,]  # 404  mol




#____________________________________________ Saving  the Test, Training and Validation datasets 


write.csv(train.data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/train data.csv")
# inhibitor contain 652 and non inhibitor 993

write.csv(test.data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/test data.csv")
# inhibitor contain 166 and non inhibitor 238

a <- filter(test.data,activity=="Inhibitor")
b <- filter(test.data,activity=="Non-Inhibitor")



#__________________________________________________________IMPORTING DATA SET

one  <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/one.csv")
one <- one[-c(1)]


fin <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/fin.csv")
fin <- fin[-c(1)]

train.data <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/train data.csv")

DATASET <-  train.data
# pre processing

one$activity <- DATASET$activity
one$Smiles <- DATASET$Smiles
one <- cbind(one,fin)

#_________________________ Removing NAs from the discriptor dataset
1645-1534


dat <- na.omit(one)   # 111 molecules were removed  

a <- filter(dat,activity=="Inhibitor")            #  inhibitor = 639
b <- filter(dat,dat$activity=="Non-Inhibitor")    #  non inhibitor = 895

#________________________________ dividing dataset according 1D 2D 

one <- dat[1:1444]                  # saved 1D data 
fin <- dat[1447:1612]               # saved 2D data 
activity <- dat[1445:1446]



# ________________________________ Normalization function 

normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}


minu <- function(x) {
  return(min(x))
}

maxu <- function(x) {
  return(max(x))
}

min_nor <- lapply(one, minu)
max_nor <- lapply(one, maxu)

min_nor <- as.data.frame(min_nor)

max_nor <- as.data.frame(max_nor)



# ------------------------------- Normalization

normalized_data <- lapply(one, normalize)
normalized_data <- as.data.frame(normalized_data)

#________________________________ Joining data set with each other 

data <- cbind(normalized_data,fin)


#  ________________________________________REMOVING Variable have zero variance 
# Zero Variance fun 

zeroVar <- function(data, useNA = 'ifany') {
  out <- apply(data, 2, function(x) {length(table(x, useNA = useNA))})
  which(out==1)
}
1610-1355

data <- data[,-zeroVar(data)]  # 255 attributes were removed 
data <- cbind(activity,data)
data <- na.omit(data)


# removed  255 attributes witn zero variance data set remains to 1357 attributes

1610-1360




#____________________________________________ Saving data which is normalized and have no zero variable 

write.csv(data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/train normalidsed data.csv")





#____________________________________________ Saving data which is normalized and have no zero variable 

write.csv(data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/train normalidsed data.csv")



#----------------------  F  E  A  T  U  R  E  ----------  S  E L  E  C  T  I  O  N -------------------------------------



#------------------------------------------- FEATURE SELECTION USING BORUTA AND RANDOM FOREST


train.data <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/train normalidsed data.csv")
train.data <- train.data[-c(1)]
dis <- train.data
str(train.data$activity)
#_________________________________ MAKING FACTORS


convert <- c(1)

dis[,convert] <- data.frame(apply(dis[convert], 1, as.factor))

str(dis$activity)

dat <- na.omit(dis)
smiles <- dat[c(2)]
dat <- dat[-c(2)]


#____________________________________________ Applying Boruta Algorithm to train data set  

library(Boruta)

set.seed(123)
bar <- Boruta(data$activity~.,data = dat,doTrace = 2)

bar
Boruta::getConfirmedFormula(bar)

feature <- getSelectedAttributes(bar, withTentative = FALSE)

boruta_data <- train.data[,c("activity",feature)]


feature

# _----------------------- -  -   Preparing test data set --------------------------



test.data <- read.csv("C:/Users/animesh negi/Desktop/dis/one.csv")        # Importing test dataset 
test.data <- test.data[-c(1)]
tdata <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/test data.csv")


tdata <- tdata[c(44)]
test.data <- cbind(tdata,test.data)




test.data <- na.omit(test.data)   # remains to 371 after aapliying na.omit removed 33 observations 
dat <- test.data


a <- filter(dat,activity=="Inhibitor")              # 163 as inhibitor
b <- filter(dat,dat$activity=="Non-Inhibitor")      #  208 as non inhibitor

#________________________________ dividing dataset according 1D 2D 

one <- dat[2:1445]                  # saved 1D data 
fin <- dat[1446:1611]               # saved 2D data 
activity <- dat[1]



# ________________________________ Normalization function 

norm_test <- matrix(ncol = 1444, nrow = 371)
norm_test <- as.data.frame(norm_test)



for (i in 1:1444) {
  for (j in 1:371) {
    value <- ((one[j,i] - min_nor[i])/(max_nor[i]-min_nor[i]))
    norm_test[j,i] <-  value
    
  }
}


#________________________________ Joining data set with each other 

normalized_data <- norm_test

colnames(normalized_data) <-  colnames(one)

data <- cbind(normalized_data,fin)


test.data <- cbind(data,activity)

write.csv(test.data,"C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/test normalidsed data.csv")




#------------------------------------------------ BUILDING MODELS WITH ATTRIBUTES SELECTED BY THE BORUTA ALGORITHM

#  ________________ 1. SVM model  (Some feature may be different, but most of them are common )




train.data <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/train normalidsed data.csv")      # Importing training dataset 



# Making a new Training dataset of that attributes which are selected  by the Boruta algorithm


dat <- train.data[,c("activity",feature)]


#   converting activity into factors 
convert <- c(1)

dat[,convert] <- data.frame(apply(dat[convert], 1, as.factor))

str(dat$activity)


# SVM Model is trained using Training dataset and radial function 
library(e1071)      # SVM library 

set.seed(123)
classifier = svm(formula = activity ~ ., 
                 data = dat, 
                 type = 'C-classification', 
                 kernel = 'radial')

classifier



# Making a new Test dataset of that attributes which are selected  by the Boruta algorithm


test.data <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/test normalidsed data.csv")      # Importing test dataset 

tdata <- test.data[,c("activity",feature)]


#   converting activity into factors 
convert <- c(1)
tdata[,convert] <- data.frame(apply(tdata[convert],1, as.factor))
str(tdata$activity)


# Prediction by SVM model for the training datasets
pred=predict(classifier,newdata=dat)
comat1 <- table(pred,Actual=dat$activity)
comat1        # confusion matrix 

# Prediction by SVM model for the test datasets
pred=predict(classifier,newdata=tdata)
comat2 <- table(pred,Actual=tdata$activity)
comat2        # confusion matrix

# Prediction by SVM model for the validation datasets
pred3=predict(randomForest,newdata=a)
comat3 <- table(pred,Actual=tdata$activity)
comat3        # confusion matrix
view(pred)
a <- a[,feature]

validation_set["Inhibition",] <- "Inhibitor"


# building a confusion matrix using caret for test and training data set 

# confusion matrix with training data set  
library(caret)

confusionMatrix(comat1)
plot(classifier,data = data)

# confusion matrix with train data set  


confusionMatrix(comat2)
plot(classifier,data = data)


# 10 cross validation SVM

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

svm1 <- train(activity ~., data = dat, method = "svmRadial", trControl = train_control,tuneLength = 10)


print(svm1)

pre_sv <- predict(svm1,newdata = tdata)

cf <- confusionMatrix(pre_sv,tdata$activity)

cf




# ------------------------------------------------------ 2. Random Forest model


library("randomForest")       # randomforest Library

# Training of Randoomforest model using training dataset
randomForest<- randomForest(dat$activity ~ ., data=dat[2:130], importance=TRUE,ntree = 70,
                            proximity=TRUE)

randomForest




#--------------------------------- Prediction by Randomforest model for the training datasets

pred_train = predict(randomForest,newdata=dat)

com1 <- table(pred_train,Actual=dat$activity)
com1

confusionMatrix(com1)

MDSplot(randomForest, tdata$activity)


#----------------------------------Prediction by Randonforest model for the test datasets

pred_test = predict(randomForest,newdata=tdata)

com2 <- table(pred_test,Actual=tdata$activity)
com2

confusionMatrix(com2)


MDSplot(randomForest, tdata$activity)



#----------------------------------10 cross validation Randonforest model for the training datasets

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)


#    10 folds repeat 3 times

control <- trainControl(method='repeatedcv',number=10,repeats=3)


#Metric compare model is Accuracy

metric <- "Accuracy"

#Number randomely variable selected is mtry

mtry <- sqrt(ncol(dat))

rf_default <- train(activity~., data = dat, method='rf', metric = 'Accuracy', trControl = control, tunelength = 20)


print(rf_default)


print(svm1)

pre_sv <- predict(rf_default,newdata = tdata)

cf <- confusionMatrix(pre_sv,tdata$activity)

cf






# ------------------------------------------------------ 3. knn model


library(class)

# ########################## FOR Train


cl = factor(dat$activity)
label <- factor(tdata$activity)


pred <- knn(train = dat[c(2:130)],dat[c(2:130)] , cl , k=3)
pred

knn2 <- table(pred,Actual= cl)

confusionMatrix(knn2)


############################

cl = factor(dat$activity)
label <- factor(tdata$activity)


pred <- knn(train = dat[c(2:130)], test = tdata[c(2:130)],cl , k=3)
pred

knn1 <- table(pred,Actual= label)

confusionMatrix(knn1)



# ########################## 10 cross fold validation 

set.seed(400)

ctrl  <- trainControl(method="repeatedcv", number=10, repeats=3)


knnFit <- train(activity ~ ., data = dat, method = "knn", trControl = ctrl, tuneLength = 20)

Knnfit <- knnFit$bestTune

pre_sv <- predict(knnFit,newdata = tdata)

cf <- confusionMatrix(pre_sv,tdata$activity)
cf




#-------------- R E G R E S S I O N ------ M O D E L IS --  I N --  N E X T --  S C R I PT -------------------------# 





# _----------------------- -  -   Preparing Validation data set --------------------------



val <- read.csv("C:/Users/animesh negi/Desktop/dis/vali/val.csv")        # Importing test dataset contain 105 obs
val <- val[-c(1)]
validation_set <- read.csv("C:/Users/animesh negi/Desktop/dis/model with 1000 nM cut off/datasets/Vadldation set.csv")


val <- na.omit(val)   # remains to 103 after aapliying na.omit removed 2 observations 


#________________________________ dividing dataset according 1D 2D 

vone <- val[1:1444]                  # saved 1D data 
vfin <- val[1445:1610]               # saved 2D data 

a <- na.omit(norm_test)


# ________________________________ Normalization function 

norm_val <- matrix(ncol = 1444, nrow = 101)
norm_val <- as.data.frame(norm_val)



for (i in 1:1444) {
  for (j in 1:101) {
    value <- ((vone[j,i] - min_nor[i])/(max_nor[i]-min_nor[i]))
    norm_val[j,i] <-  value
  }
}


a <- as_tibble(norm_val)
a<- na.omit(a)
#________________________________ Joining data set with each other 


#  ________________________________________REMOVING Variable have zero variance 
# Zero Variance fun 

zeroVar <- function(data, useNA = 'ifany') {
  out <- apply(data, 2, function(x) {length(table(x, useNA = useNA))})
  which(out==1)
}

colnames(a) <-  colnames(vone)

a <- norm_test[,-zeroVar(norm_val)]  # 255 attributes were removed 

a <- cbind(a,vfin)


a <-a[,feature]


























































         
         
         
         
         
         



