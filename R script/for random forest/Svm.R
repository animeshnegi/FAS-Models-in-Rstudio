
train.data <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/train data.csv")

data <- select(train.data,activity, mintsC ,      MDEO.11,
               SpMax2_Bhi  , C3SP2,MACCSFP41,
               SpMin2_Bhm   ,SpMax4_Bhi,
               SpMax2_Bhe   ,MPC10,
               SpMax4_Bhv   ,SpMin2_Bhp,
               maxtsC       ,SpMax4_Bhp,
               StsC         ,MDEN.12,
               SpMin2_Bhv  ,StN,SpMax2_Bhv,
               SpMin2_Bhi  ,SpMax4_Bhe ,
               MPC9        , AATS6e,  
               SpMin2_Bhe   ,SpMax2_Bhp,
               mintN, maxtN, SHaaNH, mintN,nBondsT, MDEN.13,ntsC )

convert <- c(1)

data[,convert] <- data.frame(apply(data[convert], 1, as.factor))


str(data$activity)

library(e1071)
classifier = svm(formula = activity ~ ., 
                 data = data, 
                 type = 'C-classification', 
                 kernel = 'linear')
classifier
test.data <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/test data.csv")


tdata <- select(test.data,activity, mintsC ,      MDEO.11,
                SpMax2_Bhi  , C3SP2,MACCSFP41,
                SpMin2_Bhm   ,SpMax4_Bhi,
                SpMax2_Bhe   ,MPC10,
                SpMax4_Bhv   ,SpMin2_Bhp,
                maxtsC       ,SpMax4_Bhp,
                StsC         ,MDEN.12,
                SpMin2_Bhv  ,StN,SpMax2_Bhv,
                SpMin2_Bhi  ,SpMax4_Bhe ,
                MPC9        , AATS6e,  
                SpMin2_Bhe   ,SpMax2_Bhp,
                mintN, maxtN, SHaaNH, mintN,nBondsT, MDEN.13,ntsC)

convert <- c(1)

tdata[,convert] <- data.frame(apply(tdata[convert],1, as.factor))

pred=predict(classifier,newdata=tdata)
comat <- table(pred,Actual=tdata$activity)
comat




library(caret)

confusionMatrix(comat)

################## FOR TRAIN DATA SET 

pred=predict(classifier,newdata=data)
comat <- table(pred,Actual=data$activity)
comat
















