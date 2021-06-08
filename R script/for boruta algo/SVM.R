



train.data <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/train data.csv")

data <- select(train.data,activity,ATS5m,  AATS4m,  AATS6m,  AATS1v,    AATS1p,  mintsC,       MDEO.11,
               AATS6p,  ATSC4m  ,ATSC8v  ,ATSC6p  ,AATSC1m , AATSC8v , AATSC0p , SpMax2_Bhi ,  
               AATSC6i  ,MATS2c ,MATS8v,  MATS6i,  GATS6c,  GATS6v , GATS4e , SpMin2_Bhm  ,
               GATS1p,  GATS7p  ,GATS4s , GATS6s , VE1_Dzv ,BCUTp.1l , SpMax2_Bhe  , MPC10,
               BCUTp.1h  ,SpMax1_Bhm , SpMax4_Bhm , SpMin1_Bhm  ,SpMax4_Bhv ,  SpMin2_Bhp,
               SpMax2_Bhv  ,  SpMin1_Bhv, maxtsC  ,     SpMax4_Bhp,
               SpMin1_Bhe ,   SpMin6_Bhe ,SpMin8_Bhe, StsC        , MDEN.12,
               SpMin1_Bhp  ,  SpMin4_Bhp , SpMin2_Bhv   ,StN,
               SpMin1_Bhi ,  SpMin8_Bhi , SpMin2_Bhi , SpMax4_Bhe ,C3SP2,
               SpMin2_Bhs , SpMin7_Bhs , C1SP2 , C2SP2  , SCH.4,
               SCH.6 , VCH.4 , VCH.7 ,SC.5 , VC.5 ,VPC.6 , SP.7 , ASP.7 ,
               AVP.4,  AVP.5 , AVP.7  ,SpMAD_Dt, VR2_Dt, MPC9  ,       AATS6e  ,
               SdO, minHBa  ,   maxHBa  ,  maxtN , SpMin2_Bhe ,  SpMax2_Bhp,
               maxaaN, maxsF, hmax , gmax , MAXDP,  MAXDP2 , fragC ,
               MDEN.23 , MLFER_A , MPC7 , MPC8 ,mintN, SpMax4_Bhi,
               TPC , piPC6,  piPC7 , piPC9  ,piPC10 ,TpiPC, JGI2,  MWC8)

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


tdata <- select(test.data,activity,ATS5m,  AATS4m,  AATS6m,  AATS1v,    AATS1p,  mintsC,       MDEO.11,
                AATS6p,  ATSC4m  ,ATSC8v  ,ATSC6p  ,AATSC1m , AATSC8v , AATSC0p , SpMax2_Bhi ,  
                AATSC6i  ,MATS2c ,MATS8v,  MATS6i,  GATS6c,  GATS6v , GATS4e , SpMin2_Bhm  ,
                GATS1p,  GATS7p  ,GATS4s , GATS6s , VE1_Dzv ,BCUTp.1l , SpMax2_Bhe  , MPC10,
                BCUTp.1h  ,SpMax1_Bhm , SpMax4_Bhm , SpMin1_Bhm  ,SpMax4_Bhv ,  SpMin2_Bhp,
                SpMax2_Bhv  ,  SpMin1_Bhv, maxtsC  ,     SpMax4_Bhp,
                SpMin1_Bhe ,   SpMin6_Bhe ,SpMin8_Bhe, StsC        , MDEN.12,
                SpMin1_Bhp  ,  SpMin4_Bhp , SpMin2_Bhv   ,StN,
                SpMin1_Bhi ,  SpMin8_Bhi , SpMin2_Bhi , SpMax4_Bhe ,C3SP2,
                SpMin2_Bhs , SpMin7_Bhs , C1SP2 , C2SP2  , SCH.4,
                SCH.6 , VCH.4 , VCH.7 ,SC.5 , VC.5 ,VPC.6 , SP.7 , ASP.7 ,
                AVP.4,  AVP.5 , AVP.7  ,SpMAD_Dt, VR2_Dt, MPC9  ,       AATS6e  ,
                SdO, minHBa  ,   maxHBa  ,  maxtN , SpMin2_Bhe ,  SpMax2_Bhp,
                maxaaN, maxsF, hmax , gmax , MAXDP,  MAXDP2 , fragC ,
                MDEN.23 , MLFER_A , MPC7 , MPC8 ,mintN, SpMax4_Bhi,
                TPC , piPC6,  piPC7 , piPC9  ,piPC10 ,TpiPC, JGI2,  MWC8)
convert <- c(1)

tdata[,1] <- data.frame(apply(tdata[1],1, as.factor))



pred=predict(classifier,newdata=tdata)
comat <- table(pred,Actual=tdata$activity)
comat




library(caret)

confusionMatrix(comat)



######################### FOR TRAIN 


pred=predict(classifier,newdata=data)
comat <- table(pred,Actual=data$activity)
comat




















































































