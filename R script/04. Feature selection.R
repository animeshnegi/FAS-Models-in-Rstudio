
train.data <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/train data.csv")

str(train.data$activity)
#_________________________________ MAKING FACTORS

dis <- train.data

convert <- c(1)

dis[,convert] <- data.frame(apply(dis[convert], 1, as.factor))

str(dis$activity)




#____________________________________________ Boruta Algorithum 

library(Boruta)


bar <- Boruta(activity~.,data = dis)

bar

Boruta::getConfirmedFormula(bar)





#____________________________________________ Random Forest Algorithum 

library(randomForest)

im <- randomForest(activity~ .,data = dis)


importance(im)

rank(importance(im))

varImpPlot(im)

t.test()


#_________________________________selected features 

activity ~ ATS5m + AATS4m + AATS6m + AATS1v +  + AATS1p + 
  AATS6p + ATSC4m + ATSC8v + ATSC6p + AATSC1m + AATSC8v + AATSC0p + 
  AATSC6i + MATS2c + MATS8v + MATS6i + GATS6c + GATS6v + GATS4e + 
  GATS1p + GATS7p + GATS4s + GATS6s + VE1_Dzv + BCUTp.1l + 
  BCUTp.1h + SpMax1_Bhm + SpMax4_Bhm + SpMin1_Bhm +  + 
  SpMax2_Bhv +  + SpMin1_Bhv +  +  + 
   + SpMin1_Bhe +  + SpMin6_Bhe + SpMin8_Bhe + 
   +  + SpMin1_Bhp +  + SpMin4_Bhp + 
   +  + SpMin1_Bhi + + SpMin8_Bhi + 
  SpMin2_Bhs + SpMin7_Bhs + C1SP2 + C2SP2 +  + SCH.4 + 
  SCH.6 + VCH.4 + VCH.7 + SC.5 + VC.5 + VPC.6 + SP.7 + ASP.7 + 
  AVP.4 + AVP.5 + AVP.7 + SpMAD_Dt + VR2_Dt +  +  + 
  SdO + minHBa + +  + maxHBa +  + maxtN + 
  maxaaN + maxsF + hmax + gmax + MAXDP + MAXDP2 + fragC +  + 
   + MDEN.23 + MLFER_A + MPC7 + MPC8 +  +  + 
  TPC + piPC6 + piPC7 + piPC9 + piPC10 + TpiPC + JGI2 + MWC8

mintsC       MDEO.11
SpMax2_Bhi   C3SP2
SpMin2_Bhm   SpMax4_Bhi
SpMax2_Bhe   MPC10
SpMax4_Bhv   SpMin2_Bhp
maxtsC       SpMax4_Bhp
StsC         MDEN.12
SpMin2_Bhv   StN
 SpMin2_Bhi  SpMax4_Bhe 
MPC9         AATS6e  
SpMin2_Bhe   SpMax2_Bhp
mintN     




ATS5m  AATS4m  AATS6m  AATS1v    AATS1p  mintsC       MDEO.11
AATS6p  ATSC4m  ATSC8v  ATSC6p  AATSC1m  AATSC8v  AATSC0p  SpMax2_Bhi   
AATSC6i  MATS2c  MATS8v  MATS6i  GATS6c  GATS6v  GATS4e  SpMin2_Bhm  
GATS1p  GATS7p  GATS4s  GATS6s  VE1_Dzv BCUTp.1l  SpMax2_Bhe   MPC10
BCUTp.1h  SpMax1_Bhm  SpMax4_Bhm  SpMin1_Bhm  SpMax4_Bhv   SpMin2_Bhp
SpMax2_Bhv    SpMin1_Bhv maxtsC       SpMax4_Bhp
SpMin1_Bhe    SpMin6_Bhe SpMin8_Bhe StsC         MDEN.12
SpMin1_Bhp    SpMin4_Bhp  SpMin2_Bhv   StN
SpMin1_Bhi   SpMin8_Bhi  SpMin2_Bhi  SpMax4_Bhe C3SP2
SpMin2_Bhs  SpMin7_Bhs  C1SP2  C2SP2    SCH.4
SCH.6  VCH.4  VCH.7 SC.5  VC.5  VPC.6  SP.7  ASP.7 
AVP.4  AVP.5  AVP.7  SpMAD_Dt VR2_Dt MPC9         AATS6e  
SdO minHBa     maxHBa    maxtN  SpMin2_Bhe   SpMax2_Bhp
maxaaN  maxsF  hmax  gmax  MAXDP  MAXDP2  fragC 
MDEN.23  MLFER_A  MPC7  MPC8 mintN SpMax4_Bhi
TPC + piPC6  piPC7  piPC9  piPC10 TpiPC JGI2  MWC8


















































