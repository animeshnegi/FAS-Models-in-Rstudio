

data <- read.delim("C:/Users/animesh negi/Desktop/dis/fresh/data.tsv")



#     ----------------- REMOVING NA's   removed 32 mol

zeroVar <- function(data, useNA = 'ifany') {
  out <- apply(data, 2, function(x) {length(table(x, useNA = useNA))})
  which(out==1)
}

1030+996

data <- data[,-zeroVar(data, useNA = 'no')]

data <- data %>% filter(is.na(Standard.Value)==FALSE)








#--------------------------------------------------- Data Cleaning  


data <- filter(data,Standard.Units=="nM")


# data remains to 3107 removed other data have 	ug.mL-1 units 98 mol removed

3087-3237

data <- filter(data,Standard.Type=="IC50")

#data remains to 3087 removed other data have Ki value

data <- filter(data,is.na(Molecule.ChEMBL.ID)==FALSE)


data <- data %>%  distinct(Smiles, .keep_all = TRUE)


2036-3087



#------------------------------------------Converting IC50 into PIC50 value  

pic <- function(x){
  y = 9-log10(x)
  return(y)
}

pIC50 <- data$Standard.Value
pIC50 <- as.data.frame(pIC50)

b <- lapply(pIC50, pic)
b <- as.data.frame(b) 

data <- cbind(b,data)

summary(data$pIC50)

summary(data$Standard.Value)


inactive <- data %>%  
  filter(pIC50<5.549)

# for active molecule cut of value is 160-20,000 nM

active <- data %>%  
  filter(pIC50>=5.549)

inactive <- inactive %>%  
  filter(Standard.Value<=100000)


#------------------------ 1018 inactive and 1014 active


inactive <-inactive %>%  distinct(Molecule.ChEMBL.ID, .keep_all = TRUE ) 

active <-active %>%  distinct(Molecule.ChEMBL.ID, .keep_all = TRUE ) 


#----------------NA
active <- filter(active,is.na(Smiles)==FALSE)

inactive <- filter(inactive,is.na(Smiles)==FALSE)



#------------------- Activity 


active$activity <- "1"

inactive$activity <- "0"

di <- rbind(active,inactive)

str(di$activity)

convert <- c(36)

di[,convert] <- data.frame(apply(di[convert], 1, as.factor))


str(di$activity)


write.csv(di,"C:/Users/animesh negi/Desktop/dis/fresh/final2.csv")

1444+166













































































































































