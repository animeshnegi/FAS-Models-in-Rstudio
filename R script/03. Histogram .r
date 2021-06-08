
## importing data set 

DATASET <- read.csv("C:/Users/animesh negi/Desktop/dis/fresh/DATASET.csv")

final <-  select(DATASET,Molecule.ChEMBL.ID,Standard.Value,pIC50)

#______________________________________  G R A P H

final <- arrange(final,desc(Molecule.ChEMBL.ID))
final$row <- seq.int(nrow(final))




final %>% 
  arrange(final,desc(Molecule.ChEMBL.ID)) %>% 
  ggplot(aes(x=row,y = Standard.Value))+
  geom_histogram(fill = "blue",stat = "identity",width = 5)+
  labs(y = "IC50 Value",
       x = "Number of molecule",
       title = "Range of IC50 values of data"
  )+
  scale_x_continuous(breaks = seq(1, 2026, 126), 
                     limits=c(1,2026))


#   GRAPH WITH pIC50

final %>% 
  ggplot(aes(x=row,y = pIC50))+
  geom_histogram(fill = "blue",stat = "identity",width = 5)+
  labs(y = "pIC50 Value",
       x = "Number of molecule",
       title = "Range of pIC50 values of data"
  )+
  scale_x_continuous(breaks = seq(1, 2026, 126), 
                     limits=c(1,2026))

