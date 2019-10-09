##PCA DATATABLES 

###reads in alaskasspeclib
alaskaSpecLib_050nm_plants_more05 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more05.csv")
alaskaSpecLib_050nm_plants_more10 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more10.csv") 
alaskaSpecLib_050nm_plants_more15 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more15.csv")
alaskaSpecLib_050nm_plants_more20 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more20.csv") 
alaskaSpecLib_050nm_plants_equal05<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal05.csv")
alaskaSpecLib_050nm_plants_equal10<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal10.csv")
alaskaSpecLib_050nm_plants_equal15<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal15.csv")
alaskaSpecLib_050nm_plants_equal20<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal20.csv")

## Removes unwanted metadata from dataframes
alaskaSpecLib_050nm_plants_more05 [c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL 
alaskaSpecLib_050nm_plants_more10 [c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
alaskaSpecLib_050nm_plants_more15 [c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
alaskaSpecLib_050nm_plants_more20 [c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
alaskaSpecLib_050nm_plants_equal05[c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
alaskaSpecLib_050nm_plants_equal10[c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
alaskaSpecLib_050nm_plants_equal15[c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
alaskaSpecLib_050nm_plants_equal20[c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL

#Converts species column to a factor
alaskaSpecLib_050nm_plants_more05 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more05 $PFT_2)
alaskaSpecLib_050nm_plants_more10 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more10 $PFT_2)
alaskaSpecLib_050nm_plants_more15 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more15 $PFT_2)
alaskaSpecLib_050nm_plants_more20 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more20 $PFT_2)
alaskaSpecLib_050nm_plants_equal05$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal05$PFT_2)
alaskaSpecLib_050nm_plants_equal10$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal10$PFT_2)
alaskaSpecLib_050nm_plants_equal15$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal15$PFT_2)
alaskaSpecLib_050nm_plants_equal20$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal20$PFT_2)

##Creates PCA (all bands)
pca_050nm_plants_more05 <- prcomp(alaskaSpecLib_050nm_plants_more05 [,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_more10 <- prcomp(alaskaSpecLib_050nm_plants_more10 [,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_more15 <- prcomp(alaskaSpecLib_050nm_plants_more15 [,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_more20 <- prcomp(alaskaSpecLib_050nm_plants_more20 [,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_equal05<- prcomp(alaskaSpecLib_050nm_plants_equal05[,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_equal10<- prcomp(alaskaSpecLib_050nm_plants_equal10[,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_equal15<- prcomp(alaskaSpecLib_050nm_plants_equal15[,-1], retx=TRUE, center=TRUE, scale=TRUE)
pca_050nm_plants_equal20<- prcomp(alaskaSpecLib_050nm_plants_equal20[,-1], retx=TRUE, center=TRUE, scale=TRUE)

expl.var_050nm_plants_more05 <- round(pca_050nm_plants_more05$sdev^2/sum(pca_050nm_plants_more05$sdev^2)*100)
expl.var_050nm_plants_more10 <- round(pca_050nm_plants_more10$sdev^2/sum(pca_050nm_plants_more10$sdev^2)*100)
expl.var_050nm_plants_more15 <- round(pca_050nm_plants_more15$sdev^2/sum(pca_050nm_plants_more15$sdev^2)*100)
expl.var_050nm_plants_more20 <- round(pca_050nm_plants_more20$sdev^2/sum(pca_050nm_plants_more20$sdev^2)*100)
expl.var_050nm_plants_equal05<- round(pca_050nm_plants_equal05$sdev^2/sum(pca_050nm_plants_equal05$sdev^2)*100)
expl.var_050nm_plants_equal10<- round(pca_050nm_plants_equal10$sdev^2/sum(pca_050nm_plants_equal10$sdev^2)*100)
expl.var_050nm_plants_equal15<- round(pca_050nm_plants_equal15$sdev^2/sum(pca_050nm_plants_equal15$sdev^2)*100)
expl.var_050nm_plants_equal20<- round(pca_050nm_plants_equal20$sdev^2/sum(pca_050nm_plants_equal20$sdev^2)*100)

####fist 6 PC's
pc6<-c(1:6)

#creates dataframe with first 6 PCA vales for each species
names_species_more05 <-as.data.frame(as.character(alaskaSpecLib_050nm_plants_more05 $PFT_2))
names_species_more10 <-as.data.frame(as.character(alaskaSpecLib_050nm_plants_more10 $PFT_2))
names_species_more15 <-as.data.frame(as.character(alaskaSpecLib_050nm_plants_more15 $PFT_2))
names_species_more20 <-as.data.frame(as.character(alaskaSpecLib_050nm_plants_more20 $PFT_2))
names_species_equal05<-as.data.frame(as.character(alaskaSpecLib_050nm_plants_equal05$PFT_2))
names_species_equal10<-as.data.frame(as.character(alaskaSpecLib_050nm_plants_equal10$PFT_2))
names_species_equal15<-as.data.frame(as.character(alaskaSpecLib_050nm_plants_equal15$PFT_2))
names_species_equal20<-as.data.frame(as.character(alaskaSpecLib_050nm_plants_equal20$PFT_2))

pca_050nm_plants_more05 <-as.data.frame(cbind(names_species_more05 ,pca_050nm_plants_more05 $x[,pc6]))
pca_050nm_plants_more10 <-as.data.frame(cbind(names_species_more10 ,pca_050nm_plants_more10 $x[,pc6]))
pca_050nm_plants_more15 <-as.data.frame(cbind(names_species_more15 ,pca_050nm_plants_more15 $x[,pc6]))
pca_050nm_plants_more20 <-as.data.frame(cbind(names_species_more20 ,pca_050nm_plants_more20 $x[,pc6]))
pca_050nm_plants_equal05<-as.data.frame(cbind(names_species_equal05,pca_050nm_plants_equal05$x[,pc6]))
pca_050nm_plants_equal10<-as.data.frame(cbind(names_species_equal10,pca_050nm_plants_equal10$x[,pc6]))
pca_050nm_plants_equal15<-as.data.frame(cbind(names_species_equal15,pca_050nm_plants_equal15$x[,pc6]))
pca_050nm_plants_equal20<-as.data.frame(cbind(names_species_equal20,pca_050nm_plants_equal20$x[,pc6]))

names(pca_050nm_plants_more05 )[1]<-paste("PFT_2")
names(pca_050nm_plants_more10 )[1]<-paste("PFT_2")
names(pca_050nm_plants_more15 )[1]<-paste("PFT_2")
names(pca_050nm_plants_more20 )[1]<-paste("PFT_2")
names(pca_050nm_plants_equal05)[1]<-paste("PFT_2")
names(pca_050nm_plants_equal10)[1]<-paste("PFT_2")
names(pca_050nm_plants_equal15)[1]<-paste("PFT_2")
names(pca_050nm_plants_equal20)[1]<-paste("PFT_2")

write.csv(pca_050nm_plants_more05 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more05.csv" , row.names = F)
write.csv(pca_050nm_plants_more10 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more10.csv" , row.names = F)
write.csv(pca_050nm_plants_more15 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more15.csv" , row.names = F)
write.csv(pca_050nm_plants_more20 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more20.csv" , row.names = F)
write.csv(pca_050nm_plants_equal05,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal05.csv", row.names = F)
write.csv(pca_050nm_plants_equal10,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal10.csv", row.names = F)
write.csv(pca_050nm_plants_equal15,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal15.csv", row.names = F)
write.csv(pca_050nm_plants_equal20,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal20.csv", row.names = F)


