library(randomForest)
library(tidyverse)
##setwd("/Alaska_Spectral_Library")

##Reads in alaskasspeclib Predictor=rawspec_050nm
alaskaSpecLib_050nm_plants_more05 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more05.csv")
alaskaSpecLib_050nm_plants_more10 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more10.csv") 
alaskaSpecLib_050nm_plants_more15 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more15.csv")
alaskaSpecLib_050nm_plants_more20 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more20.csv") 
alaskaSpecLib_050nm_plants_equal05<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal05.csv")
alaskaSpecLib_050nm_plants_equal10<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal10.csv")
alaskaSpecLib_050nm_plants_equal15<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal15.csv")
alaskaSpecLib_050nm_plants_equal20<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal20.csv")

##Reads in alaskasspeclib Predictor=rawspec_1ST 6 PCAs
pca_050nm_plants_more05 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more05.csv")
pca_050nm_plants_more10 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more10.csv") 
pca_050nm_plants_more15 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more15.csv")
pca_050nm_plants_more20 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_more20.csv") 
pca_050nm_plants_equal05<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal05.csv")
pca_050nm_plants_equal10<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal10.csv")
pca_050nm_plants_equal15<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal15.csv")
pca_050nm_plants_equal20<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/pca_050nm_plants_equal20.csv")

##Removes unwanted metadata from dataframes
alaskaSpecLib_050nm_plants_more05 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL 
alaskaSpecLib_050nm_plants_more10 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_050nm_plants_more15 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_050nm_plants_more20 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_050nm_plants_equal05[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_050nm_plants_equal10[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_050nm_plants_equal15[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_050nm_plants_equal20[c("ScanID","PFT","PFT_3","area","Freq")] = NULL

#Converts species column to a factor
alaskaSpecLib_050nm_plants_more05 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more05 $PFT_2)
alaskaSpecLib_050nm_plants_more10 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more10 $PFT_2)
alaskaSpecLib_050nm_plants_more15 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more15 $PFT_2)
alaskaSpecLib_050nm_plants_more20 $PFT_2<-as.factor(alaskaSpecLib_050nm_plants_more20 $PFT_2)
alaskaSpecLib_050nm_plants_equal05$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal05$PFT_2)
alaskaSpecLib_050nm_plants_equal10$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal10$PFT_2)
alaskaSpecLib_050nm_plants_equal15$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal15$PFT_2)
alaskaSpecLib_050nm_plants_equal20$PFT_2<-as.factor(alaskaSpecLib_050nm_plants_equal20$PFT_2)

pca_050nm_plants_more05           $PFT_2<-as.factor(pca_050nm_plants_more05 $PFT_2)
pca_050nm_plants_more10           $PFT_2<-as.factor(pca_050nm_plants_more10 $PFT_2)
pca_050nm_plants_more15           $PFT_2<-as.factor(pca_050nm_plants_more15 $PFT_2)
pca_050nm_plants_more20           $PFT_2<-as.factor(pca_050nm_plants_more20 $PFT_2)
pca_050nm_plants_equal05          $PFT_2<-as.factor(pca_050nm_plants_equal05$PFT_2)
pca_050nm_plants_equal10          $PFT_2<-as.factor(pca_050nm_plants_equal10$PFT_2)
pca_050nm_plants_equal15          $PFT_2<-as.factor(pca_050nm_plants_equal15$PFT_2)
pca_050nm_plants_equal20          $PFT_2<-as.factor(pca_050nm_plants_equal20$PFT_2)

###Creates random forest model for (plant and abiotic scans)
rf_050nm_more05    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_more05,mtry=5,ntree=2001,importance=TRUE)
rf_050nm_more10    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_more10,mtry=5,ntree=2001,importance=TRUE)
rf_050nm_more15    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_more15,mtry=5,ntree=2001,importance=TRUE)
rf_050nm_more20    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_more20,mtry=5,ntree=2001,importance=TRUE)

PCA_rf_050nm_more05<-randomForest(PFT_2~.,data=pca_050nm_plants_more05          ,mtry=5,ntree=2001,importance=TRUE)
PCA_rf_050nm_more10<-randomForest(PFT_2~.,data=pca_050nm_plants_more10          ,mtry=5,ntree=2001,importance=TRUE)
PCA_rf_050nm_more15<-randomForest(PFT_2~.,data=pca_050nm_plants_more15          ,mtry=5,ntree=2001,importance=TRUE)
PCA_rf_050nm_more20<-randomForest(PFT_2~.,data=pca_050nm_plants_more20          ,mtry=5,ntree=2001,importance=TRUE)

##Create data frame showing showing percent error for each species
species_050nm_error_more05    <-rf_050nm_more05$confusion%>%as.data.frame%>%select(class.error)
species_050nm_error_more10    <-rf_050nm_more10$confusion%>%as.data.frame%>%select(class.error)
species_050nm_error_more15    <-rf_050nm_more15$confusion%>%as.data.frame%>%select(class.error)
species_050nm_error_more20    <-rf_050nm_more20$confusion%>%as.data.frame%>%select(class.error)

PCA_species_050nm_error_more05<-PCA_rf_050nm_more05$confusion%>%as.data.frame%>%select(class.error)
PCA_species_050nm_error_more10<-PCA_rf_050nm_more10$confusion%>%as.data.frame%>%select(class.error)
PCA_species_050nm_error_more15<-PCA_rf_050nm_more15$confusion%>%as.data.frame%>%select(class.error)
PCA_species_050nm_error_more20<-PCA_rf_050nm_more20$confusion%>%as.data.frame%>%select(class.error)

##Change Index column to first column 
species_050nm_error_more05<- cbind(Species = rownames(species_050nm_error_more05), species_050nm_error_more05)%>%`rownames<-`(seq_len(nrow(species_050nm_error_more05)))
species_050nm_error_more10<- cbind(Species = rownames(species_050nm_error_more10), species_050nm_error_more10)%>%`rownames<-`(seq_len(nrow(species_050nm_error_more10)))
species_050nm_error_more15<- cbind(Species = rownames(species_050nm_error_more15), species_050nm_error_more15)%>%`rownames<-`(seq_len(nrow(species_050nm_error_more15)))
species_050nm_error_more20<- cbind(Species = rownames(species_050nm_error_more20), species_050nm_error_more20)%>%`rownames<-`(seq_len(nrow(species_050nm_error_more20)))

PCA_species_050nm_error_more05<- cbind(Species = rownames(PCA_species_050nm_error_more05), PCA_species_050nm_error_more05)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_more05)))
PCA_species_050nm_error_more10<- cbind(Species = rownames(PCA_species_050nm_error_more10), PCA_species_050nm_error_more10)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_more10)))
PCA_species_050nm_error_more15<- cbind(Species = rownames(PCA_species_050nm_error_more15), PCA_species_050nm_error_more15)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_more15)))
PCA_species_050nm_error_more20<- cbind(Species = rownames(PCA_species_050nm_error_more20), PCA_species_050nm_error_more20)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_more20)))

##Changes column name class.error 
names(species_050nm_error_more05)[2]<-"Raw_050nm"
names(species_050nm_error_more10)[2]<-"Raw_050nm"
names(species_050nm_error_more15)[2]<-"Raw_050nm"
names(species_050nm_error_more20)[2]<-"Raw_050nm"

names(PCA_species_050nm_error_more05)[2]<-"Raw_PCA_050nm"
names(PCA_species_050nm_error_more10)[2]<-"Raw_PCA_050nm"
names(PCA_species_050nm_error_more15)[2]<-"Raw_PCA_050nm"
names(PCA_species_050nm_error_more20)[2]<-"Raw_PCA_050nm"

#Creates dataframe showing the OOB estimate of error rate of the model
Overall_050nm_error_more05<-rf_050nm_more05$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_more05")
Overall_050nm_error_more10<-rf_050nm_more10$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_more10")
Overall_050nm_error_more15<-rf_050nm_more15$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_more15")
Overall_050nm_error_more20<-rf_050nm_more20$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_more20")

PCA_Overall_050nm_error_more05<-PCA_rf_050nm_more05$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_more05")
PCA_Overall_050nm_error_more10<-PCA_rf_050nm_more10$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_more10")
PCA_Overall_050nm_error_more15<-PCA_rf_050nm_more15$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_more15")
PCA_Overall_050nm_error_more20<-PCA_rf_050nm_more20$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_more20")

################################Model equal05, equal10,equal15,equal20#######################################
##Creates random forest model for (plant and abiotic scans)
rf_050nm_equal05    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_equal05,mtry=5,ntree=2001,importance=TRUE)
rf_050nm_equal10    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_equal10,mtry=5,ntree=2001,importance=TRUE)
rf_050nm_equal15    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_equal15,mtry=5,ntree=2001,importance=TRUE)
rf_050nm_equal20    <-randomForest(PFT_2~.,data=alaskaSpecLib_050nm_plants_equal20,mtry=5,ntree=2001,importance=TRUE)

PCA_rf_050nm_equal05<-randomForest(PFT_2~.,data=pca_050nm_plants_equal05           ,mtry=5,ntree=2001,importance=TRUE)
PCA_rf_050nm_equal10<-randomForest(PFT_2~.,data=pca_050nm_plants_equal10           ,mtry=5,ntree=2001,importance=TRUE)
PCA_rf_050nm_equal15<-randomForest(PFT_2~.,data=pca_050nm_plants_equal15           ,mtry=5,ntree=2001,importance=TRUE)
PCA_rf_050nm_equal20<-randomForest(PFT_2~.,data=pca_050nm_plants_equal20           ,mtry=5,ntree=2001,importance=TRUE)

##Create data frame showing percent error for each species
species_050nm_error_equal05<-rf_050nm_equal05$confusion%>%as.data.frame%>%select(class.error)
species_050nm_error_equal10<-rf_050nm_equal10$confusion%>%as.data.frame%>%select(class.error)
species_050nm_error_equal15<-rf_050nm_equal15$confusion%>%as.data.frame%>%select(class.error)
species_050nm_error_equal20<-rf_050nm_equal20$confusion%>%as.data.frame%>%select(class.error)

PCA_species_050nm_error_equal05<-PCA_rf_050nm_equal05$confusion%>%as.data.frame%>%select(class.error)
PCA_species_050nm_error_equal10<-PCA_rf_050nm_equal10$confusion%>%as.data.frame%>%select(class.error)
PCA_species_050nm_error_equal15<-PCA_rf_050nm_equal15$confusion%>%as.data.frame%>%select(class.error)
PCA_species_050nm_error_equal20<-PCA_rf_050nm_equal20$confusion%>%as.data.frame%>%select(class.error)

##Change Index column to first column 
species_050nm_error_equal05<- cbind(Species = rownames(species_050nm_error_equal05), species_050nm_error_equal05)%>%`rownames<-`(seq_len(nrow(species_050nm_error_equal05)))
species_050nm_error_equal10<- cbind(Species = rownames(species_050nm_error_equal10), species_050nm_error_equal10)%>%`rownames<-`(seq_len(nrow(species_050nm_error_equal10)))
species_050nm_error_equal15<- cbind(Species = rownames(species_050nm_error_equal15), species_050nm_error_equal15)%>%`rownames<-`(seq_len(nrow(species_050nm_error_equal15)))
species_050nm_error_equal20<- cbind(Species = rownames(species_050nm_error_equal20), species_050nm_error_equal20)%>%`rownames<-`(seq_len(nrow(species_050nm_error_equal20)))

PCA_species_050nm_error_equal05<- cbind(Species = rownames(PCA_species_050nm_error_equal05), PCA_species_050nm_error_equal05)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_equal05)))
PCA_species_050nm_error_equal10<- cbind(Species = rownames(PCA_species_050nm_error_equal10), PCA_species_050nm_error_equal10)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_equal10)))
PCA_species_050nm_error_equal15<- cbind(Species = rownames(PCA_species_050nm_error_equal15), PCA_species_050nm_error_equal15)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_equal15)))
PCA_species_050nm_error_equal20<- cbind(Species = rownames(PCA_species_050nm_error_equal20), PCA_species_050nm_error_equal20)%>%`rownames<-`(seq_len(nrow(PCA_species_050nm_error_equal20)))

##Changes column name class.error 
names(species_050nm_error_equal05)[2]<-"Raw_050nm"
names(species_050nm_error_equal10)[2]<-"Raw_050nm"
names(species_050nm_error_equal15)[2]<-"Raw_050nm"
names(species_050nm_error_equal20)[2]<-"Raw_050nm"

names(PCA_species_050nm_error_equal05)[2]<-"Raw_PCA_050nm"
names(PCA_species_050nm_error_equal10)[2]<-"Raw_PCA_050nm"
names(PCA_species_050nm_error_equal15)[2]<-"Raw_PCA_050nm"
names(PCA_species_050nm_error_equal20)[2]<-"Raw_PCA_050nm"


#Creates dataframe showing the OOB estimate of error rate of the model
Overall_050nm_error_equal05<-rf_050nm_equal05$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_equal05")
Overall_050nm_error_equal10<-rf_050nm_equal10$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_equal10")
Overall_050nm_error_equal15<-rf_050nm_equal15$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_equal15")
Overall_050nm_error_equal20<-rf_050nm_equal20$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw"))%>%mutate(Category="050nm_equal20")

PCA_Overall_050nm_error_equal05<-PCA_rf_050nm_equal05$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_equal05")
PCA_Overall_050nm_error_equal10<-PCA_rf_050nm_equal10$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_equal10")
PCA_Overall_050nm_error_equal15<-PCA_rf_050nm_equal15$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_equal15")
PCA_Overall_050nm_error_equal20<-PCA_rf_050nm_equal20$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("PCA"))%>%mutate(Category="050nm_equal20")

################################Ceating datafreames showing errors for models#######################################
##Combines all those dataframes that were created showing the OOB estimate of error rate of the model
ModelOOB_050nm_raw_spec<-rbind(Overall_050nm_error_more05
                              ,Overall_050nm_error_more10
                              ,Overall_050nm_error_more15
                              ,Overall_050nm_error_more20
                              ,Overall_050nm_error_equal05
                              ,Overall_050nm_error_equal10
                              ,Overall_050nm_error_equal15
                              ,Overall_050nm_error_equal20)

ModeModelOOB_050nm_PCA_spec<-rbind(PCA_Overall_050nm_error_equal05
                                  ,PCA_Overall_050nm_error_equal10
                                  ,PCA_Overall_050nm_error_equal15
                                  ,PCA_Overall_050nm_error_equal20
                                  ,PCA_Overall_050nm_error_more05
                                  ,PCA_Overall_050nm_error_more10
                                  ,PCA_Overall_050nm_error_more15
                                  ,PCA_Overall_050nm_error_more20)

ModelOOB_050nm_raw<-merge(ModelOOB_050nm_raw_spec,ModeModelOOB_050nm_PCA_spec,by="Category")

#######################################export dataframes################################################################
###export relvant dataframes 
write.csv(ModelOOB_050nm_raw  ,"Processed_spec/Error_rates/Raw/ModelOOB_050nm_raw.csv", row.names = F)

write.csv(species_050nm_error_more05   ,"Processed_spec/Error_rates/Raw/species_050nm_error_more05_raw.csv", row.names = F)
write.csv(species_050nm_error_more10   ,"Processed_spec/Error_rates/Raw/species_050nm_error_more10_raw.csv", row.names = F)
write.csv(species_050nm_error_more15   ,"Processed_spec/Error_rates/Raw/species_050nm_error_more15_raw.csv", row.names = F)
write.csv(species_050nm_error_more20   ,"Processed_spec/Error_rates/Raw/species_050nm_error_more20_raw.csv", row.names = F)

write.csv(PCA_species_050nm_error_more05   ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_more05_raw.csv", row.names = F)
write.csv(PCA_species_050nm_error_more10   ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_more10_raw.csv", row.names = F)
write.csv(PCA_species_050nm_error_more15   ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_more15_raw.csv", row.names = F)
write.csv(PCA_species_050nm_error_more20   ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_more20_raw.csv", row.names = F)

write.csv(species_050nm_error_equal05  ,"Processed_spec/Error_rates/Raw/species_050nm_error_equal05_raw.csv", row.names = F)
write.csv(species_050nm_error_equal10  ,"Processed_spec/Error_rates/Raw/species_050nm_error_equal10_raw.csv", row.names = F)
write.csv(species_050nm_error_equal15  ,"Processed_spec/Error_rates/Raw/species_050nm_error_equal15_raw.csv", row.names = F)
write.csv(species_050nm_error_equal20  ,"Processed_spec/Error_rates/Raw/species_050nm_error_equal20_raw.csv", row.names = F)

write.csv(PCA_species_050nm_error_equal05  ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_equal05_raw.csv", row.names = F)
write.csv(PCA_species_050nm_error_equal10  ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_equal10_raw.csv", row.names = F)
write.csv(PCA_species_050nm_error_equal15  ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_equal15_raw.csv", row.names = F)
write.csv(PCA_species_050nm_error_equal20  ,"Processed_spec/Error_rates/Raw/PCA_species_050nm_error_equal20_raw.csv", row.names = F)

