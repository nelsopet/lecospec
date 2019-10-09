library(randomForest)
library(tidyverse)
##setwd("/Alaska_Spectral_Library")

##Reads in alaskasspeclib Predictor=smoothspec_050nm
alaskaSpecLib_smooth_050nm_plants_more05 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_more05.csv")
alaskaSpecLib_smooth_050nm_plants_more10 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_more10.csv") 
alaskaSpecLib_smooth_050nm_plants_more15 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_more15.csv")
alaskaSpecLib_smooth_050nm_plants_more20 <-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_more20.csv") 
alaskaSpecLib_smooth_050nm_plants_equal05<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_equal05.csv")
alaskaSpecLib_smooth_050nm_plants_equal10<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_equal10.csv")
alaskaSpecLib_smooth_050nm_plants_equal15<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_equal15.csv")
alaskaSpecLib_smooth_050nm_plants_equal20<-read.csv("Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_050nm_plants_equal20.csv")

##Removes unwanted metadata from dataframes
alaskaSpecLib_smooth_050nm_plants_more05 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL 
alaskaSpecLib_smooth_050nm_plants_more10 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_smooth_050nm_plants_more15 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_smooth_050nm_plants_more20 [c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_smooth_050nm_plants_equal05[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_smooth_050nm_plants_equal10[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_smooth_050nm_plants_equal15[c("ScanID","PFT","PFT_3","area","Freq")] = NULL
alaskaSpecLib_smooth_050nm_plants_equal20[c("ScanID","PFT","PFT_3","area","Freq")] = NULL

##Converts species column to a factor
alaskaSpecLib_smooth_050nm_plants_more05 $PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_more05 $PFT_2)
alaskaSpecLib_smooth_050nm_plants_more10 $PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_more10 $PFT_2)
alaskaSpecLib_smooth_050nm_plants_more15 $PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_more15 $PFT_2)
alaskaSpecLib_smooth_050nm_plants_more20 $PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_more20 $PFT_2)
alaskaSpecLib_smooth_050nm_plants_equal05$PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_equal05$PFT_2)
alaskaSpecLib_smooth_050nm_plants_equal10$PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_equal10$PFT_2)
alaskaSpecLib_smooth_050nm_plants_equal15$PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_equal15$PFT_2)
alaskaSpecLib_smooth_050nm_plants_equal20$PFT_2<-as.factor(alaskaSpecLib_smooth_050nm_plants_equal20$PFT_2)

###Creates random forest model for (plant and abiotic scans)
rf_smooth_050nm_more05    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_more05,mtry=5,ntree=2001,importance=TRUE)
rf_smooth_050nm_more10    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_more10,mtry=5,ntree=2001,importance=TRUE)
rf_smooth_050nm_more15    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_more15,mtry=5,ntree=2001,importance=TRUE)
rf_smooth_050nm_more20    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_more20,mtry=5,ntree=2001,importance=TRUE)

##Create data frame showing showing percent error for each species
species_error_smooth_050nm_more05    <-rf_smooth_050nm_more05$confusion%>%as.data.frame%>%select(class.error)
species_error_smooth_050nm_more10    <-rf_smooth_050nm_more10$confusion%>%as.data.frame%>%select(class.error)
species_error_smooth_050nm_more15    <-rf_smooth_050nm_more15$confusion%>%as.data.frame%>%select(class.error)
species_error_smooth_050nm_more20    <-rf_smooth_050nm_more20$confusion%>%as.data.frame%>%select(class.error)

##Change Index column to first column 
species_error_smooth_050nm_more05<- cbind(Species = rownames(species_error_smooth_050nm_more05), species_error_smooth_050nm_more05)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_more05)))
species_error_smooth_050nm_more10<- cbind(Species = rownames(species_error_smooth_050nm_more10), species_error_smooth_050nm_more10)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_more10)))
species_error_smooth_050nm_more15<- cbind(Species = rownames(species_error_smooth_050nm_more15), species_error_smooth_050nm_more15)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_more15)))
species_error_smooth_050nm_more20<- cbind(Species = rownames(species_error_smooth_050nm_more20), species_error_smooth_050nm_more20)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_more20)))

##Changes column name class.error 
names(species_error_smooth_050nm_more05)[2]<-"smooth_050nm_more05"
names(species_error_smooth_050nm_more10)[2]<-"smooth_050nm_more10"
names(species_error_smooth_050nm_more15)[2]<-"smooth_050nm_more15"
names(species_error_smooth_050nm_more20)[2]<-"smooth_050nm_more20"

#Creates dataframe showing the OOB estimate of error rate of the model
Overall_error_smooth_050nm_more05<-rf_smooth_050nm_more05$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_more05"))%>%mutate(Category="050nm_bands")
Overall_error_smooth_050nm_more10<-rf_smooth_050nm_more10$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_more10"))%>%mutate(Category="050nm_bands")
Overall_error_smooth_050nm_more15<-rf_smooth_050nm_more15$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_more15"))%>%mutate(Category="050nm_bands")
Overall_error_smooth_050nm_more20<-rf_smooth_050nm_more20$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_more20"))%>%mutate(Category="050nm_bands")

##Subset all the species that had an error rate lower than 30%
species_error_smooth_050nm_more05_below30<-subset(species_error_smooth_050nm_more05,smooth_050nm_more05<0.3)
species_error_smooth_050nm_more10_below30<-subset(species_error_smooth_050nm_more10,smooth_050nm_more10<0.3)
species_error_smooth_050nm_more15_below30<-subset(species_error_smooth_050nm_more15,smooth_050nm_more15<0.3)
species_error_smooth_050nm_more20_below30<-subset(species_error_smooth_050nm_more20,smooth_050nm_more20<0.3)

################################Model equal05, equal10,equal15,equal20#######################################
##Creates random forest model for (plant and abiotic scans)
rf_smooth_050nm_equal05    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_equal05,mtry=5,ntree=2001,importance=TRUE)
rf_smooth_050nm_equal10    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_equal10,mtry=5,ntree=2001,importance=TRUE)
rf_smooth_050nm_equal15    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_equal15,mtry=5,ntree=2001,importance=TRUE)
rf_smooth_050nm_equal20    <-randomForest(PFT_2~.,data=alaskaSpecLib_smooth_050nm_plants_equal20,mtry=5,ntree=2001,importance=TRUE)

##Create data frame showing percent error for each species
species_error_smooth_050nm_equal05<-rf_smooth_050nm_equal05$confusion%>%as.data.frame%>%select(class.error)
species_error_smooth_050nm_equal10<-rf_smooth_050nm_equal10$confusion%>%as.data.frame%>%select(class.error)
species_error_smooth_050nm_equal15<-rf_smooth_050nm_equal15$confusion%>%as.data.frame%>%select(class.error)
species_error_smooth_050nm_equal20<-rf_smooth_050nm_equal20$confusion%>%as.data.frame%>%select(class.error)

##Change Index column to first column 
species_error_smooth_050nm_equal05<- cbind(Species = rownames(species_error_smooth_050nm_equal05), species_error_smooth_050nm_equal05)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_equal05)))
species_error_smooth_050nm_equal10<- cbind(Species = rownames(species_error_smooth_050nm_equal10), species_error_smooth_050nm_equal10)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_equal10)))
species_error_smooth_050nm_equal15<- cbind(Species = rownames(species_error_smooth_050nm_equal15), species_error_smooth_050nm_equal15)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_equal15)))
species_error_smooth_050nm_equal20<- cbind(Species = rownames(species_error_smooth_050nm_equal20), species_error_smooth_050nm_equal20)%>%`rownames<-`(seq_len(nrow(species_error_smooth_050nm_equal20)))

##Changes column name class.error 
names(species_error_smooth_050nm_equal05)[2]<-"smooth_050nm_equal05"
names(species_error_smooth_050nm_equal10)[2]<-"smooth_050nm_equal10"
names(species_error_smooth_050nm_equal15)[2]<-"smooth_050nm_equal15"
names(species_error_smooth_050nm_equal20)[2]<-"smooth_050nm_equal20"

#Creates dataframe showing the OOB estimate of error rate of the model
Overall_error_smooth_050nm_equal05<-rf_smooth_050nm_equal05$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_equal05"))%>%mutate(Category="050nm_bands")
Overall_error_smooth_050nm_equal10<-rf_smooth_050nm_equal10$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_equal10"))%>%mutate(Category="050nm_bands")
Overall_error_smooth_050nm_equal15<-rf_smooth_050nm_equal15$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_equal15"))%>%mutate(Category="050nm_bands")
Overall_error_smooth_050nm_equal20<-rf_smooth_050nm_equal20$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("smooth_050nm_equal20"))%>%mutate(Category="050nm_bands")

##Subset all the species that had an error rate lower than 30%
species_error_smooth_050nm_equal05_below30<-subset(species_error_smooth_050nm_equal05,smooth_050nm_equal05<0.3)
species_error_smooth_050nm_equal10_below30<-subset(species_error_smooth_050nm_equal10,smooth_050nm_equal10<0.3)
species_error_smooth_050nm_equal15_below30<-subset(species_error_smooth_050nm_equal15,smooth_050nm_equal15<0.3)
species_error_smooth_050nm_equal20_below30<-subset(species_error_smooth_050nm_equal20,smooth_050nm_equal20<0.3)

################################Ceating datafreames showing errors for models#######################################
##Combines all those dataframes that were created showing the OOB estimate of error rate of the model
ModelOOB_050nm_smooth<-Reduce(function(x,y) merge(x,y,by="Category",all=TRUE) ,list(Overall_error_smooth_050nm_more05
                                                                                    ,Overall_error_smooth_050nm_more10
                                                                                    ,Overall_error_smooth_050nm_more15
                                                                                    ,Overall_error_smooth_050nm_more20
                                                                                    ,Overall_error_smooth_050nm_equal05
                                                                                    ,Overall_error_smooth_050nm_equal10
                                                                                    ,Overall_error_smooth_050nm_equal15
                                                                                    ,Overall_error_smooth_050nm_equal20))

#######################################export dataframes################################################################
###export relvant dataframes 
write.csv(ModelOOB_050nm_smooth  ,"Processed_spec/Error_rates/Smooth/ModelOOB_050nm_smooth.csv", row.names = F)

write.csv(species_error_smooth_050nm_more05   ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more05_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_more10   ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more10_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_more15   ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more15_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_more20   ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more20_smooth.csv", row.names = F)

write.csv(species_error_smooth_050nm_equal05  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal05_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_equal10  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal10_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_equal15  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal15_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_equal20  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal20_smooth.csv", row.names = F)

write.csv(species_error_smooth_050nm_more05_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more05_below30_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_more10_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more10_below30_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_more15_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more15_below30_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_more20_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_more20_below30_smooth.csv", row.names = F)

write.csv(species_error_smooth_050nm_equal05_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal05_below30_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_equal10_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal10_below30_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_equal15_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal15_below30_smooth.csv", row.names = F)
write.csv(species_error_smooth_050nm_equal20_below30  ,"Processed_spec/Error_rates/Smooth/species_error_smooth_050nm_equal20_below30_smooth.csv", row.names = F)
