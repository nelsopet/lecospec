###################################Headwall_FLIGHT_LINES####################################
library(raster)
library(spectrolab)
library(tidyverse)

##Reads in Imagery as multi layer raster
Clayton_HDW_Tiff_Spatial<-brick("Imagery/Headwall_clip")

##Reads in bandpasses for imagery
HDW_ng_wv<-scan("Processed_imagery/Headwall/Bandpasses/Headwall_WVLs", numeric())

names(Clayton_HDW_Tiff_Spatial)<-HDW_ng_wv

##Converts raster layer to combined dataframe 
Clayton_HDW_Tiff_df<-rasterToPoints(Clayton_HDW_Tiff_Spatial)%>% as.data.frame()

##Reads in bandpasses for imagery
HDW_ng_wv<-scan("Processed_imagery/Headwall/Bandpasses/Headwall_WVLs", numeric())

##change column names of the df above to be numeric
colnames(Clayton_HDW_Tiff_df)[-1:-2]<-HDW_ng_wv

##Reads in spectral library as .rda
alaskaSpecLib_HDW<-readRDS("Processed_spec/alaskaSpeclib_rds.rds")

##Resample spectral library based on Imagery bandpasses
alaskaSpecLib_HDW<-spectrolab::resample(alaskaSpecLib_HDW,HDW_ng_wv)%>%as.data.frame()

###deletes col "sample_name"
alaskaSpecLib_HDW$sample_name<-NULL

#Changes band names so they'll correspond to the ones from the images
colnames(alaskaSpecLib_HDW)[-(1:6)]<-c(colnames(Clayton_HDW_Tiff_df)[-(1:2)])

##change column names of the df above to be numeric
colnames(alaskaSpecLib_HDW)[-1:-6]<-HDW_ng_wv

## Removes unwanted metadata from dataframes to be used for PCA Calculation later
drop<-c("ScanID", "PFT","PFT_2", "Freq", "area","sample_name")

alaskaSpecLib_pca_HDW<-alaskaSpecLib_HDW        [,!(names(alaskaSpecLib_HDW        ) %in% drop)]

##Do PCA calculation
PCA_preds_HDW <- prcomp(alaskaSpecLib_pca_HDW [,-1], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants <- round(PCA_preds_HDW$sdev^2/sum(PCA_preds_HDW$sdev^2)*100)

####fist 6 PC's
pc6<-c(1:6)

#creates dataframe with first 6 PCA vales for each species
names_species     <-as.data.frame(as.character(alaskaSpecLib_pca_HDW $PFT_3))
PCA_preds_HDW <-as.data.frame(cbind(names_species ,PCA_preds_HDW $x[,pc6]))
names(PCA_preds_HDW )[1]<-paste("PFT_3")




##creates datafranme with equal scans for each species (10,15,20 scans per species )
alaskaSpecLib_HDW_equal05_PFT3<-alaskaSpecLib_HDW %>% group_by(PFT_3) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_HDW_equal10_PFT3<-alaskaSpecLib_HDW %>% group_by(PFT_3) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_HDW_equal15_PFT3<-alaskaSpecLib_HDW %>% group_by(PFT_3) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_HDW_equal20_PFT3<-alaskaSpecLib_HDW %>% group_by(PFT_3) %>% sample_n(20,replace = TRUE)

PCA_preds_HDW_equal50_PFT3 <-PCA_preds_HDW %>% group_by(PFT_3) %>% sample_n(50 ,replace = TRUE)
PCA_preds_HDW_equal70_PFT3 <-PCA_preds_HDW %>% group_by(PFT_3) %>% sample_n(70 ,replace = TRUE)
PCA_preds_HDW_equal90_PFT3 <-PCA_preds_HDW %>% group_by(PFT_3) %>% sample_n(90 ,replace = TRUE)
PCA_preds_HDW_equal110_PFT3<-PCA_preds_HDW %>% group_by(PFT_3) %>% sample_n(110,replace = TRUE)
PCA_preds_HDW_equal200_PFT3<-PCA_preds_HDW %>% group_by(PFT_3) %>% sample_n(200,replace = TRUE)



##Save spectral Library to be used in imagery prediction 
write.csv(Clayton_HDW_Tiff_df                        ,"Processed_spec/Imagery/Raw/Clayton_HDW_Tiff_df.csv     ",row.names= FALSE)
write.csv(alaskaSpecLib_HDW                          ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW.csv       ",row.names= FALSE)

write.csv(alaskaSpecLib_HDW_equal05_PFT3                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal05_PFT3.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_equal10_PFT3                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal10_PFT3.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_equal15_PFT3                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal15_PFT3.csv",row.names = FALSE)
write.csv(alaskaSpecLib_HDW_equal20_PFT3                  ,"Processed_spec/Imagery/Raw/alaskaSpecLib_HDW_equal20_PFT3.csv",row.names = FALSE)

write.csv(PCA_preds_HDW                                ,"Processed_spec/Imagery/Raw/PCA_preds_HDW.csv"              ,row.names = FALSE)
write.csv(PCA_preds_HDW_equal50_PFT3                   ,"Processed_spec/Imagery/Raw/PCA_preds_HDW_equal50_PFT3 .csv",row.names = FALSE)
write.csv(PCA_preds_HDW_equal70_PFT3                   ,"Processed_spec/Imagery/Raw/PCA_preds_HDW_equal70_PFT3 .csv",row.names = FALSE)
write.csv(PCA_preds_HDW_equal90_PFT3                   ,"Processed_spec/Imagery/Raw/PCA_preds_HDW_equal90_PFT3 .csv",row.names = FALSE)
write.csv(PCA_preds_HDW_equal110_PFT3                  ,"Processed_spec/Imagery/Raw/PCA_preds_HDW_equal110_PFT3.csv",row.names = FALSE)
write.csv(PCA_preds_HDW_equal200_PFT3                  ,"Processed_spec/Imagery/Raw/PCA_preds_HDW_equal200_PFT3.csv",row.names = FALSE)



