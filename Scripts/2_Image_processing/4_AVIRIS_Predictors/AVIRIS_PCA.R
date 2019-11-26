##################################Calculates Principal Components for the spectral library that was resampled on our headwall bandpasses#####################################
library(spectrolab)
library(raster)
library(tidyverse)

##Reads in Imagery
Clayton_test_AV<-brick("Original_data/Test_imagery_AVIRIS/Clayton_test_AVIRIS")

##Marks raster as unrotated
Clayton_test_AV@rotated<-FALSE

##Converts to a dataframe
Clayton_test_AV<-rasterToPoints(Clayton_test_AV)%>% as.data.frame()

##Reads in spectral library....dim 1974  333
alaskaSpeclib_AV<-readRDS("Outputs/3_AV_Imagery/1_processing/alaskaSpeclib_AV.rds")%>%as.data.frame()%>%dplyr::select(-sample_name)

##Please note there are the number of samples per functional group within our spectral library
##alaskaSpeclib_AV$PFT_3%>%table()
##Abiotic  Dwarf Shrub    Forb     Graminoid    Lichen        Moss       Shrub        Tree 
## 58         991          79          21        464          90         241          25 

###First we want to make the amount of scans among functional groups equivalent...I'll make it 25 since that's the lowest amount of samples we have
alaskaSpecLib_AV_equal21<-alaskaSpeclib_AV %>% group_by(PFT_3) %>% sample_n(21,replace = TRUE)
       
##Do PCA calculation on spectral library and image..this calculates the PCA
##this function generates the principal components for "alaskaSpeclib_AV" and "Clayton_test_AV"
AV_PCAspeclib <- prcomp(alaskaSpecLib_AV_equal21[-1:-7], retx=TRUE, center=TRUE, scale=TRUE)
AV_PCAimage   <- prcomp(Clayton_test_AV         [-1:-2], retx=TRUE, center=TRUE, scale=TRUE)

##Lets look at the explaine variance by all the PCAs calculated
##expl.var_speclib<- round(AV_PCAspeclib$sdev^2/sum(AV_PCAspeclib$sdev^2)*100)####Firts 4 PCs best explains the variability
##expl.var_image  <- round(AV_PCAimage  $sdev^2/sum(AV_PCAimage$sdev^2)*100)###Firts 7 PCs best explains the variability

##Lets use 7 PC's
##Creates dataframe with first 7 PCA vales for each species
AV_PCAspeclib<-cbind(as.data.frame(alaskaSpecLib_AV_equal21[1:7]),as.data.frame(AV_PCAspeclib  $x[,1:7]))
AV_PCAimage  <-cbind(as.data.frame(Clayton_test_AV[1:2])         ,as.data.frame(AV_PCAimage    $x[,1:7]))


##Lets save these new PCA dataframes
write.csv(AV_PCAspeclib,"Outputs/3_AV_Imagery/1_Processing/AV_PCAspeclib.csv",row.names = FALSE)
write.csv(AV_PCAimage  ,"Outputs/3_AV_Imagery/1_Processing/AV_PCAimage.csv"  ,row.names = FALSE)
