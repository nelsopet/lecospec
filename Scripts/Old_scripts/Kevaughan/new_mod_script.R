library(randomForest)
library(tidyverse)

##Creates objcet that stores file paths to be used (input and output folders)
#Input
Raw_spec   <-"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019"
Smooth_spec<-"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019"

#Output
output_raw   <-"Processed_spec/Error_rates/Raw"
output_smooth<-"Processed_spec/Error_rates/Smooth"

##Creates a list of all filenames for the different sets of predictors that are raw spectra (allbands,5nm,10nm,50nm,100nm)
alaskaspeclib_raw_files_allba<-list.files(path = Raw_spec, pattern = "alaskaSpecLib_plants"      , full.names = T)
alaskaspeclib_raw_files_005nm<-list.files(path = Raw_spec, pattern = "alaskaSpecLib_005nm_plants", full.names = T)
alaskaspeclib_raw_files_010nm<-list.files(path = Raw_spec, pattern = "alaskaSpecLib_010nm_plants", full.names = T)
alaskaspeclib_raw_files_050nm<-list.files(path = Raw_spec, pattern = "alaskaSpecLib_050nm_plants", full.names = T)
alaskaspeclib_raw_files_100nm<-list.files(path = Raw_spec, pattern = "alaskaSpecLib_100nm_plants", full.names = T)

##Reads in all dataframes for the different sets of predictors that are raw spectra (allbands,5nm,10nm,50nm,100nm)
alaskaspeclib_raw_allba<-lapply(alaskaspeclib_raw_files_allba, read.csv)
alaskaspeclib_raw_005nm<-lapply(alaskaspeclib_raw_files_005nm, read.csv)
alaskaspeclib_raw_010nm<-lapply(alaskaspeclib_raw_files_010nm, read.csv)
alaskaspeclib_raw_050nm<-lapply(alaskaspeclib_raw_files_050nm, read.csv)
alaskaspeclib_raw_100nm<-lapply(alaskaspeclib_raw_files_100nm, read.csv)

##Creates a list of all the dataframes (list within a list)
Raw_spectra<-list(alaskaspeclib_raw_allba
              ,alaskaspeclib_raw_005nm
              ,alaskaspeclib_raw_010nm
              ,alaskaspeclib_raw_050nm
              ,alaskaspeclib_raw_100nm)


###Create several functions to manipulate list of dataframes
##Multifunc<-function(x){

##removes unwanted metadata
remove_unwanted<-function(x){ 
  x[c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL; x}

###converts column PFT_2 to factor
PFT_2_to_fac<-function(x){
  as.factor(x[,"PFT_2"])}

rf_mod<-function(x){
  randomForest(PFT_2~.,data=(x),mtry=5,ntree=2001,importance=TRUE)}#Randomforest model

Raw_spectra<-lapply(Raw_spectra,lapply,PFT_2_to_fac)
Raw_spectra<-lapply(Raw_spectra,lapply,remove_unwanted)




##Manipulating Random Forest model
species_err<-function(x){
  x[,"confusion"]%>%as.data.frame()%>%select(class.error)}##Create data frame showing percent error for each species

indextofirst<-function(x){
  cbind(rownames(x),x)%>%`rownames<-`(seq_len(nrow(x)))##Change Index column to first column
}


change_colname<-function(x){
  names(x)[2]<-"???"
}


names(species_error_equal05)[2]<-"raw_allbands_equal05"
subset(species_error_equal05,raw_allbands_equal05<0.3)
ModelOOB_allbands_raw<-Reduce(function(x,y) merge(x,y,by="Category",all=TRUE) ,list(Overall_error_more05
                                                                                    ,Overall_error_more10
                                                                                    ,Overall_error_more15
                                                                                    ,Overall_error_more20
                                                                                    ,Overall_error_equal05
                                                                                    ,Overall_error_equal10
                                                                                    ,Overall_error_equal15
                                                                                    ,Overall_error_equal20
                                                                                    ,PCA_Overall_error_equal05
                                                                                    ,PCA_Overall_error_equal10
                                                                                    ,PCA_Overall_error_equal15
                                                                                    ,PCA_Overall_error_equal20))
write.csv()



