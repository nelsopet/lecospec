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
alaskaspeclib_raw_files_100nm<-list.files(path = Raw_spec, pattern = "alaskaSpecLib_100nm_plants", full.names = T)

##Reads in all dataframes for the different sets of predictors that are raw spectra (allbands,5nm,10nm,50nm,100nm)
alaskaspeclib_raw_100nm<-lapply(alaskaspeclib_raw_files_100nm, read.csv)
names(alaskaspeclib_raw_100nm)<-alaskaspeclib_raw_files_100nm

##removes unwanted metadata
alaskaspeclib_raw_100nm<-lapply(alaskaspeclib_raw_100nm,function(x){ 
  x[c("ScanID","PFT","PFT_3","area","Freq","X")] = NULL
  x
  })

##Randomforest model
Model= lapply(alaskaspeclib_raw_100nm,function(x){
  randomForest(PFT_2~.,data= x,mtry=5,ntree=2001,importance=TRUE)
})

##Creates df showing percent error for each species 
Species_error<-lapply(Model,function(x){
  x[["confusion"]]%>%
    as.data.frame %>%
    select(class.error) %>%
    rename(Raw_100nm=class.error)
})

