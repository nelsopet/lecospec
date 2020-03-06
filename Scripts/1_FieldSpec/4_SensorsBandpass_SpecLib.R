##Creates a spectral library from spectroradiometeric scans based on bandpasses of different sensors
##Need to run scripts 1 and 2 in folder "1_Field_spec_processing/2_AK_SpecLib" prior this script if the data is not present in the output folder below
#" Outputs/Field_spec/Processing/PSR/"
library(spectrolab)
library(tidyverse)

# Reads in spectral library as a spectral object
# This is the spectral library that had all uncalibrated scans removed
# Even distribution of species within each functional group applied
alaskaSpeclib<-readRDS("Outputs/Field_spec/Processing/PSR/alaskaSpecLib_reduced.rds")

# Import names of bandpasses into character list
names_bandpasses = list.files("Original_data/Sensors/", pattern="bandpass",full.names = T) 

# Reads in bandpasses from different sensors
sensor_Bandpasses<-lapply(names_bandpasses,function(x){
  scan(x,numeric())
})%>% 
  
  # Removes dir path from the name
  setNames(gsub("Original_data/Sensors/","",names_bandpasses)) 



# Resamples alsakSpeclib based on the bandpasses of different sensors 
list_fieldSpec_resamp<-Map(x=replicate(length(sensor_Bandpasses),alaskaSpeclib,simplify = F)
                           , y=sensor_Bandpasses,function(x,y){
  
  # Resamples alsakSpeclib based on the bandpasses
  Resamp<-spectrolab::resample(x,y)
  
  # Converts Spectral library to a dataframe
  Df_convert<-Resamp%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  
  # Removes bad scans (Scans with reflectance values >2 or <0)
  goodscans<-Df_convert%>%
    filter_at(vars(-(ScanID:PFT4_Freq)), all_vars(. <2))%>%
    filter_at(vars(-(ScanID:PFT4_Freq)), all_vars(. >=0))
  return(goodscans)
  
})%>% 
  
  # Renames objects
  setNames(gsub("Original_data/Sensors/","",names_bandpasses))

# Writes out each dataframe as a .csv file
lapply(1:length(list_fieldSpec_resamp), function (x) 
  write.csv(list_fieldSpec_resamp[[x]],
            file = paste('Outputs/Field_spec/Processing/Sensors/',
                         gsub("_bandpass","_SpecLib",names (list_fieldSpec_resamp[x])),
                         '.csv',sep=""), row.names = F))


