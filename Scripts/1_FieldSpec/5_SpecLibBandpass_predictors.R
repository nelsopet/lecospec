## Calculates reflectance values for bands resampled every 5, 10 , 50 and 100nm (Only headwall sensor)
## Calculates the vegitation index
## Need to run scripts in 1_By_site_PSR, 2 and 3 in folder "1_FieldSpec/" prior this script if the data is not present in the output folder below
## 'OutputsPSR/Processing/Sensors/'
## At the end of the script memeroy will be cleaned (to stop this put a # infront the last line of code)
library(spectrolab)
library(tidyverse)
library(hsdar)

# Lets create names for output and input folders
# Input folder is the dir path to where yor data is stored
# output folder is the dir path to where you want your processed data to be stored
# Replace these before running 
outputs_folder<-"OutputsPSR/Processing/Sensors/"
input_folder  <-"OutputsPSR/Processing/Sensors/"

# Import names of Spectral libraries into a list
# For now we'll work with headwall and AVIRIS
names_SpecLibs = list.files(input_folder, 
                            pattern="SpecLib",full.names = T)[1:2] 

# Reads in spectral library for each sensor
Sensor_SpecLibs<-lapply(names_SpecLibs,function(x){
  read.csv(x,check.names = F)})%>% 
  
  # Removes dir path from the name
  setNames(gsub(input_folder,"",names_SpecLibs)) 

# Lets create functions that will remove all the metatdata and one that will keep the data for bandspasses
# These function will be used alot
# Returns columns that are bandpasses
metaRemove<-function(x){
  meta<-c(grep("^[0-9][0-9][0-9]",colnames(x)))
  colremove<-x[,meta]
  return(colremove)
}# Function ends

# Returns columns that are not bandpasses
bandsRemove<-function(x){
  meta<-c(grep("[a-z A-Z]",colnames(x)))
  colremove<-x[,meta]
  return(colremove)
}# Function ends]2

# Function resamples a spectral library every 5, 10, 50 and 100 nm and combines those results into a dataframe
# Resampling will change reflectance values which might result in negative values (need to remove these rows)
Func_Resamp<-function(Resamp){
  
  # Function adds  suffix to columns in dataframes
  # This is a uniqe way to identify columns when merging these dataframes later 
  add_suffix<-function(x){
    colnames(x)<-paste(colnames(x),deparse(substitute(x)),sep = "")
    return(x)}
  
  # Removes metadata before function can be applied
  DF<-metaRemove(Resamp)
  
  # Function resamples spectral library every 5nm
  nm_5<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,5))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_5<-add_suffix(nm_5)
  
  # Function resamples spectral library every 10nm
  nm_10<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,10  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_10<-add_suffix(nm_10)
  
  # Function resamples spectral library every 50nm
  nm_50<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,50  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_50<-add_suffix(nm_50)
  
  # Function resamples spectral library every 100nm
  nm_100<-DF%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,100  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_100<-add_suffix(nm_100)
  
  # Combines 5nm, 10nm 50nm and 100nm dataframes and add metadata
  df<-Reduce(cbind,list(bandsRemove(Resamp)
                        ,nm_5
                        ,nm_10
                        ,nm_50
                        ,nm_100))
  # Removes rows with negative values or values >2
  goodValues<-df%>%
    filter_at(vars(c(colnames(metaRemove(df)))), all_vars(.  <2))%>%
    filter_at(vars(c(colnames(metaRemove(df)))), all_vars(. >=0))
 
    
  return(goodValues)
  
}# Function ends

# Test your data below
# test_case<-Func_Resamp(Sensor_SpecLibs[[2]])

# Function calculates vegitation indices
Func_VI<-function(VI){
  
  # Converts dataframe to matrix before VIs can be applied
  matrix_a<-as.matrix(metaRemove(VI))
  
  # Creates numeric vector of wavelengths
  bat<-metaRemove(VI)%>%
  colnames()%>%
  as.numeric()
  
  # Creates a spectralib object
  spec_library<- speclib(matrix_a,bat)
  
  # creates a vectror of names of all the vegitation indices
  AVIRIS_VI  <-vegindex()[-58]
  Headwall_VI<-vegindex()[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
  
  # Creates dataframe with Vegitation indices
  VI_CALC<-if(ncol(metaRemove(VI)) == 272){
    vegindex(spec_library,index = Headwall_VI)
  } else {
    vegindex(spec_library,index = AVIRIS_VI)}
  
  # Function removes spaces and special charcters from column names
  # Models will not run if these aren't removed
  names(VI_CALC)<-str_remove_all(names(VI_CALC),"[[:punct:]]| ")
    
  return(cbind(bandsRemove(VI),VI_CALC))
}# Function ends 

# Test your data below
# test_case2<-Func_VI(Sensor_SpecLibs[[2]])

# Creates Predictors for model prediction
Func_preds<-lapply(Sensor_SpecLibs,function(x){
  preds<-if (ncol(metaRemove(x)) == 272){
    cbind(Func_VI(x),
          Func_Resamp(metaRemove(x)))
  }else{
    Func_VI(x)
  }
  
  # Removes Infs and NA values from dataframe 
  Df_preds<-preds[Reduce(`&`,lapply(preds,function(x)
    !is.na(x) & is.finite(x))),]
  return(Df_preds)
})# datframes created

# Writes out each dataframe as a .csv file
lapply(1:length(Func_preds), function (x) 
  write.csv(Func_preds[[x]],
            file = paste("OutputsPSR/Processing/Sensors/",
                         gsub("_SpecLib.csv","_PredsDF",names (Func_preds[x])),
                         '.csv',sep=""), row.names = F))

# Cleans up R memeory
rm(list=ls())




