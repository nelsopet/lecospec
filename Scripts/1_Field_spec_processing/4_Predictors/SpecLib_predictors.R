## Calculates reflectance values for bands resampled every 5, 10 , 50 and 100nm (Only headwall sensor)
## Calculates the vegitation index
## Need to run scripts 1, 2 and 3 in folder "1_Field_spec_processing/2_AK_SpecLib" prior this script if the data is not present in the output folder below
#'Outputs/Field_spec/Processing/Sensors/'
library(spectrolab)
library(tidyverse)
library(hsdar)

# Import names of Spectral libraries into a list
# For now we'll work with headwall and AVIRIS
names_SpecLibs = list.files("Outputs/Field_spec/Processing/Sensors/", pattern="SpecLib",full.names = T)[1:2] 

# Reads in spectral library for each sensor
SpecLibs<-lapply(names_SpecLibs,function(x){
  read.csv(x,check.names = F)})%>% 
  # Removes dir path from the name
  setNames(gsub("Outputs/Field_spec/Processing/Sensors/","",names_SpecLibs)) 

#Lets create a function that will resample a dataframe every 5, 10, 50 and 100 nm and combine those results

Func_Resamp<-function(Resamp){
  
  # Function adds  suffix to columns in dataframes
  add_suffix<-function(x){
    colnames(x)<-paste(colnames(x),deparse(substitute(x)),sep = "")
    return(x)}
  
  # Function resamples spectral library every 5nm
  nm_5<-Resamp%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,5))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_5<-add_suffix(nm_5)
  
  # Function resamples spectral library every 10nm
  nm_10<-Resamp%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,10  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_10<-add_suffix(nm_10)
  
  # Function resamples spectral library every 50nm
  nm_50<-Resamp%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,50  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_50<-add_suffix(nm_50)
  
  # Function resamples spectral library every 100nm
  nm_100<-Resamp%>%
    as.spectra()%>%
    spectrolab::resample(seq(397.593,899.424,100  ))%>%
    as.data.frame()%>%
    dplyr::select(-sample_name)
  nm_100<-add_suffix(nm_100)
  
  df<-Reduce(cbind,list(nm_5
                       ,nm_10
                       ,nm_50
                       ,nm_100))
  return(df)
  
}

#Function calculates vegitation indices
Func_VI<-function(VI){
  
  # Converts dataframe to matrix before VIs can be applied
  matrix_a<-as.matrix(VI[-1:-9])
  
  # Creates numeric vector of wavelengths
  bat<-VI[-1:-9]%>%
  colnames()%>%
  as.numeric()
  
  # Creates a spectralib object
  spec_library<- speclib(matrix_a,bat)
  
  # creates a vectror of names of all the vegitation indices
  AVIRIS_VI  <-vegindex()[-58]
  Headwall_VI<-vegindex()[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]
  
  # Creates dataframe with Vegitation indices
  VI_CALC<-if(ncol(VI) == 281){
    vegindex(spec_library,index = Headwall_VI)
  } else {
    vegindex(spec_library,index = AVIRIS_VI)}
  return(cbind(VI[1:9],VI_CALC))
}



# Function creates Predictors for model prediction
Func_preds<-lapply(SpecLibs,function(x){
  preds<-if (ncol(x) == 281){
    cbind(Func_VI(x),
          Func_Resamp(x[-1:-9]))
  }else{
    Func_VI(x)
    }
})

# Writes out each dataframe as a .csv file
lapply(1:length(Func_preds), function (x) 
  write.csv(Func_preds[[x]],
            file = paste('Outputs/Field_spec/Processing/Sensors/',
                         gsub("_SpecLib.csv","_PredsDF",names (Func_preds[x])),
                         '.csv',sep=""), row.names = F))







