library(spectrolab)
library(tidyverse)

process.spectra <- function(filepath, name){
  # Load the data
  df <- read_spectra(filepath, format="sed")
  
  # correct names
  names(df)<-gsub(".sed","",names(df))
  
  # save metadata
  df_metadata<-as.data.frame(names(df))
  names(df_metadata)[1]<-"ScanID"
  
  # Tweak metadata
  df_metadata<-df_metadata%>%mutate(Code_name=substr(df_metadata$ScanID,start = 1,stop = 6))
  df_metadata$Area<- name
  
  # update metadata on main data.frame
  meta(df) = data.frame(df_metadata, stringsAsFactors = FALSE)
  
  return(df)
}

files <- c("Data/SpectraByLocation/12_mile/original_samples", 
           )

names <- c("12mile", 
           )

save_paths <- c()
