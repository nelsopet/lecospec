###################################Murphy dome A#########################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Murphy_domeA_spectra<-read_spectra("Original_data/Field_spec/Alaska/Murphy_domeA/original_samples",
                           format="sed")

##Fix Names 
names(Murphy_domeA_spectra)<-gsub(".sed","",names(Murphy_domeA_spectra))


###Create Metadata
Murphy_domeA_metadata<-as.data.frame(names(Murphy_domeA_spectra))
names(Murphy_domeA_metadata)[1]<-"ScanID"

###Create column PFT and column area
Murphy_domeA_metadata<-Murphy_domeA_metadata%>%mutate(PFT=substr(Murphy_domeA_metadata$ScanID,start = 1,stop = 6))
Murphy_domeA_metadata$area<- "Murphy A"

##Set metadata
meta(Murphy_domeA_spectra) = data.frame(Murphy_domeA_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Murphy_domeA_spectra      ,"Outputs/1_Field_spec/1_Processing/Murphy_domeA_spectra.rds")
