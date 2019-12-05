###Murphy dome B####
###################################Murphy dome A#########################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Murphy_domeB_spectra<-read_spectra("Original_data/Field_spec/Alaska/Murphy_domeB/original_samples",
                           format="sed")

##Fix Names 
names(Murphy_domeB_spectra)<-gsub(".sed","",names(Murphy_domeB_spectra))

###Create Metadata
Murphy_domeB_metadata<-as.data.frame(names(Murphy_domeB_spectra))
names(Murphy_domeB_metadata)[1]<-"ScanID"

###Create column PFT and column area
Murphy_domeB_metadata<-Murphy_domeB_metadata%>%mutate(PFT=substr(Murphy_domeB_metadata$ScanID,start = 1,stop = 6))
Murphy_domeB_metadata$area<- "Murphy B"

##Set metadata
meta(Murphy_domeB_spectra) = data.frame(Murphy_domeB_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Murphy_domeB_spectra      ,"Outputs/1_Field_spec/1_Processing/Murphy_domeB_spectra.rds")
