###Eagle Summit###
###BigTrail_lake####
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Eagle_summit_spectra<-read_spectra("Original_data/Field_spec/Alaska/Eagle_summit/original_samples",
                                     format="sed")

##Fix Names 
names(Eagle_summit_spectra)<-gsub(".sed","",names(Eagle_summit_spectra))

###Create Metadata
Eagle_summit_metadata<-as.data.frame(names(Eagle_summit_spectra))
names(Eagle_summit_metadata)[1]<-"ScanID"

###Create column PFT and column area
Eagle_summit_metadata<-Eagle_summit_metadata%>%mutate(PFT=substr(Eagle_summit_metadata$ScanID,start = 1,stop = 6))
Eagle_summit_metadata$area<- "Big Trail"

##Set metadata
meta(Eagle_summit_spectra) = data.frame(Eagle_summit_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Eagle_summit_spectra      ,"Outputs/1_Field_spec/1_Processing/Eagle_summit_spectra.rds"      )
