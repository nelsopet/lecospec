########################################BigTrail_lake####################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Big_Trail_Lake_spectra<-read_spectra("Original_data/Field_spec/Alaska/Big_Trail_Lake/original_samples",
                                 format="sed")

##Fix Names 
names(Big_Trail_Lake_spectra)<-gsub(".sed","",names(Big_Trail_Lake_spectra))

###Create Metadata
Big_Trail_Lake_metadata<-as.data.frame(names(Big_Trail_Lake_spectra))
names(Big_Trail_Lake_metadata)[1]<-"ScanID"

###Create column PFT and column area
Big_Trail_Lake_metadata<-Big_Trail_Lake_metadata%>%mutate(PFT=substr(Big_Trail_Lake_metadata$ScanID,start = 1,stop = 6))
Big_Trail_Lake_metadata$area<- "Big Trail"

##Set metadata
meta(Big_Trail_Lake_spectra) = data.frame(Big_Trail_Lake_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Big_Trail_Lake_spectra      ,"Outputs/1_Field_spec/1_Processing/Big_Trail_Lake_spectra.rds"      )
