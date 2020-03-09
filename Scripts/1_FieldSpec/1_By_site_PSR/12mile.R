#########################################12mile#####################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra (all scans collected at this location)
TwelveMile_spectra<-read_spectra("Original_data/Field_spec/12_mile/original_samples",
                                 format="sed")

##Plots all spectral objects withing this location (scans of individuals)
##plot_interactive(TwelveMile_spectra)

##Fix Names 
names(TwelveMile_spectra)<-gsub(".sed","",names(TwelveMile_spectra))

###Create Metadata
TwelveMile_metadata<-as.data.frame(names(TwelveMile_spectra))
names(TwelveMile_metadata)[1]<-"ScanID"

###Create column PFT and column Area
TwelveMile_metadata<-TwelveMile_metadata%>%mutate(PFT=substr(TwelveMile_metadata$ScanID,start = 1,stop = 6))
TwelveMile_metadata$Area<- "12mile"

##Set metadata
meta(TwelveMile_spectra) = data.frame(TwelveMile_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(TwelveMile_spectra      ,"OutputsPSR/Processing/PSR/TwelveMile_spectra.rds"      )

