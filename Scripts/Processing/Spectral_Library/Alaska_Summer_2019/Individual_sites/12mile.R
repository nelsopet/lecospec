#########################################12mile#####################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
TwelveMile_spectra<-read_spectra("Original_data/Alaska_Summer_2019/12_mile/original_samples",
                                 format="sed")

##Fix Names 
names(TwelveMile_spectra)<-gsub(".sed","",names(TwelveMile_spectra))

###Create Metadata
TwelveMile_metadata<-as.data.frame(names(TwelveMile_spectra))
names(TwelveMile_metadata)[1]<-"ScanID"

###Create column PFT and column area
TwelveMile_metadata<-TwelveMile_metadata%>%mutate(PFT=substr(TwelveMile_metadata$ScanID,start = 1,stop = 6))
TwelveMile_metadata$area<- "12mile"

##Set metadata
meta(TwelveMile_spectra) = data.frame(TwelveMile_metadata, stringsAsFactors = FALSE)

## Smooth spectra
TwelveMile_spectra_smooth = smooth(TwelveMile_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Raw)
TwelveMile_spectra_005nm = resample(TwelveMile_spectra, seq(350, 2500, 5))
TwelveMile_spectra_010nm = resample(TwelveMile_spectra, seq(350, 2500, 10))
TwelveMile_spectra_050nm = resample(TwelveMile_spectra, seq(350, 2500, 50))
TwelveMile_spectra_100nm = resample(TwelveMile_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth)
TwelveMile_spectra_smooth_005nm = resample(TwelveMile_spectra_smooth, seq(350, 2500, 5))
TwelveMile_spectra_smooth_010nm = resample(TwelveMile_spectra_smooth, seq(350, 2500, 10))
TwelveMile_spectra_smooth_050nm = resample(TwelveMile_spectra_smooth, seq(350, 2500, 50))
TwelveMile_spectra_smooth_100nm = resample(TwelveMile_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(TwelveMile_spectra      ,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra.rds")
saveRDS(TwelveMile_spectra_005nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_005nm.rds")
saveRDS(TwelveMile_spectra_010nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_010nm.rds")
saveRDS(TwelveMile_spectra_050nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_050nm.rds")
saveRDS(TwelveMile_spectra_100nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(TwelveMile_spectra_smooth      ,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth.rds")
saveRDS(TwelveMile_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth_005nm.rds")
saveRDS(TwelveMile_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth_010nm.rds")
saveRDS(TwelveMile_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth_050nm.rds")
saveRDS(TwelveMile_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth_100nm.rds")
