###Murphy dome B####
###################################Murphy dome A#########################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Murphy_domeB_spectra<-read_spectra("Original_data/Alaska_Summer_2019/Murphy_domeB/original_samples",
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

## Smooth spectra
Murphy_domeB_spectra_smooth = smooth(Murphy_domeB_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Raw)
Murphy_domeB_spectra_005nm = resample(Murphy_domeB_spectra, seq(350, 2500, 5))
Murphy_domeB_spectra_010nm = resample(Murphy_domeB_spectra, seq(350, 2500, 10))
Murphy_domeB_spectra_050nm = resample(Murphy_domeB_spectra, seq(350, 2500, 50))
Murphy_domeB_spectra_100nm = resample(Murphy_domeB_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth)
Murphy_domeB_spectra_smooth_005nm = resample(Murphy_domeB_spectra_smooth, seq(350, 2500, 5))
Murphy_domeB_spectra_smooth_010nm = resample(Murphy_domeB_spectra_smooth, seq(350, 2500, 10))
Murphy_domeB_spectra_smooth_050nm = resample(Murphy_domeB_spectra_smooth, seq(350, 2500, 50))
Murphy_domeB_spectra_smooth_100nm = resample(Murphy_domeB_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(Murphy_domeB_spectra      ,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra.rds")
saveRDS(Murphy_domeB_spectra_005nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_005nm.rds")
saveRDS(Murphy_domeB_spectra_010nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_010nm.rds")
saveRDS(Murphy_domeB_spectra_050nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_050nm.rds")
saveRDS(Murphy_domeB_spectra_100nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(Murphy_domeB_spectra_smooth      ,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth.rds")
saveRDS(Murphy_domeB_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth_005nm.rds")
saveRDS(Murphy_domeB_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth_010nm.rds")
saveRDS(Murphy_domeB_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth_050nm.rds")
saveRDS(Murphy_domeB_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth_100nm.rds")