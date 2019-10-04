###Eagle Summit###
###BigTrail_lake####
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Eagle_summit_spectra<-read_spectra("Original_data/Alaska_Summer_2019/Eagle_summit/original_samples",
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

## Smooth spectra
Eagle_summit_spectra_smooth = smooth(Eagle_summit_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Raw)
Eagle_summit_spectra_005nm = resample(Eagle_summit_spectra, seq(350, 2500, 5))
Eagle_summit_spectra_010nm = resample(Eagle_summit_spectra, seq(350, 2500, 10))
Eagle_summit_spectra_050nm = resample(Eagle_summit_spectra, seq(350, 2500, 50))
Eagle_summit_spectra_100nm = resample(Eagle_summit_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth)
Eagle_summit_spectra_smooth_005nm = resample(Eagle_summit_spectra_smooth, seq(350, 2500, 5))
Eagle_summit_spectra_smooth_010nm = resample(Eagle_summit_spectra_smooth, seq(350, 2500, 10))
Eagle_summit_spectra_smooth_050nm = resample(Eagle_summit_spectra_smooth, seq(350, 2500, 50))
Eagle_summit_spectra_smooth_100nm = resample(Eagle_summit_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(Eagle_summit_spectra      ,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra.rds")
saveRDS(Eagle_summit_spectra_005nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_005nm.rds")
saveRDS(Eagle_summit_spectra_010nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_010nm.rds")
saveRDS(Eagle_summit_spectra_050nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_050nm.rds")
saveRDS(Eagle_summit_spectra_100nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(Eagle_summit_spectra_smooth      ,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth.rds")
saveRDS(Eagle_summit_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth_005nm.rds")
saveRDS(Eagle_summit_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth_010nm.rds")
saveRDS(Eagle_summit_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth_050nm.rds")
saveRDS(Eagle_summit_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth_100nm.rds")

