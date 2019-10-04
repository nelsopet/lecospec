########################################BigTrail_lake####################################
library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Big_Trail_Lake_spectra<-read_spectra("Original_data/Alaska_Summer_2019/Big_Trail_Lake/original_samples",
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

## Smooth spectra
Big_Trail_Lake_spectra_smooth = smooth(Big_Trail_Lake_spectra)

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Raw)
Big_Trail_Lake_spectra_005nm = resample(Big_Trail_Lake_spectra, seq(350, 2500, 5))
Big_Trail_Lake_spectra_010nm = resample(Big_Trail_Lake_spectra, seq(350, 2500, 10))
Big_Trail_Lake_spectra_050nm = resample(Big_Trail_Lake_spectra, seq(350, 2500, 50))
Big_Trail_Lake_spectra_100nm = resample(Big_Trail_Lake_spectra, seq(350, 2500, 100))

## Trim ends and resample every 005nm,010nm,050nm and 100nm (Smooth)
Big_Trail_Lake_spectra_smooth_005nm = resample(Big_Trail_Lake_spectra_smooth, seq(350, 2500, 5))
Big_Trail_Lake_spectra_smooth_010nm = resample(Big_Trail_Lake_spectra_smooth, seq(350, 2500, 10))
Big_Trail_Lake_spectra_smooth_050nm = resample(Big_Trail_Lake_spectra_smooth, seq(350, 2500, 50))
Big_Trail_Lake_spectra_smooth_100nm = resample(Big_Trail_Lake_spectra_smooth, seq(350, 2500, 100))

##save spectra (Raw)
saveRDS(Big_Trail_Lake_spectra      ,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra.rds")
saveRDS(Big_Trail_Lake_spectra_005nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_005nm.rds")
saveRDS(Big_Trail_Lake_spectra_010nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_010nm.rds")
saveRDS(Big_Trail_Lake_spectra_050nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_050nm.rds")
saveRDS(Big_Trail_Lake_spectra_100nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_100nm.rds")

##save spectra (Smooth)
saveRDS(Big_Trail_Lake_spectra_smooth      ,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth.rds")
saveRDS(Big_Trail_Lake_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth_005nm.rds")
saveRDS(Big_Trail_Lake_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth_010nm.rds")
saveRDS(Big_Trail_Lake_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth_050nm.rds")
saveRDS(Big_Trail_Lake_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth_100nm.rds")
