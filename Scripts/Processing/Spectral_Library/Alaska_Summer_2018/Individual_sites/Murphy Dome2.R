library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Murph2_spectra<-read_spectra("original_data/Alaska_Summer_2018/20180729/murphydomeday2",
                     format="sed")

##Remove polstricum scans
Murph2_spectra<-Murph2_spectra[grep("polstricum",invert=TRUE,names(Murph2_spectra))]
Murph2_spectra<-Murph2_spectra[grep("poljplesch",invert=TRUE,names(Murph2_spectra))]
Murph2_spectra<-Murph2_spectra[grep("russulared",invert=TRUE,names(Murph2_spectra))]

#######Fix Names
names(Murph2_spectra)<-gsub(".sed","",names(Murph2_spectra))
names(Murph2_spectra)<-gsub("_00","_Murp2_",names(Murph2_spectra))
names(Murph2_spectra)<-gsub("wet","_wet",names(Murph2_spectra))
names(Murph2_spectra)<-gsub("trapeliopsisgranulosa","tragra",names(Murph2_spectra))

##Create metadata
Murph2_spectra_metadata<-as.data.frame(names(Murph2_spectra))
names(Murph2_spectra_metadata)[1]<-"ScanID"

###Create column PFT and column area
Murph2_spectra_metadata<-Murph2_spectra_metadata%>%mutate(PFT= substr(Murph2_spectra_metadata$ScanID,start=1,stop = 6))
Murph2_spectra_metadata$PFT[Murph2_spectra_metadata$PFT=="bryori"] <- "bryoria"
Murph2_spectra_metadata$PFT[Murph2_spectra_metadata$PFT=="dicran"] <- "dicranum"
Murph2_spectra_metadata$PFT[Murph2_spectra_metadata$PFT=="melane"] <- "melanelia"
Murph2_spectra_metadata$area<- "Murphy"

###Set metadata
meta(Murph2_spectra) = data.frame(Murph2_spectra_metadata, stringsAsFactors = FALSE)

## Smooth spectra
Murph2_spectra_smooth = smooth(Murph2_spectra)

## Trim ends and resample every 00005nm, 0010nm, 0050nm and 100nm (Raw)
Murph2_spectra_005nm = resample(Murph2_spectra, seq(350, 2500, 5))
Murph2_spectra_010nm = resample(Murph2_spectra, seq(350, 2500, 10))
Murph2_spectra_050nm = resample(Murph2_spectra, seq(350, 2500, 50))
Murph2_spectra_100nm = resample(Murph2_spectra, seq(350, 2500, 100))

## Trim ends and resample every 00005nm, 0010nm, 0050nm and 100nm (Smooth)
Murph2_spectra_smooth_005nm = resample(Murph2_spectra_smooth, seq(350, 2500, 5))
Murph2_spectra_smooth_010nm = resample(Murph2_spectra_smooth, seq(350, 2500, 10))
Murph2_spectra_smooth_050nm = resample(Murph2_spectra_smooth, seq(350, 2500, 50))
Murph2_spectra_smooth_100nm = resample(Murph2_spectra_smooth, seq(350, 2500, 100))

###save spectra (Raw)
saveRDS(Murph2_spectra      ,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra.rds")
saveRDS(Murph2_spectra_005nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_005nm.rds")
saveRDS(Murph2_spectra_010nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_010nm.rds")
saveRDS(Murph2_spectra_050nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_050nm.rds")
saveRDS(Murph2_spectra_100nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_100nm.rds")

###save spectra (Smooth)
saveRDS(Murph2_spectra_smooth      ,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth.rds")
saveRDS(Murph2_spectra_smooth_005nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth_005nm.rds")
saveRDS(Murph2_spectra_smooth_010nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth_010nm.rds")
saveRDS(Murph2_spectra_smooth_050nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth_050nm.rds")
saveRDS(Murph2_spectra_smooth_100nm,"Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth_100nm.rds")