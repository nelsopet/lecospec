library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Murph2_spectra<-read_spectra("original_data/20180729/murphydomeday2",
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

## Trim ends and resample every 5nm
Murph2_spectra_5nm = resample(Murph2_spectra, seq(350, 2500, 5))
Murph2_spectra_10nm = resample(Murph2_spectra, seq(350, 2500, 10))
Murph2_spectra_50nm = resample(Murph2_spectra, seq(350, 2500, 50))
Murph2_spectra_100nm = resample(Murph2_spectra, seq(350, 2500, 100))

###save spectra
saveRDS(Murph2_spectra,"processed spec/Murph2_Lib/Murph2_spectra.rds")
saveRDS(Murph2_spectra_5nm,"processed spec/Murph2_Lib/Murph2_spectra_5nm.rds")
saveRDS(Murph2_spectra_10nm,"processed spec/Murph2_Lib/Murph2_spectra_10nm.rds")
saveRDS(Murph2_spectra_50nm,"processed spec/Murph2_Lib/Murph2_spectra_50nm.rds")
saveRDS(Murph2_spectra_100nm,"processed spec/Murph2_Lib/Murph2_spectra_100nm.rds")
