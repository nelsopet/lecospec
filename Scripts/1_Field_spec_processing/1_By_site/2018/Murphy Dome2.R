library(spectrolab)
library(tidyverse)

####Read in data as spectra
Murph2_spectra<-read_spectra("Original_data/Field_spec/Alaska/20180729/murphydomeday2",
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

###save spectra (Raw)
saveRDS(Murph2_spectra      ,"Outputs/1_Field_spec/1_Processing/Murph2_spectra.rds")


