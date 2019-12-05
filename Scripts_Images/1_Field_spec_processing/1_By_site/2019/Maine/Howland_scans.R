library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Howland_spectra<-read_spectra("Original_data/Field_spec/Maine/Howland_Scans",
                             format="sed")
##Fix Names by removing file extensions
names(Howland_spectra)<-gsub(".sed","",names(Howland_spectra))

###Create column PFT and column area
Howland_metadata<-as.data.frame(names(Howland_spectra))
Howland_metadata<-Howland_metadata%>%mutate(PFT= substr(Howland_metadata$`names(Howland_spectra)`,start=5,stop=10))

#Change column 1 name to scanID AND SET THE AREA
names(Howland_metadata)[1]<-"ScanID"
Howland_metadata$area<- "Howland"

###Set metadata
meta(Howland_spectra) = data.frame(Howland_metadata, stringsAsFactors = FALSE)

#save spectral object
saveRDS(Howland_spectra      ,"Outputs/1_Field_spec/1_Processing/Maine/Howland_spectra.rds")
