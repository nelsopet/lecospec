library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
PEF_spectra<-read_spectra("Original_data/Field_spec/Maine/PEF_Scans",
                              format="sed")
##Fix Names by removing file extensions
names(PEF_spectra)<-gsub(".sed","",names(PEF_spectra))

###Create column PFT and column area
PEF_metadata<-as.data.frame(names(PEF_spectra))
PEF_metadata<-PEF_metadata%>%mutate(PFT= substr(PEF_metadata$`names(PEF_spectra)`,start=5,stop=10))

#Change column 1 name to scanID AND SET THE AREA
names(PEF_metadata)[1]<-"ScanID"
PEF_metadata$area<- "PEF"

###Set metadata
meta(PEF_spectra) = data.frame(PEF_metadata, stringsAsFactors = FALSE)

#save spectral object
saveRDS(PEF_spectra      ,"Outputs/1_Field_spec/1_Processing/Maine/PEF_spectra.rds")
