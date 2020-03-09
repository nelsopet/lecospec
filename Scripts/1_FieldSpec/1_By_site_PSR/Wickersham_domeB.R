########################################Wickersham dome B#############################
setwd("/Alaska_Spectral_Library")

####Read in data as spectra
Wickersham_domeB_spectra<-read_spectra("Original_data/Field_spec/Wickersham_domeB/original_samples",
                                       format="sed")

##Fix Names 
names(Wickersham_domeB_spectra)<-gsub(".sed","",names(Wickersham_domeB_spectra))

###Create Metadata
Wickersham_domeB_metadata<-as.data.frame(names(Wickersham_domeB_spectra))
names(Wickersham_domeB_metadata)[1]<-"ScanID"

###Create column PFT and column Area
Wickersham_domeB_metadata<-Wickersham_domeB_metadata%>%mutate(PFT=substr(Wickersham_domeB_metadata$ScanID,start = 1,stop = 6))
Wickersham_domeB_metadata$Area<- "Wickersham B"

##Set metadata
meta(Wickersham_domeB_spectra) = data.frame(Wickersham_domeB_metadata, stringsAsFactors = FALSE)

##save spectra (Raw)
saveRDS(Wickersham_domeB_spectra      ,"OutputsPSR/Processing/PSR/Wickersham_domeB_spectra.rds")
