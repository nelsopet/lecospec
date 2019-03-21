library(spectrolab)
library(tidyverse)

####Read in data as spectra
brooksLib_spectra<-read_spectra("/Alaska_Spectral_Library/data/BrooksLib",
                                format="sed")

##Remove eightmileflight1 scans
brooksLib_spectra<-brooksLib_spectra[grep("orangeporpidia",invert=TRUE,names(brooksLib_spectra))]


#######Fix Names
names(brooksLib_spectra)<-gsub(".sed","",names(brooksLib_spectra))
names(brooksLib_spectra)<-gsub("_00","_Brooks",names(brooksLib_spectra))
names(brooksLib_spectra)<-gsub("2_","_",names(brooksLib_spectra))

###Create Metadata
brooksLib_metadata<-as.data.frame(names(brooksLib_spectra))
names(brooksLib_metadata)[1]<-"ScanID"

###Create column PFT and column area
brooksLib_metadata<-brooksLib_metadata%>%mutate(PFT= sub("_.*", "",brooksLib_metadata$ScanID))
brooksLib_metadata$area<- "Brooks_Range"

####ensure all scans are different
brooksLib_metadata<-brooksLib_metadata %>%
  group_by(PFT, area) %>%
  mutate(
    ScanID = as.character(ScanID),
    ScanID = as.character(paste0(substr(ScanID, 1, nchar(ScanID) - 1), row_number())))

##Set metadata
meta(brooksLib_spectra) = data.frame(brooksLib_metadata, stringsAsFactors = FALSE)

##save spectra
saveRDS(brooksLib_spectra,"/Alaska_Spectral_Library/processed spec/brooksLib_spectra.rds")

####test<-brooksLib %>% group_by(PFT, Band) %>% ggplot(aes(Band,Refl))+geom_line(aes(color=PFT))
##test+facet_wrap(facets= vars(PFT), nrow=10, ncol=5)
