#Create cleaned spectral library from the PFT that matches the spectral library
source("Functions/lecospectR.R")
PFT_ADJ<-rjson::fromJSON(file = "./assets/pft_adj_list.json")
Species_groups<-read.csv("./Data/SpeciesTable_20220125.csv", encoding = 'UTF-8')
PFT_IMG_SPEC<- read.csv("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_smooth.csv")

#Clean out all values not in key or add new value to key

PFT_IMG_SPEC$PFT<-gsub("Spirea","ShrubDecidOther", PFT_IMG_SPEC$PFT) #%>% unique()
PFT_IMG_SPEC$PFT<-gsub("Gravel","gravel",PFT_IMG_SPEC$PFT) #%>% unique()
PFT_IMG_SPEC$PFT<-gsub("Thamnolia", "LightTerrestrialMacrolichen",PFT_IMG_SPEC$PFT) #%>% unique()
PFT_IMG_SPEC$PFT<-gsub("Cassopie_tetragona", "Cassiope_tetragona", PFT_IMG_SPEC$PFT) #%>% unique()
PFT_IMG_SPEC$PFT<-gsub("_"," ",PFT_IMG_SPEC$PFT)#%>% unique()
PFT_IMG_SPEC$PFT<-gsub("EvergreenShrub", "ShrubEvergreen",PFT_IMG_SPEC$PFT)#%>% unique()
PFT_IMG_SPEC$PFT<-gsub("Vaccinum uliginosum","Vaccinium uliginosum",PFT_IMG_SPEC$PFT)
PFT_IMG_SPEC$PFT<-gsub("Picea bark", "spruce bark",PFT_IMG_SPEC$PFT)

#Make two lists to compare
list2<-names(PFT_ADJ)
list1<-unique(PFT_IMG_SPEC$PFT)

setdiff(list1,list2)

#Change aggregation level to Functional Group 1 and add to metadata
PFT_IMG_SPEC$FncGrp1<-change_aggregation(as.vector(PFT_IMG_SPEC$PFT), 1, PFT_ADJ)
  ##Make a site variable
RawUID<-PFT_IMG_SPEC %>% 
  dplyr::select(UID) %>% as.data.frame() #%>%
  #imgs_names<-

SiteNames<-str_split(RawUID[,1], "PFT") %>% as.data.frame() %>% t %>% as.data.frame() %>%dplyr::rename(Site = V1) %>% dplyr::select(Site)
rownames(SiteNames)<-NULL

PFT_IMG_SPEC$Site<-SiteNames
PFT_IMG_SPEC<-
PFT_IMG_SPEC %>% 
  dplyr::select(-PFT.1:-UID.3) %>% 
  #dplyr::select(sample_name:UID) %>%
  dplyr::select(UID, ScanNum, sample_name, PFT, FncGrp1, Site, everything()) %>%
  dplyr::select(-X) 

str(PFT_IMG_SPEC)
PFT_IMG_SPEC$Site<-PFT_IMG_SPEC$Site$Site
str(PFT_IMG_SPEC)


write.csv(PFT_IMG_SPEC, "./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv")
 
out_file <- "Output/"

#Make_Speclib_Derivs("./Data/Ground_Validation/PFT_Image_spectra/PFT_Image_SpectralLib_Clean.csv", out_file)

