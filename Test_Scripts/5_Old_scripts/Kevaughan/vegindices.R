library(spectrolab)
library(hsdar)
library(randomForest)
##reads in alaskaspeclib spectra ...spectra object of 1906 samples, 2151 bands, 6 metadata columns
alaskaSpecLib_rds<-readRDS("Processed_spec/Alaska_Spectral_Library/alaskaSpeclib.rds")
alaskaSpecLib_spec<-as.data.frame(alaskaSpecLib_rds)

alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1891'<2)    
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1892'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1893'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1894'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1895'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1896'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1897'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1898'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1899'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1900'<2) 
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1902'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1908'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1909'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1916'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1917'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1930'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1931'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1932'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1938'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1939'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1940'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1947'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1948'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1980'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1981'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1982'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1990'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1991'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1992'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1993'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1997'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1998'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'1999'<2)
alaskaSpecLib_spec<-subset(alaskaSpecLib_spec, alaskaSpecLib_spec$'2500'<2)

##Extract metadata to be merged later ... dataframe of 1901 rows and 7 columns
alaskaSpecLib_meta<-as.data.frame(alaskaSpecLib_spec)%>%dplyr::select(1:7)
str(alaskaSpecLib_meta)

###Creates a matrix containing all the columns indicating spectral nbands and rows of reflectances. ... matrix of 1901 rows and 2151 columns
alaskaSpecLib_spec<-(alaskaSpecLib_spec)[-1:-7]
str(alaskaSpecLib_spec)

##creates a vector of wavelengths ... a list of numbers 2151 entries long
alaskaSpecLib_wavelength<-alaskaSpecLib_rds$wavelengths
str(alaskaSpecLib_wavelength)

##convert alaskaSpeclib_spec to matrix...it has to be a matrix to be converted to a speclib object
alaskaSpecLib_spec<-as.matrix(alaskaSpecLib_spec)

##Creates a spectralib object ... 13 slots
alaskaSpecLib_lib<-speclib(alaskaSpecLib_spec,alaskaSpecLib_wavelength)
str(alaskaSpecLib_lib)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()

##alaskaSpecLib_mREIP<-vegindex(alaskaSpecLib_lib,index = "mREIP"        )%>%as.data.frame()%>%`colnames<-`(c("mREIP"        ))
##Vegitation indices mREIP won't work so remove it from list
VIs<-VIs[-58]

##Creates dataframe with Vegitation indices as variables ..dataframe of 1901 rows by 114 variables, each of which is a veg index
alaskaSpecLib_VIs<-vegindex(alaskaSpecLib_lib, index = VIs)

.ns
alaskaSpecLib_VIs_OLD<-vegindex(alaskaSpecLib_lib, index = VIs)

## check to see if NaNs/Inf exist ... NaNs produced in vegindex stuff ... check to see if input to vegindex has weird values anyways
lapply(alaskaSpecLib_VIs, range)

##REMOVE THOSE NaNs/Inf 
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(NDVI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(TCARI2)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(TCARI2/OSAVI2)==F)
names(alaskaSpecLib_VIs)[101]<-"SWIRFI"##had to remove space within this column name
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(SWIRFI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(SRPI)==F)

##Combines metadta with new variables to be used in models...use duplicated dataframe, this way we could do a join later
alaskaSpecLib_VIs_OLD<-cbind(alaskaSpecLib_meta,alaskaSpecLib_VIs_OLD)

##Join old VIs data frame with new VIs dataframe...this way all the rows will line up with the corrent species
alaskaSpecLib_VIs_NEW<-left_join(alaskaSpecLib_VIs,alaskaSpecLib_VIs_OLD)

##Reorder columns
alaskaSpecLib_VIs_NEW_PFT2<-alaskaSpecLib_VIs_NEW%>%dplyr::select(sample_name,ScanID,PFT,PFT_2,PFT_3,area,Freq,everything())
alaskaSpecLib_VIs_NEW_PFT3<-alaskaSpecLib_VIs_NEW%>%dplyr::select(sample_name,ScanID,PFT,PFT_2,PFT_3,area,Freq,everything())

##Remove last column since its repeated
alaskaSpecLib_VIs_NEW_PFT2[122]<-NULL
alaskaSpecLib_VIs_NEW_PFT3[122]<-NULL

##Change column names so they have no spaces or arrithmetic operators
colnames(alaskaSpecLib_VIs_NEW_PFT2)[8:121]<-c("Boochs"     ,   "Boochs2"   ,   "CAI"          , "CARI"   ,       "Carter"        ,"Carter2"  ,     "Carter3"   ,   
                                               "Carter4"    ,   "Carter5"   ,   "Carter6"      , "CI"     ,       "CI2"           ,"ClAInt"   ,     "CRI1"      ,   
                                               "CRI2"       ,   "CRI3"      ,   "CRI4"         , "D1"     ,       "D2"            ,"Datt"     ,     "Datt2"     ,   
                                               "Datt3"      ,   "Datt4"     ,   "Datt5"        , "Datt6"  ,       "Datt7"         ,"Datt8"    ,     "DD"        ,   
                                               "DDn"        ,   "DPI"       ,   "DWSI1"        , "DWSI2"  ,       "DWSI3"         ,"DWSI4"    ,     "DWSI5"     ,   
                                               "EGFN"       ,   "EGFR"      ,   "EVI"          , "GDVI_2" ,       "GDVI_3"        ,"GDVI_4"   ,     "GI"        ,   
                                               "Gitelson"   ,   "Gitelson2" ,   "GMI1"         , "GMI2"   ,       "GreenNDVI"     ,"LWVI1"    ,     "LWVI2"     ,   
                                               "Maccioni"   ,   "MCARI"     ,   "MCARIOSAVI"   , "MCARI2" ,       "MCARI2OSAVI2"  ,"mND705"   ,     "mNDVI"     ,   
                                               "MPRI"       ,   "MSAVI"     ,   "MSI"          , "mSR"    ,       "mSR2"          ,"mSR705"   ,     "MTCI"      ,   
                                               "MTVI"       ,   "NDLI"      ,   "NDNI"         , "NDVI"   ,       "NDVI2"         ,"NDVI3"    ,     "NDWI"      ,   
                                               "NPCI"       ,   "OSAVI"     ,   "OSAVI2"       , "PARS"   ,       "PRI"           ,"PRICI2"   ,     "PRI_norm"  ,   
                                               "PSND"       ,   "PSRI"      ,   "PSSR"         , "PWI"    ,       "RDVI"          ,"REP_LE"   ,     "REP_Li"    ,   
                                               "SAVI"       ,   "SIPI"      ,   "SPVI"         , "SR"     ,       "SR1"           ,"SR2"      ,     "SR3"       ,   
                                               "SR4"        ,   "SR5"       ,   "SR6"          , "SR7"    ,       "SR8"           ,"SRPI"     ,     "SRWI"      ,   
                                               "Sum_Dr1"    ,   "Sum_Dr2"   ,   "SWIRFI"       , "SWIRLI" ,       "SWIRSI"        ,"SWIRVI"   ,     "TCARI"     ,   
                                               "TCARIOSAVI" ,   "TCARI2"    ,   "TCARI2OSAVI2" , "TGI"    ,       "TVI"           ,"Vogelmann",     "Vogelmann2",   
                                               "Vogelmann3" ,   "Vogelmann4")

colnames(alaskaSpecLib_VIs_NEW_PFT3)[8:121]<-c("Boochs"     ,   "Boochs2"   ,   "CAI"          , "CARI"   ,       "Carter"        ,"Carter2"  ,     "Carter3"   ,   
                                               "Carter4"    ,   "Carter5"   ,   "Carter6"      , "CI"     ,       "CI2"           ,"ClAInt"   ,     "CRI1"      ,   
                                               "CRI2"       ,   "CRI3"      ,   "CRI4"         , "D1"     ,       "D2"            ,"Datt"     ,     "Datt2"     ,   
                                               "Datt3"      ,   "Datt4"     ,   "Datt5"        , "Datt6"  ,       "Datt7"         ,"Datt8"    ,     "DD"        ,   
                                               "DDn"        ,   "DPI"       ,   "DWSI1"        , "DWSI2"  ,       "DWSI3"         ,"DWSI4"    ,     "DWSI5"     ,   
                                               "EGFN"       ,   "EGFR"      ,   "EVI"          , "GDVI_2" ,       "GDVI_3"        ,"GDVI_4"   ,     "GI"        ,   
                                               "Gitelson"   ,   "Gitelson2" ,   "GMI1"         , "GMI2"   ,       "GreenNDVI"     ,"LWVI1"    ,     "LWVI2"     ,   
                                               "Maccioni"   ,   "MCARI"     ,   "MCARIOSAVI"   , "MCARI2" ,       "MCARI2OSAVI2"  ,"mND705"   ,     "mNDVI"     ,   
                                               "MPRI"       ,   "MSAVI"     ,   "MSI"          , "mSR"    ,       "mSR2"          ,"mSR705"   ,     "MTCI"      ,   
                                               "MTVI"       ,   "NDLI"      ,   "NDNI"         , "NDVI"   ,       "NDVI2"         ,"NDVI3"    ,     "NDWI"      ,   
                                               "NPCI"       ,   "OSAVI"     ,   "OSAVI2"       , "PARS"   ,       "PRI"           ,"PRICI2"   ,     "PRI_norm"  ,   
                                               "PSND"       ,   "PSRI"      ,   "PSSR"         , "PWI"    ,       "RDVI"          ,"REP_LE"   ,     "REP_Li"    ,   
                                               "SAVI"       ,   "SIPI"      ,   "SPVI"         , "SR"     ,       "SR1"           ,"SR2"      ,     "SR3"       ,   
                                               "SR4"        ,   "SR5"       ,   "SR6"          , "SR7"    ,       "SR8"           ,"SRPI"     ,     "SRWI"      ,   
                                               "Sum_Dr1"    ,   "Sum_Dr2"   ,   "SWIRFI"       , "SWIRLI" ,       "SWIRSI"        ,"SWIRVI"   ,     "TCARI"     ,   
                                               "TCARIOSAVI" ,   "TCARI2"    ,   "TCARI2OSAVI2" , "TGI"    ,       "TVI"           ,"Vogelmann",     "Vogelmann2",   
                                               "Vogelmann3" ,   "Vogelmann4")

##Now lets create a dataframe with equal amounts of scans for each species
alaskaSpecLib_VIs_NEW_PFT3_equal15<-alaskaSpecLib_VIs_NEW_PFT3 %>% group_by(PFT_3) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_VIs_NEW_PFT3_equal20<-alaskaSpecLib_VIs_NEW_PFT3 %>% group_by(PFT_3) %>% sample_n(20,replace = TRUE)
alaskaSpecLib_VIs_NEW_PFT3_equal20<-alaskaSpecLib_VIs_NEW_PFT3 %>% group_by(PFT_3) %>% sample_n(20,replace = TRUE)

##Removes unwanted metadata from dataframes
alaskaSpecLib_VIs_NEW_PFT2 [c("ScanID","PFT","PFT_3","area","Freq","sample_name","SWIR FI")] = NULL
alaskaSpecLib_VIs_NEW_PFT3 [c("ScanID","PFT","PFT_2","area","Freq","sample_name","SWIR FI")] = NULL

alaskaSpecLib_VIs_NEW_PFT3_equal15 [c("ScanID","PFT","PFT_2","area","Freq","sample_name","SWIR FI")] = NULL
alaskaSpecLib_VIs_NEW_PFT3_equal20 [c("ScanID","PFT","PFT_2","area","Freq","sample_name","SWIR FI")] = NULL

#Converts species column to a factor
alaskaSpecLib_VIs_NEW_PFT2$PFT_2<-as.factor(alaskaSpecLib_VIs_NEW_PFT2$PFT_2)
alaskaSpecLib_VIs_NEW_PFT3$PFT_3<-as.factor(alaskaSpecLib_VIs_NEW_PFT3$PFT_3)

alaskaSpecLib_VIs_NEW_PFT3_equal15$PFT_3<-as.factor(alaskaSpecLib_VIs_NEW_PFT3_equal15$PFT_3)
alaskaSpecLib_VIs_NEW_PFT3_equal20$PFT_3<-as.factor(alaskaSpecLib_VIs_NEW_PFT3_equal20$PFT_3)

##Apply randomforest classifyer to new VIs dataframe
rf_VIs_PFT2<-randomForest(PFT_2~.,data=alaskaSpecLib_VIs_NEW_PFT2,mtry=5,ntree=2001,importance=TRUE)
rf_VIs_PFT3<-randomForest(PFT_3~.,data=alaskaSpecLib_VIs_NEW_PFT3,mtry=5,ntree=2001,importance=TRUE)

rf_VIs_PFT3_equal15<-randomForest(PFT_3~.,data=alaskaSpecLib_VIs_NEW_PFT3_equal15,mtry=5,ntree=2001,importance=TRUE)
rf_VIs_PFT3_equal20<-randomForest(PFT_3~.,data=alaskaSpecLib_VIs_NEW_PFT3_equal20,mtry=5,ntree=2001,importance=TRUE)

##Error Analysis
VIs_species_error  <-rf_VIs_PFT2$confusion%>%as.data.frame%>%dplyr::select(class.error)
VIs_Funcgroup_Error<-rf_VIs_PFT3$confusion%>%as.data.frame%>%dplyr::select(class.error)

VIs_Funcgroup_Error_equal15<-rf_VIs_PFT3_equal15$confusion%>%as.data.frame%>%dplyr::select(class.error)
VIs_Funcgroup_Error_equal20<-rf_VIs_PFT3_equal20$confusion%>%as.data.frame%>%dplyr::select(class.error)


##uses model from spectral library to predict images
Results_HDW_plants        <-predict(rf_VIs_PFT3_equal15        ,Clayton_Headwall_Tiff_df[-(1:2)])

















