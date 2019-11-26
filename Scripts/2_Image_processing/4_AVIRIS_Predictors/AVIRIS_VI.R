####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in Imagery
Clayton_test_AV<-brick("Original_data/Test_imagery_AVIRIS/Clayton_test_AVIRIS")

##Marks raster as unrotated
Clayton_test_AV@rotated<-FALSE

##Converts to a dataframe
Clayton_test_AV<-rasterToPoints(Clayton_test_AV)%>% as.data.frame()

##Reads in spectral library as a dataframe
##this is the spectral library that had all uncalibrated bands removed
alaskaSpeclib_AV<-readRDS("Outputs/3_AV_Imagery/1_Processing/alaskaSpeclib_AV.rds")%>%as.data.frame()%>%dplyr::select(-sample_name)

##Reads in bandpasses for imagery to be used later
AV_wv<-scan("Outputs/3_AV_Imagery/1_Processing/AVIRIS_wv", numeric())

##change colnames to correct band names
colnames(Clayton_test_AV)[-1:-2]<-AV_wv

####Lets run that test again
tst<-lapply(alaskaSpeclib_AV[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst$V2%>%range()##There are no weird values, those are values outside of 0 and 2

##Reads in bandpasses for imagery to be used later
AV_wv<-scan("Outputs/3_AV_Imagery/1_Processing/AVIRIS_wv", numeric())

##Now lets change the column names of the df created from thge image, we want these column names to match the bandpasses
colnames(Clayton_test_AV)[-1:-2]<-AV_ng_wv

##Now lets check the range of the values in the image
test=lapply(Clayton_test_AV[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
test%>%View()
test%>%lapply(range) ### There are negative values here
Clayton_test_AV%>%subset(`381.87`<0)%>%nrow()

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
alaskaSpeclib_AV_matrix<-as.matrix(alaskaSpeclib_AV[-1:-7])
Clayton_test_AV_matrix <-as.matrix(Clayton_test_AV [-1:-2])

##lets check the column attributes to see if any weird values were introduced
alaskaSpeclib_AV_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()
Clayton_test_AV_matrix %>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
alaskaSpeclib_AV_speclib<-speclib(alaskaSpeclib_AV_matrix,AV_wv)
Clayton_test_AV_speclib <-speclib(Clayton_test_AV_matrix ,AV_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()
VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list


##Creates dataframe with Vegitation indices
alaskaSpeclib_AV_VIs<-vegindex(alaskaSpeclib_AV_speclib,index=VIs)
Clayton_test_AV_VIs <-vegindex(Clayton_test_AV_speclib ,index=VIs)


##lets do a logical test on Clayton_test_AV_VIs to see if strange values exist
test3<-lapply(Clayton_test_AV_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
test3%>%View()##There are columns where NaNs and Inf exist because the spectral range of the sensor is 400nm-100nm
              ##This means some Veg indices won't generate values because those bands are not present
              ##Lets remove all those columns with values that have NaNs and Infs
#Clayton_test_AV_VIs<-Clayton_test_AV_VIs%>%dplyr::select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI
                                                           #,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')


##lets do a logical test on alaskaSpeclib_AV_VIs to see if strange values exist
test4<-lapply(alaskaSpeclib_AV_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
test4%>%View()##There are columns where NaNs exist because the spectral range of the sensor is 400nm-100nm
              ##This means some Veg indices won't generate values because those bands are not present
              ##Lets remove all those columns with values that have NaNs and Infs
#alaskaSpeclib_AV_VIs<-alaskaSpeclib_AV_VIs%>%dplyr::select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI
                                                          # ,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')

##we need to combine the other columns with our new VI variables
alaskaSpeclib_AV_VIs<-cbind(alaskaSpeclib_AV[1:7],alaskaSpeclib_AV_VIs)
Clayton_test_AV_VIs <-cbind(Clayton_test_AV [1:2],Clayton_test_AV_VIs )


##Now we have to ensure that all column names have no spaces nor arithmetic operators
colnames(alaskaSpeclib_AV_VIs)[-1:-7]<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
                                         ,"Carter3"       ,"Carter4"       ,"Carter5"       ,"Carter6"       ,"CI"            ,"CI2"           ,"ClAInt"       
                                         ,"CRI1"          ,"CRI2"          ,"CRI3"          ,"CRI4"          ,"D1"            ,"D2"            ,"Datt"         
                                         ,"Datt2"         ,"Datt3"         ,"Datt4"         ,"Datt5"         ,"Datt6"         ,"DD"            ,"DDn"          
                                         ,"DPI"           ,"DWSI4"         ,"EGFN"          ,"EGFR"          ,"EVI"           ,"GDVI_2"        ,"GDVI_3"       
                                         ,"GDVI_4"        ,"GI"            ,"Gitelson"      ,"Gitelson2"     ,"GMI1"          ,"GMI2"          ,"GreenNDVI"   
                                         ,"Maccioni"      ,"MCARI"         ,"MCARIOSAVI"    ,"MCARI2"        ,"MCARI2OSAVI2"  ,"mND705"        ,"mNDVI"        
                                         ,"MPRI"          ,"MSAVI"         ,"mSR"           ,"mSR2"          ,"mSR705"        ,"MTCI"          ,"MTVI"         
                                         ,"NDVI"          ,"NDVI2"         ,"NDVI3"         ,"NPCI"          ,"OSAVI"         ,"OSAVI2"        ,"PARS"         
                                         ,"PRI"           ,"PRICI2"        ,"PRI_norm"      ,"PSND"          ,"PSRI"          ,"PSSR"          ,"RDVI"         
                                         ,"REP_LE"        ,"REP_Li"        ,"SAVI"          ,"SIPI"          ,"SPVI"          ,"SR"            ,"SR1"          
                                         ,"SR2"           ,"SR3"           ,"SR4"           ,"SR5"           ,"SR6"           ,"SR7"           ,"SR8"          
                                         ,"SRPI"          ,"Sum_Dr1"       ,"Sum_Dr2"       ,"TCARI"         ,"TCARIOSAVI"    ,"TCARI2"        ,"TCARI2OSAVI2"
                                         ,"TGI"           ,"TVI"           ,"Vogelmann"     ,"Vogelmann2"    ,"Vogelmann3"    ,"Vogelmann4")  

colnames(Clayton_test_AV_VIs)[-1:-2]<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
                                        ,"Carter3"       ,"Carter4"       ,"Carter5"       ,"Carter6"       ,"CI"            ,"CI2"           ,"ClAInt"       
                                        ,"CRI1"          ,"CRI2"          ,"CRI3"          ,"CRI4"          ,"D1"            ,"D2"            ,"Datt"         
                                        ,"Datt2"         ,"Datt3"         ,"Datt4"         ,"Datt5"         ,"Datt6"         ,"DD"            ,"DDn"          
                                        ,"DPI"           ,"DWSI4"         ,"EGFN"          ,"EGFR"          ,"EVI"           ,"GDVI_2"        ,"GDVI_3"       
                                        ,"GDVI_4"        ,"GI"            ,"Gitelson"      ,"Gitelson2"     ,"GMI1"          ,"GMI2"          ,"GreenNDVI"   
                                        ,"Maccioni"      ,"MCARI"         ,"MCARIOSAVI"    ,"MCARI2"        ,"MCARI2OSAVI2"  ,"mND705"        ,"mNDVI"        
                                        ,"MPRI"          ,"MSAVI"         ,"mSR"           ,"mSR2"          ,"mSR705"        ,"MTCI"          ,"MTVI"         
                                        ,"NDVI"          ,"NDVI2"         ,"NDVI3"         ,"NPCI"          ,"OSAVI"         ,"OSAVI2"        ,"PARS"         
                                        ,"PRI"           ,"PRICI2"        ,"PRI_norm"      ,"PSND"          ,"PSRI"          ,"PSSR"          ,"RDVI"         
                                        ,"REP_LE"        ,"REP_Li"        ,"SAVI"          ,"SIPI"          ,"SPVI"          ,"SR"            ,"SR1"          
                                        ,"SR2"           ,"SR3"           ,"SR4"           ,"SR5"           ,"SR6"           ,"SR7"           ,"SR8"          
                                        ,"SRPI"          ,"Sum_Dr1"       ,"Sum_Dr2"       ,"TCARI"         ,"TCARIOSAVI"    ,"TCARI2"        ,"TCARI2OSAVI2"
                                        ,"TGI"           ,"TVI"           ,"Vogelmann"     ,"Vogelmann2"    ,"Vogelmann3"    ,"Vogelmann4")  
##lets do a logical test again
test5<-lapply(alaskaSpeclib_AV_VIs[-1:-7], range)%>%as.data.frame%>%t()%>%as.data.frame()
test5%>%View()###There are NaNs and infs, lets remove them, dim() 1974  333

##lets do a logical test again to the image
test6<-lapply(Clayton_test_AV_VIs[-1:-7], range)%>%as.data.frame%>%t()%>%as.data.frame()
test6%>%View()###There are NaNs and infs, lets remove them, dim() 1974  333


##need to come up with a function
##NAs_inf<-c("Datt","MCARIOSAVI","PRI_norm","SIPI","TCARIOSAVI")
##alaskaSpeclib_AV_VIs1<-apply(alaskaSpeclib_AV_VIs,alaskaSpeclib_AV_VIs[,NAs_inf]%>%na.omit())
##a<-alaskaSpeclib_AV_VIs[,NAs_inf]%>%na.omit()%>%as.data.frame()
##alaskaSpeclib_AV_VIs<- alaskaSpeclib_AV_VIs[apply(alaskaSpeclib_AV_VIs[,NAs_inf]<2, 1, all),]

alaskaSpeclib_AV_VIs<-subset(alaskaSpeclib_AV_VIs, is.infinite(Datt)      ==F) ##dim()  1973  102 
alaskaSpeclib_AV_VIs<-subset(alaskaSpeclib_AV_VIs, is.na      (MCARIOSAVI)==F) ##dim()  1973  102
alaskaSpeclib_AV_VIs<-subset(alaskaSpeclib_AV_VIs, is.infinite(PRI_norm)  ==F) ##dim()  1973  102
alaskaSpeclib_AV_VIs<-subset(alaskaSpeclib_AV_VIs, is.infinite(SIPI)      ==F) ##dim()  1973  102
alaskaSpeclib_AV_VIs<-subset(alaskaSpeclib_AV_VIs, is.na      (TCARIOSAVI)==F) ##dim()  1973  102

##Now lets equalize the scans per functional group to be used in models later
alaskaSpeclib_AV_VIs_equal25<-alaskaSpeclib_AV_VIs %>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(alaskaSpeclib_AV_VIs        ,"Outputs/2_AV_Imagery/1_processing/alaskaSpeclib_AV_VIs.csv"        ,row.names = FALSE)
write.csv(alaskaSpeclib_AV_VIs_equal25,"Outputs/2_AV_Imagery/1_processing/alaskaSpeclib_AV_VIs_equal25.csv",row.names = FALSE)
write.csv(Clayton_test_AV_VIs         ,"Outputs/2_AV_Imagery/1_processing/Clayton_test_AV_VIs.csv"         ,row.names = FALSE)













