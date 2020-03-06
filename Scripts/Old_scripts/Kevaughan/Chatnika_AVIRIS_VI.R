####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in Imagery
Chatnika_clipped_AV<-brick("Original_data/Test_imagery_AVIRIS/Chatnika_clipped_AV")

##Marks raster as unrotated
Chatnika_clipped_AV@rotated<-FALSE

##Converts to a dataframe
Chatnika_clipped_AV<-rasterToPoints(Chatnika_clipped_AV)%>% as.data.frame()

##Reads in bandpasses for imagery to be used later
AV_wv<-scan("Outputs/3_AV_Imagery/1_Processing/AVIRIS_wv", numeric())

##Now lets change the column names of the df created from thge image, we want these column names to match the bandpasses
colnames(Chatnika_clipped_AV)[-1:-2]<-AV_wv

##Now lets check the range of the values in the image
test=lapply(Chatnika_clipped_AV[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
test%>%View()
test%>%lapply(range) ### There are negative values here
Chatnika_clipped_AV%>%subset(`381.87`<0)%>%nrow()

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
Chatnika_clipped_AV_matrix <-as.matrix(Chatnika_clipped_AV [-1:-2])

##lets check the column attributes to see if any weird values were introduced
Chatnika_clipped_AV_matrix %>%min()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
Chatnika_clipped_AV_speclib <-speclib(Chatnika_clipped_AV_matrix ,AV_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()
VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list

##Creates dataframe with Vegitation indices
Chatnika_clipped_AV_VIs <-vegindex(Chatnika_clipped_AV_speclib ,index=VIs)

##lets do a logical test on Chatnika_clipped_AV_VIs to see if strange values exist
test3<-lapply(Chatnika_clipped_AV_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
test3%>%View()##There are columns where NaNs and Inf exist because the spectral range of the sensor is 400nm-100nm
              ##This means some Veg indices won't generate values because those bands are not present
              ##Lets remove all those columns with values that have NaNs and Infs
Chatnika_clipped_AV_VIs<-Chatnika_clipped_AV_VIs%>%dplyr::select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI
                                                                 ,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')

##we need to combine the other columns with our new VI variables
Chatnika_clipped_AV_VIs <-cbind(Chatnika_clipped_AV [1:2],Chatnika_clipped_AV_VIs )

newcolnames<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
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

colnames(Chatnika_clipped_AV_VIs)[-1:-2]<-newcolnames

#lets do a logical test again to the image
test6<-lapply(Chatnika_clipped_AV_VIs[-1:-7], range)%>%as.data.frame%>%t()%>%as.data.frame()
test6%>%View()###There are NaNs and infs, lets remove them, dim() 1974  333

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(Chatnika_clipped_AV_VIs         ,"Outputs/3_AV_Imagery/1_Processing/Chatnika_clipped_AV_VIs.csv"         ,row.names = FALSE)













