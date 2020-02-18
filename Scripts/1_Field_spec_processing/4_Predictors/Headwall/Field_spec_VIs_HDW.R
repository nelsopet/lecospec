library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in spectral library....dim 1974  333
alaskaSpeclib_HDW<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpecLib_HDW_df.csv")

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/1_Field_spec/1_Processing/Headwall_data/Headwall_wv", numeric())

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
alaskaSpeclib_HDW_matrix<-as.matrix(alaskaSpeclib_HDW[-1:-9])

##lets check the column attributes to see if any weird values were introduced
alaskaSpeclib_HDW_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
alaskaSpeclib_HDW_speclib<-speclib(alaskaSpeclib_HDW_matrix,HDW_ng_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()

##Vegitation indices mREIP won't work so remove it from list
##Remember the field spectral library was resampled on the headwall sensor's bandpasses...400nm-100nm
##This means some Veg indices won't generate values because those bands are not present
##Lets remove thos VIs that won't work
VIs<-VIs[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]

##Creates dataframe with Vegitation indices
alaskaSpeclib_HDW_VIs<-vegindex(alaskaSpeclib_HDW_speclib,index=VIs)

##lets do a logical test on alaskaSpeclib_HDW_VIs to see if strange values exist
test4<-lapply(alaskaSpeclib_HDW_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
#test4%>%View()##There are no columns where NaNs or Infs exist 


##we need to combine the other columns with our new VI variables
alaskaSpeclib_HDW_VIs<-cbind(alaskaSpeclib_HDW[1:9],alaskaSpeclib_HDW_VIs)

Newcolnames<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
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

colnames(alaskaSpeclib_HDW_VIs)[-1:-9]<-Newcolnames

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(alaskaSpeclib_HDW_VIs,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_VIs.csv",row.names = FALSE)
