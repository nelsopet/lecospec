####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe
EightMile_HDW<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_df.csv",check.names = FALSE)

##Now lets check the range of the values in the image
test<-lapply(EightMile_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#test%>%View()
test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Field_Spec/Headwall_wv", numeric())

#EightMile_HDW[is.na(EightMile_HDW$`397.593`),]%>% nrow()

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
EightMile_HDW_matrix   <-as.matrix(EightMile_HDW   [-1:-2])

##lets check the column attributes to see if any weird values were introduced
##EightMile_HDW_matrix   %>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
EightMile_HDW_speclib   <-speclib  (EightMile_HDW_matrix   ,HDW_ng_wv[1:272])

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()
#VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list

##Vegitation indices mREIP won't work so remove it from list
VIs<-VIs[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]

##Creates dataframe with Vegitation indices
EightMile_VIs       <-vegindex(EightMile_HDW_speclib       ,index=VIs)

##rename columns
colnames(EightMile_VIs  )<-VIs

##lets do a logical test on EightMile_HDW_VIs to see if strange values exist
test3<-lapply(EightMile_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
#test3%>%View()##There are columns where NaNs and Inf exist because the spectral range of the sensor is 400nm-100nm
##This means some Veg indices won't generate values because those bands are not present
##Lets remove all those columns with values that have NaNs and Infs
#EightMile_VIs<-EightMile_VIs%>%dplyr::select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI
                                           #,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')

EightMile_VIs_A  <-cbind(EightMile_HDW   [1:2],EightMile_VIs   )

##Now we have to ensure that all column names have no spaces nor arithmetic operators
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

colnames(EightMile_VIs_A )[-1:-2]<-newcolnames
##DIM725940 274

##lets do a logical test again to the image 
test6<-lapply(EightMile_VIs_A[-1:-2], range)%>%as.data.frame%>%t()%>%as.data.frame()
test6%>%view()###There are NaNs and infs, lets remove them, dim() 1974  333

##need to come up with a function
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Boochs)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Boochs2)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CARI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Carter)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Carter2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Carter3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Carter4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Carter5)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Carter6)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(ClAInt)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CRI1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CRI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CRI3)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(CRI4)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(D1)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(D2)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Datt)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Datt2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Datt3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Datt4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Datt5)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Datt6)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(DD)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(DDn)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(DPI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(DWSI4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(EGFN)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(EGFR)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(EVI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GDVI_2)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GDVI_3)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GDVI_4)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Gitelson)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Gitelson2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GMI1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GMI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(GreenNDVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Maccioni)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MCARI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MCARIOSAVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MCARI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MCARI2OSAVI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(mND705)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(mNDVI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MPRI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MSAVI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(mSR)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(mSR2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(mSR705)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MTCI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(MTVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(NDVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(NDVI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(NDVI3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(NPCI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(OSAVI)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(OSAVI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PARS)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PRI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PRICI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PRI_norm)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PSND)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PSRI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(PSSR)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(RDVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(REP_LE)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(REP_Li)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SAVI)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SIPI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SPVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR5)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR6)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR7)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SR8)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(SRPI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Sum_Dr1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Sum_Dr2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(TCARI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(TCARIOSAVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(TCARI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(TCARI2OSAVI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(TGI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(TVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Vogelmann)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Vogelmann2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Vogelmann3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.infinite(Vogelmann4)==F) ##dim()  1973  102
#
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Boochs)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Boochs2)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CARI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Carter)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Carter2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Carter3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Carter4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Carter5)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Carter6)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(ClAInt)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CRI1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CRI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CRI3)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(CRI4)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(D1)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(D2)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Datt)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Datt2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Datt3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Datt4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Datt5)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Datt6)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(DD)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(DDn)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(DPI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(DWSI4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(EGFN)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(EGFR)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(EVI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GDVI_2)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GDVI_3)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GDVI_4)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Gitelson)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Gitelson2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GMI1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GMI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(GreenNDVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Maccioni)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MCARI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MCARIOSAVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MCARI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MCARI2OSAVI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(mND705)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(mNDVI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MPRI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MSAVI)==F) ##dim()  1973  102 
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(mSR)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(mSR2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(mSR705)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MTCI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(MTVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(NDVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(NDVI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(NDVI3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(NPCI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(OSAVI)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(OSAVI2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PARS)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PRI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PRICI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PRI_norm)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PSND)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PSRI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(PSSR)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(RDVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(REP_LE)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(REP_Li)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SAVI)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SIPI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SPVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR4)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR5)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR6)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR7)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SR8)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(SRPI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Sum_Dr1)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Sum_Dr2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(TCARI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(TCARIOSAVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(TCARI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(TCARI2OSAVI2)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(TGI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(TVI)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Vogelmann)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Vogelmann2)==F) ##dim()  569688     97
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Vogelmann3)==F) ##dim()  1973  102
#EightMile_VIs_A<-subset(EightMile_VIs_A, is.na(Vogelmann4)==F) ##dim()  1973  102

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(EightMile_VIs_A             ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_HDW_VIs.csv"         ,row.names = FALSE)













