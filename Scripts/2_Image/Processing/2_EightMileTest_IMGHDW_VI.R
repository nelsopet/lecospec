####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe (created in the resampling script)
EightMileTest_HDW  <-read.csv("Outputs/2_Imagery/Headwall/Processing/EightMileTest_IMG_HDW_df.csv")

##Now lets check the range of the values in the image
test<-lapply(EightMileTest_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#test%>%View()
test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/1_Field_spec/1_Processing/Headwall_data/Headwall_wv", numeric())

#EightMileTest_HDW[is.na(EightMileTest_HDW$`397.593`),]%>% nrow()

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
EightMileTest_HDW_matrix   <-as.matrix(EightMileTest_HDW   [-1:-2])

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
EightMileTest_HDW_speclib   <-speclib  (EightMileTest_HDW_matrix   ,HDW_ng_wv[1:272])

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()

##Vegitation indices mREIP won't work so remove it from list
##Remember the field spectral library was resampled on the headwall sensor's bandpasses...400nm-100nm
##This means some Veg indices won't generate values because those bands are not present
##Lets remove thos VIs that won't work
VIs<-VIs[-c(3,26,27,31,32,33,35,48,49,58,60,66,67,71,82,99,102,103,104,105)]

##Creates dataframe with Vegitation indices
EightMileTest_VIs       <-vegindex(EightMileTest_HDW_speclib       ,index=VIs)

##rename columns
colnames(EightMileTest_VIs  )<-VIs

##lets do a logical test on EightMileTest_HDW_VIs to see if strange values exist
##Here we need an ifelse function to remove NaNs and Inf or possibly convert them to NAs/0s
test3<-lapply(EightMileTest_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
##test3%>%View()##There are no columns where NaNs and Inf exist in this dataframe

##we need to combine lat/long columns with our new VI variables
EightMileTest_VIs_A  <-cbind(EightMileTest_HDW   [1:2],EightMileTest_VIs   )

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

colnames(EightMileTest_VIs_A )[-1:-2]<-newcolnames  ##dim 66024 97

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(EightMileTest_VIs_A             ,"Outputs/2_Imagery/Headwall/Processing/EightMileTest_HDW_VIs.csv",row.names = FALSE)













