library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in spectral library....dim 1974  333
alaskaSpeclib_AV<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpecLib_AV_df_equal25.csv")

####Lets run that test again
tst<-lapply(alaskaSpeclib_AV[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst$V2%>%range()##There are no weird values, those are values outside of 0 and 2

##Reads in bandpasses for imagery to be used later
AV_ng_wv<-scan("Test_Outputs/3_AV_Imagery/1_Processing/AVIRIS_wv", numeric())

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
alaskaSpeclib_AV_matrix<-as.matrix(alaskaSpeclib_AV[-1:-7])

##lets check the column attributes to see if any weird values were introduced
alaskaSpeclib_AV_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
alaskaSpeclib_AV_speclib<-speclib(alaskaSpeclib_AV_matrix,AV_ng_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()
VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list

##Creates dataframe with Vegitation indices
alaskaSpeclib_AV_VIs<-vegindex(alaskaSpeclib_AV_speclib,index=VIs)

##lets do a logical test on alaskaSpeclib_AV_VIs to see if strange values exist
test4<-lapply(alaskaSpeclib_AV_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
test4%>%View()##There are NO columns where NaNs exist

##we need to combine the other columns with our new VI variables
alaskaSpeclib_AV_VIs<-cbind(alaskaSpeclib_AV[1:7],alaskaSpeclib_AV_VIs)

##Lets change column naames to exclude spaces and arithmetic operators
colnames(alaskaSpeclib_AV_VIs)[-1:-7]<-c("Boochs"     ,   "Boochs2"  ,     "CAI"           , "CARI"     , "Carter"       , "Carter2"   ,  "Carter3"   ,   
                                    "Carter4"    ,   "Carter5"  ,     "Carter6"       , "CI"       , "CI2"          , "ClAInt"    ,  "CRI1"      ,   
                                    "CRI2"       ,   "CRI3"     ,     "CRI4"          , "D1"       , "D2"           , "Datt"      ,  "Datt2"     ,   
                                    "Datt3"      ,   "Datt4"    ,     "Datt5"         , "Datt6"    , "Datt7"        , "Datt8"     ,  "DD"        ,   
                                    "DDn"        ,   "DPI"      ,     "DWSI1"         , "DWSI2"    , "DWSI3"        , "DWSI4"     ,  "DWSI5"     ,   
                                    "EGFN"       ,   "EGFR"     ,     "EVI"           , "GDVI_2"   , "GDVI_3"       , "GDVI_4"    ,  "GI"        ,   
                                    "Gitelson"   ,   "Gitelson2",     "GMI1"          , "GMI2"     , "GreenNDVI"    , "LWVI1"     ,  "LWVI2"     ,   
                                    "Maccioni"   ,   "MCARI"    ,     "MCARIOSAVI"    , "MCARI2"   , "MCARI2OSAVI2" , "mND705"    ,  "mNDVI"     ,   
                                    "MPRI"       ,   "MSAVI"    ,     "MSI"           , "mSR"      , "mSR2"         , "mSR705"    ,  "MTCI"      ,   
                                    "MTVI"       ,   "NDLI"     ,     "NDNI"          , "NDVI"     , "NDVI2"        , "NDVI3"     ,  "NDWI"      ,   
                                    "NPCI"       ,   "OSAVI"    ,     "OSAVI2"        , "PARS"     , "PRI"          , "PRICI2"    ,  "PRI_norm"  ,   
                                    "PSND"       ,   "PSRI"     ,     "PSSR"          , "PWI"      , "RDVI"         , "REP_LE"    ,  "REP_Li"    ,   
                                    "SAVI"       ,   "SIPI"     ,     "SPVI"          , "SR"       , "SR1"          , "SR2"       ,  "SR3"       ,   
                                    "SR4"        ,   "SR5"      ,     "SR6"           , "SR7"      , "SR8"          , "SRPI"      ,  "SRWI"      ,   
                                    "Sum_Dr1"    ,   "Sum_Dr2"  ,     "SWIRFI"        , "SWIRLI"   , "SWIRSI"       , "SWIRVI"    ,  "TCARI"     ,   
                                    "TCARIOSAVI" ,   "TCARI2"   ,     "TCARI2OSAVI2"  , "TGI"      , "TVI"          , "Vogelmann" ,  "Vogelmann2",  
                                    "Vogelmann3" ,   "Vogelmann4")

##Now lets equalize the scans per functional group to be used in models later
alaskaSpeclib_AV_VIs_equal25<-alaskaSpeclib_AV_VIs %>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(alaskaSpeclib_AV_VIs,"Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpeclib_AV_VIs_equal25.csv",row.names = FALSE)
