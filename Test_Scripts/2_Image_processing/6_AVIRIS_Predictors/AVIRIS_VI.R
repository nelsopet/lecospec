####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in Imagery
Test_IMG_AV<-brick("Original_data/Test_imagery_AVIRIS/Test_IMG_AVIRIS")

##Marks raster as unrotated
Test_IMG_AV@rotated<-FALSE

##Converts to a dataframe
Test_IMG_AV<-rasterToPoints(Test_IMG_AV)%>% as.data.frame()

##Reads in bandpasses for imagery to be used later
AV_wv<-scan("Outputs/3_AV_Imagery/1_Processing/AVIRIS_wv", numeric())

##change colnames to correct band names
colnames(Test_IMG_AV)[-1:-2]<-AV_wv

##Now lets check the range of the values in the image
test=lapply(Test_IMG_AV[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#test%>%View()
test%>%lapply(range) ### There are negative values here in all the rows
Test_IMG_AV%>%subset(`381.87`<0)%>%nrow()

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
Test_IMG_AV_matrix <-as.matrix(Test_IMG_AV [-1:-2])

##lets check the column attributes to see if any weird values were introduced
Test_IMG_AV_matrix %>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
Test_IMG_AV_speclib <-speclib(Test_IMG_AV_matrix ,AV_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()
VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list

##Creates dataframe with Vegitation indices
Test_IMG_AV_VIs <-vegindex(Test_IMG_AV_speclib ,index=VIs)


##lets do a logical test on Test_IMG_AV_VIs to see if strange values exist
test3<-lapply(Test_IMG_AV_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
#test3%>%View()##There are no columns where NaNs and Inf exist

##we need to combine the other columns with our new VI variables
Test_IMG_AV_VIs <-cbind(Test_IMG_AV [1:2],Test_IMG_AV_VIs )

##Lets change column naames to exclude spaces and srithmetic operators
colnames(Test_IMG_AV_VIs)[-1:-2]<-c("Boochs"     ,   "Boochs2"  ,     "CAI"           , "CARI"     , "Carter"       , "Carter2"   ,  "Carter3"   ,   
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



##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(Test_IMG_AV             ,"Test_Outputs/3_AV_Imagery/1_Processing/Test_IMG_AV_df.csv"          ,row.names = FALSE)
write.csv(Test_IMG_AV_VIs         ,"Test_Outputs/3_AV_Imagery/1_Processing/Test_IMG_AV_VIs.csv"         ,row.names = FALSE)













