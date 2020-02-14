###################################Headwall_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(raster)
library(tidyverse)
library(hsdar)

##Reads in Imagery as multi-layer raster...a RasterBrick (214 rows,238 cols, 50932 cells)
Clayton_Headwall_Tiff_Spatial<-brick("Imagery/headwallclipnew")
###Take a look at the first layer in this raster to get an idea of what it looks like
##plot(Clayton_Headwall_Tiff_Spatial[[1]])

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Processed_imagery/Headwall/Bandpasses/Headwall_WVLs", numeric())

##Convert hyperspectral image into a dataframe....dataframe with 397510 rows and 328 columns  
Clayton_Headwall_Tiff_df<-rasterToPoints(Clayton_Headwall_Tiff_Spatial)%>%as.data.frame()

##change column names of the df above to be numeric
colnames(Clayton_Headwall_Tiff_df)[-1:-2]<-HDW_ng_wv

##Check the range of the values in this data set
test=lapply(Clayton_Headwall_Tiff_df[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
test%>%View() ###
test%>%subset(V1==0)%>%View() ### All values fall between 0 and 1.2 and there are no NA values

#Extract metadata to be merged later ... dataframe of 1901 rows and 7 columns..dataframe with 2 columns and 397510 rows
Clayton_Headwall_Tiff_df_meta<-as.data.frame(Clayton_Headwall_Tiff_df)%>%dplyr::select(1:2)

##change column names of the df above to be numeric
colnames(Clayton_Headwall_Tiff_df)[-1:-2]<-HDW_ng_wv

###Creates a dataframe containing all the columns indicating bands and rows of reflectances. .......dataframe with 397510 rows and 326 columns  
Clayton_Headwall_Tiff_df_spec<-(Clayton_Headwall_Tiff_df)[-1:-2]

##convert alaskaSpeclib_spec to matrix...it has to be a matrix to be converted to a speclib object
## Check the column attributes here to see if weird values are introduced
Clayton_Headwall_Tiff_df_spec<-as.matrix(Clayton_Headwall_Tiff_df_spec)
Clayton_Headwall_Tiff_df_spec%>%max()

##Creates a spectralib object ... 13 slots
## Check the column attributes here to see if weird values are introduced
Clayton_Headwall_Tiff_speclib<-speclib(Clayton_Headwall_Tiff_df_spec,HDW_ng_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()

##alaskaSpecLib_mREIP<-vegindex(alaskaSpecLib_lib,index = "mREIP"        )%>%as.data.frame()%>%`colnames<-`(c("mREIP"        ))
##Vegitation indices mREIP won't work so remove it from list
VIs<-VIs[-58]

##Creates dataframe with Vegitation indices as variables ..dataframe of  rows by 114 variables, each of which is a veg index
# Crop this input evern futher to make it super tiny to get it to work. It takes several minutes to run as is.
Clayton_Headwall_Tiff_VIs<-vegindex(Clayton_Headwall_Tiff_speclib, index = VIs)
colnames(Clayton_Headwall_Tiff_VIs)<-VIs

##check to see if NaNs/Inf exist ... NaNs produced in vegindex stuff ... check to see if input to vegindex has weird values anyways
##If Nas exist, write a function that removes the all
test_Clayton_Headwall_Tiff_VIs=lapply(Clayton_Headwall_Tiff_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
View(test_Clayton_Headwall_Tiff_VIs)

##NaNs exist so we will remove them
Clayton_Headwall_Tiff_VIs2<-Clayton_Headwall_Tiff_VIs%>%select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-'SWIR FI',-'SWIR LI',-'SWIR SI',-'SWIR VI')

##Combines metadta with new variables to be used in models...use duplicated dataframe, this way we could do a join later
Clayton_Headwall_Tiff_VIs2<-cbind(Clayton_Headwall_Tiff_df_meta,Clayton_Headwall_Tiff_VIs2)

Clayton<-rasterFromXYZ(Clayton_Headwall_Tiff_VIs2, crs = crs(Clayton_Headwall_Tiff_Spatial))

plot(Clayton[[19]])

##Change column names so they have no spaces or arithmetic operators
colnames(Clayton_Headwall_Tiff_VIs2)[3:97]<-c("Boochs"        ,"Boochs2"       ,"CARI"          ,"Carter"        ,"Carter2"      
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
###Now lets make models
VEG_Index<-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index.csv")

##CREATE DATAFRAME WITH EQUAL AMOUNTS OF SCANS PER FUNCTIONAL GROUP
VEG_Index_equal10<-VEG_Index %>% group_by(PFT_3) %>% sample_n(10,replace = TRUE)
VEG_Index_equal30<-VEG_Index %>% group_by(PFT_3) %>% sample_n(30,replace = TRUE)


##REMOVE VEG INDICES THAT ARE NOT IN IMAGERY
VEG_Index        <-VEG_Index%>%select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-SWIRFI,-SWIRLI,-SWIRSI,-SWIRVI,-Boochs.1)
VEG_Index_equal10<-VEG_Index_equal10%>%select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-SWIRFI,-SWIRLI,-SWIRSI,-SWIRVI,-Boochs.1)
VEG_Index_equal30<-VEG_Index_equal30%>%select(-CAI,-Datt7,-Datt8,-DWSI1,-DWSI2,-DWSI3,-DWSI5,-LWVI1,-LWVI2,-MSI,-NDLI,-NDNI,-NDWI,-PWI,-SRWI,-SWIRFI,-SWIRLI,-SWIRSI,-SWIRVI,-Boochs.1)

VEG_Index        [c("ScanID","PFT","PFT_2","area","Freq")] = NULL
VEG_Index_equal10[c("ScanID","PFT","PFT_2","area","Freq")] = NULL
VEG_Index_equal30[c("ScanID","PFT","PFT_2","area","Freq")] = NULL


rfdata_HDW_VI        <-randomForest(PFT_3~.,data=VEG_Index        ,mtry=5,ntree=2001,importance=TRUE)
rfdata_HDW_VI_10     <-randomForest(PFT_3~.,data=VEG_Index_equal10,mtry=5,ntree=2001,importance=TRUE)
rfdata_HDW_VI_30     <-randomForest(PFT_3~.,data=VEG_Index_equal30,mtry=5,ntree=2001,importance=TRUE)


Results_HDW           <-predict(rfdata_HDW_VI           ,Clayton_Headwall_Tiff_VIs2[-(1:2)])
Results_HDW_10        <-predict(rfdata_HDW_VI_10        ,Clayton_Headwall_Tiff_VIs2[-(1:2)])
Results_HDW_30        <-predict(rfdata_HDW_VI_30        ,Clayton_Headwall_Tiff_VIs2[-(1:2)])


##converts prediction from rf model to dataframe
Results_HDW   <-as.data.frame(Results_HDW)
Results_HDW_10<-as.data.frame(Results_HDW_10)
Results_HDW_30<-as.data.frame(Results_HDW_30)

##Changes first column name to predicted
names(Results_HDW   )[1]<-"predicted"
names(Results_HDW_10)[1]<-"predicted"
names(Results_HDW_30)[1]<-"predicted"

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_dat   <-cbind(Results_HDW    ,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)
Results_HDW_10_dat<-cbind(Results_HDW_10,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)
Results_HDW_30_dat<-cbind(Results_HDW_30,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)

###Creates Unique PFT_IDs
Unique_PFT_HDW<-unique(as.data.frame(Results_HDW_dat$predicted)) 
Unique_PFT_HDW$PFT_ID<-seq(1:nrow(Unique_PFT_HDW))
names(Unique_PFT_HDW)[1]<-"predicted"

Unique_PFT_HDW_10<-unique(as.data.frame(Results_HDW_10_dat$predicted)) 
Unique_PFT_HDW_10$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_10))
names(Unique_PFT_HDW_10)[1]<-"predicted"

Unique_PFT_HDW_30<-unique(as.data.frame(Results_HDW_30_dat$predicted)) 
Unique_PFT_HDW_30$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_30))
names(Unique_PFT_HDW_30)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_PFT   <-merge(Results_HDW_dat   ,Unique_PFT_HDW   , by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_PFT_10<-merge(Results_HDW_10_dat,Unique_PFT_HDW_10, by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_PFT_30<-merge(Results_HDW_30_dat,Unique_PFT_HDW_30, by="predicted")%>% dplyr::select(x,y,PFT_ID)

##Converts dataframe to a raster for predicted layer
Results_HDW_raster   <-rasterFromXYZ(Results_HDW_PFT   , crs = crs(Clayton_Headwall_Tiff_Spatial))
Results_HDW_raster_10<-rasterFromXYZ(Results_HDW_PFT_10, crs = crs(Clayton_Headwall_Tiff_Spatial))
Results_HDW_raster_30<-rasterFromXYZ(Results_HDW_PFT_30, crs = crs(Clayton_Headwall_Tiff_Spatial))

##Creates a plot showing the output of the predicted ratser layer
par(xpd = FALSE)
plot(
  Results_HDW_raster_10,
  legend = FALSE,
  col = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  xaxt = 'n',
  yaxt = 'n'
)

par(xpd = TRUE)
legend(
  "right",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid"),
  fill = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  cex=0.75,
  xjust =1,
  horiz = FALSE,
  inset = -0.1,
  par(cex=0.3)
)                   

par(xpd = FALSE)
plot(
  Results_HDW_raster_30,
  legend = FALSE,
  col = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  xaxt = 'n',
  yaxt = 'n'
)

par(xpd = TRUE)
legend(
  "right",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid"),
  fill = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  cex=0.75,
  xjust =1,
  horiz = FALSE,
  inset = -0.1,
  par(cex=0.3)
)                    
                                              











