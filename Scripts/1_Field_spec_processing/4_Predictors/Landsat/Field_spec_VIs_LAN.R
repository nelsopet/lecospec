library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in spectral library....dim 1974  333
alaskaSpeclib_LAN<-read.csv("Outputs/1_Field_spec/1_Processing/Landsat_data/alaskaSpecLib_LAN_df.csv")

###Reads in bandpasses for imagery to be used later
#LAN_ng_wv<-scan("Outputs/1_Field_spec/1_Processing/Landsat_data/Landsat_wv", numeric())
#
####you'll need to convert your dfs to a matrix before VIS can be applied
###lets fo this for df created from the image and our spectral library of scans
#alaskaSpeclib_LAN_matrix<-as.matrix(alaskaSpeclib_LAN[-1:-9])
#
####lets check the column attributes to see if any weird values were introduced
#alaskaSpeclib_LAN_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()
#
####Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
#alaskaSpeclib_LAN_speclib<-speclib(alaskaSpeclib_LAN_matrix,LAN_ng_wv)

##Lets create a list of band names
Landsat_wv_names<-c("band_1"
                   ,"band_2"
                   ,"band_3"
                   ,"band_4"
                   ,"band_5"
                   ,"band_7")

##Lets rename the columns
colnames(alaskaSpeclib_LAN)[-1:-9]<-Landsat_wv_names

##Now lets do the calculations for each vegitation index
alaskaSpeclib_LAN_VIs<-alaskaSpeclib_LAN%>%
  mutate(NDVI = (band_4 - band_3) / (band_4 + band_3)
        ,NDWI = (band_2 - band_4) / (band_2 + band_4)
        ,NDMI = (band_4 - band_5) / (band_4 + band_5)
        ,NDSI = (band_2 - band_5) / (band_2 + band_5)
        ,NBR  = (band_4 - band_7) / (band_4 + band_7)
        ,EVI  = 2.5 * ((band_4 - band_3) / (band_4 + 6 * band_3 - 7.5 * band_1 + 1)))%>%
  dplyr::select(-band_1,-band_2,-band_3,-band_4,-band_5,-band_7)


###Lets create a function that will calculate the VIs for the ones listed above
#VIs<-vegindex()
#
##Vegitation indices mREIP won't work so remove it from list
###Remember the field spectral library was resampled on the headwall sensor's bandpasses...400nm-100nm
###This means some Veg indices won't generate values because those bands are not present
###Lets remove thos VIs that won't work
#VIs<-VIs[c(71,68,38)]
#
###Creates dataframe with Vegitation indices
#alaskaSpeclib_LAN_VIs<-vegindex(alaskaSpeclib_LAN_speclib,index=VIs)
#
###lets do a logical test on alaskaSpeclib_LAN_VIs to see if strange values exist
#test4<-lapply(alaskaSpeclib_LAN_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
#test4%>%View()##There are no columns where NaNs exist 

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(alaskaSpeclib_LAN_VIs,"Outputs/1_Field_spec/1_Processing/Landsat_data/alaskaSpeclib_LAN_VIs.csv",row.names = FALSE)
