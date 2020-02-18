library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in spectral library....dim 1974  333
Lichen_groups_LAN<-read.csv("Outputs/1_Field_spec/1_Processing/Landsat_data/Lichen_groups_df.csv")

##Lets create a list of band names
Landsat_wv_names<-c("band_1"
                   ,"band_2"
                   ,"band_3"
                   ,"band_4"
                   ,"band_5"
                   ,"band_7")

##Lets rename the columns
colnames(Lichen_groups_LAN)[-1:-12]<-Landsat_wv_names

##Now lets do the calculations for each vegitation index
Lichen_groups_LAN_VIs<-Lichen_groups_LAN%>%
  mutate(NDVI = (band_4 - band_3) / (band_4 + band_3)
        ,NDWI = (band_2 - band_4) / (band_2 + band_4)
        ,NDMI = (band_4 - band_5) / (band_4 + band_5)
        ,NDSI = (band_2 - band_5) / (band_2 + band_5)
        ,NBR  = (band_4 - band_7) / (band_4 + band_7)
        ,EVI  = 2.5 * ((band_4 - band_3) / (band_4 + 6 * band_3 - 7.5 * band_1 + 1)))%>%
  dplyr::select(-band_1,-band_2,-band_3,-band_4,-band_5,-band_7)

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(Lichen_groups_LAN_VIs,"Outputs/1_Field_spec/1_Processing/Landsat_data/Lichen_groups_LAN_VIs.csv",row.names = FALSE)
