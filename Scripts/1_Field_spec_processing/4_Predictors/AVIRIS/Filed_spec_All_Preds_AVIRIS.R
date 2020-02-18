#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for scans
alaskaSpeclib_AV_df <-read.csv("Outputs/1_Field_spec/1_Processing/AVIRIS_data/alaskaSpecLib_AV_df.csv"         )
alaskaSpeclib_AV_VIs<-read.csv("Outputs/1_Field_spec/1_Processing/AVIRIS_data/alaskaSpeclib_AV_VIs_equal25.csv")


##Make names for colnames in each df unique
colnames(alaskaSpeclib_AV_df )[-1:-9]<-paste0(colnames(alaskaSpeclib_AV_df )[-1:-9],"_raw")
colnames(alaskaSpeclib_AV_VIs)[-1:-9]<-paste0(colnames(alaskaSpeclib_AV_VIs)[-1:-9],"_VIs")

##Let's merge these dataframes
alaskaSpecLib_data_AV<-Reduce(cbind,list(alaskaSpeclib_AV_df 
                                        ,alaskaSpeclib_AV_VIs  [-1:-9]))
##Lets save this dataframe
write.csv(alaskaSpecLib_data_AV,"Outputs/1_Field_spec/1_Processing/AVIRIS_data/alaskaSpecLib_data_AV.csv",row.names = FALSE)

