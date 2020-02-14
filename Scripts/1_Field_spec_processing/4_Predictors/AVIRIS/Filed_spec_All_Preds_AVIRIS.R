#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for scans
alaskaSpecLib_AV_df_equal25   <-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpecLib_AV_df_equal25.csv"  )
alaskaSpeclib_AV_VIs_equal25  <-read.csv("Test_Outputs/3_AV_Imagery/1_processing/alaskaSpeclib_AV_VIs_equal25.csv" )

##Make names for colnames in each df unique
colnames(alaskaSpecLib_AV_df_equal25)[-1:-7]<-paste0(colnames(alaskaSpecLib_AV_df_equal25)[-1:-7],"_raw"        )
colnames(alaskaSpeclib_AV_VIs_equal25  )[-1:-7]<-paste0(colnames(alaskaSpeclib_AV_VIs_equal25  )[-1:-7],"_VIs"  )

##Let's merge these dataframes
alaskaSpecLib_data_AV<-Reduce(cbind,list(alaskaSpecLib_AV_df_equal25
                                         ,alaskaSpeclib_AV_VIs_equal25  [-1:-7]))
##Lets save this dataframe
write.csv(alaskaSpecLib_data_AV,"Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpecLib_data_AV.csv",row.names = FALSE)

