#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for scans
alaskaSpeclib_AV_010nm_equal25<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpeclib_AV_010nm_equal25.csv")
alaskaSpeclib_AV_050nm_equal25<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpeclib_AV_050nm_equal25.csv")
alaskaSpeclib_AV_100nm_equal25<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpeclib_AV_100nm_equal25.csv")
alaskaSpeclib_AV_VIs_equal25  <-read.csv("Test_Outputs/3_AV_Imagery/1_processing/alaskaSpeclib_AV_VIs_equal25.csv" )

##Make names for colnames in each df unique
colnames(alaskaSpeclib_AV_010nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_AV_010nm_equal25)[-1:-7],"_010nm")
colnames(alaskaSpeclib_AV_050nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_AV_050nm_equal25)[-1:-7],"_050nm")
colnames(alaskaSpeclib_AV_100nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_AV_100nm_equal25)[-1:-7],"_100nm")
colnames(alaskaSpeclib_AV_VIs_equal25  )[-1:-7]<-paste0(colnames(alaskaSpeclib_AV_VIs_equal25  )[-1:-7],"_VIs"  )

##Let's merge these dataframes
alaskaSpecLib_data_AV<-Reduce(cbind,list(alaskaSpeclib_AV_010nm_equal25
                                         ,alaskaSpeclib_AV_050nm_equal25[-1:-7]
                                         ,alaskaSpeclib_AV_100nm_equal25[-1:-7]
                                         ,alaskaSpeclib_AV_VIs_equal25  [-1:-7]))
##Lets save this dataframe
write.csv(alaskaSpecLib_data_AV,"Test_Outputs/2_AV_Imagery/1_Processing/alaskaSpecLib_data_AV.csv",row.names = FALSE)

