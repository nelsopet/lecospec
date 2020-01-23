#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for scans
alaskaSpeclib_HDW_010nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_010nm.csv")
alaskaSpeclib_HDW_050nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_050nm.csv")
alaskaSpeclib_HDW_100nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_100nm.csv")
alaskaSpeclib_HDW_VIs  <-read.csv("Test_Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs.csv" )

##Make names for colnames in each df unique
colnames(alaskaSpeclib_HDW_010nm)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_010nm)[-1:-7],"_010nm")
colnames(alaskaSpeclib_HDW_050nm)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_050nm)[-1:-7],"_050nm")
colnames(alaskaSpeclib_HDW_100nm)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_100nm)[-1:-7],"_100nm")
colnames(alaskaSpeclib_HDW_VIs  )[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_VIs  )[-1:-7],"_VIs"  )

##Let's merge these dataframes
alaskaSpecLib_data_HDW<-Reduce(cbind,list(alaskaSpeclib_HDW_010nm
                                         ,alaskaSpeclib_HDW_050nm[-1:-7]
                                         ,alaskaSpeclib_HDW_100nm[-1:-7]
                                         ,alaskaSpeclib_HDW_VIs  [-1:-7]))
##Lets save this dataframe
write.csv(alaskaSpecLib_data_HDW,"Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_data_HDW.csv",row.names = FALSE)

