#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for scans
alaskaSpeclib_HDW_010nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_010nm_equal25.csv")
alaskaSpeclib_HDW_050nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_050nm_equal25.csv")
alaskaSpeclib_HDW_100nm_equal25<-read.csv("Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_100nm_equal25.csv")
alaskaSpeclib_HDW_VIs_equal25  <-read.csv("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW_VIs_equal25.csv" )

##Make names for colnames in each df unique
colnames(alaskaSpeclib_HDW_010nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_010nm_equal25)[-1:-7],"_010nm")
colnames(alaskaSpeclib_HDW_050nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_050nm_equal25)[-1:-7],"_050nm")
colnames(alaskaSpeclib_HDW_100nm_equal25)[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_100nm_equal25)[-1:-7],"_100nm")
colnames(alaskaSpeclib_HDW_VIs_equal25  )[-1:-7]<-paste0(colnames(alaskaSpeclib_HDW_VIs_equal25  )[-1:-7],"_VIs"  )

##Let's merge these dataframes
alaskaSpecLib_data_HDW<-Reduce(cbind,list(alaskaSpeclib_HDW_010nm_equal25
                                         ,alaskaSpeclib_HDW_050nm_equal25[-1:-7]
                                         ,alaskaSpeclib_HDW_100nm_equal25[-1:-7]
                                         ,alaskaSpeclib_HDW_VIs_equal25  [-1:-7]))
##Lets save this dataframe
write.csv(alaskaSpecLib_data_HDW,"Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_data_HDW.csv",row.names = FALSE)

