#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for scans
alaskaSpeclib_HDW_005nm<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_005nm.csv")
alaskaSpeclib_HDW_010nm<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_010nm.csv")
alaskaSpeclib_HDW_050nm<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_050nm.csv")
alaskaSpeclib_HDW_100nm<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_100nm.csv")
alaskaSpeclib_HDW_VIs  <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_VIs.csv"  )

##Make names for colnames in each df unique
colnames(alaskaSpeclib_HDW_005nm)[-1:-9]<-paste0(colnames(alaskaSpeclib_HDW_005nm)[-1:-9],"_005nm")
colnames(alaskaSpeclib_HDW_010nm)[-1:-9]<-paste0(colnames(alaskaSpeclib_HDW_010nm)[-1:-9],"_010nm")
colnames(alaskaSpeclib_HDW_050nm)[-1:-9]<-paste0(colnames(alaskaSpeclib_HDW_050nm)[-1:-9],"_050nm")
colnames(alaskaSpeclib_HDW_100nm)[-1:-9]<-paste0(colnames(alaskaSpeclib_HDW_100nm)[-1:-9],"_100nm")
colnames(alaskaSpeclib_HDW_VIs  )[-1:-9]<-paste0(colnames(alaskaSpeclib_HDW_VIs  )[-1:-9],"_VIs"  )

##Let's merge these dataframes
alaskaSpecLib_data_HDW<-Reduce(cbind,list(alaskaSpeclib_HDW_005nm
                                         ,alaskaSpeclib_HDW_010nm[-1:-9]
                                         ,alaskaSpeclib_HDW_050nm[-1:-9]
                                         ,alaskaSpeclib_HDW_100nm[-1:-9]
                                         ,alaskaSpeclib_HDW_VIs  [-1:-9]))

alaskaSpecLib_data_HDWALLbands<-Reduce(cbind,list(alaskaSpeclib_HDW_005nm
                                                 ,alaskaSpeclib_HDW_010nm[-1:-9]
                                                 ,alaskaSpeclib_HDW_050nm[-1:-9]
                                                 ,alaskaSpeclib_HDW_100nm[-1:-9]))
##Lets save this dataframe
write.csv(alaskaSpecLib_data_HDW,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpecLib_data_HDW.csv",row.names = FALSE)
write.csv(alaskaSpecLib_data_HDWALLbands,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpecLib_data_HDWALLbands.csv",row.names = FALSE)

