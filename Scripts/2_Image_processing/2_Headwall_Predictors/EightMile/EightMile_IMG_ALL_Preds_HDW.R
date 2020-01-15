#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
EightMile_HDW_010nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_010nm.csv")
EightMile_HDW_050nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_050nm.csv")
EightMile_HDW_100nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_100nm.csv")
EightMile_VIs_A    <-read.csv("Outputs/2_HDW_Imagery/1_processing/EightMile_IMG/EightMile_HDW_VIs.csv"      )

##Make names for colnames in each df unique
colnames(EightMile_HDW_010nm)[-1:-2]<-paste0(colnames(EightMile_HDW_010nm)[-1:-2],"_010nm")
colnames(EightMile_HDW_050nm)[-1:-2]<-paste0(colnames(EightMile_HDW_050nm)[-1:-2],"_050nm")
colnames(EightMile_HDW_100nm)[-1:-2]<-paste0(colnames(EightMile_HDW_100nm)[-1:-2],"_100nm" )
colnames(EightMile_VIs_A    )[-1:-2]<-paste0(colnames(EightMile_VIs_A    )[-1:-2],"_VIs" )

##Let's merge these dataframes
EightMile_data_HDW<-Reduce(cbind,list(EightMile_HDW_010nm
                                         ,EightMile_HDW_050nm[-1:-2]
                                         ,EightMile_HDW_100nm[-1:-2]
                                         ,EightMile_VIs_A    [-1:-2]))
###Lets save dataframe
write.csv(EightMile_data_HDW    ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_data_HDW.csv",row.names = FALSE)




