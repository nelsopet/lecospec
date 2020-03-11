#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
EightMile_HDW_010nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_010nm.csv")
EightMile_HDW_050nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_050nm.csv")
EightMile_HDW_100nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_100nm.csv")
EightMile_VIs_A    <-read.csv("Outputs/2_HDW_Imagery/1_processing/EightMile_HDW_VIs.csv"  )

##Make names for colnames in each df unique
colnames(EightMile_HDW_010nm)[-1:-2]<-paste0(colnames(EightMile_HDW_010nm)[-1:-2],"_010nm")
colnames(EightMile_HDW_050nm)[-1:-2]<-paste0(colnames(EightMile_HDW_050nm)[-1:-2],"_050nm")
colnames(EightMile_HDW_100nm)[-1:-2]<-paste0(colnames(EightMile_HDW_100nm)[-1:-2],"_100nm")
colnames(EightMile_VIs_A    )[-1:-2]<-paste0(colnames(EightMile_VIs_A    )[-1:-2],"_VIs"  )

##Need to make sure that those pixels that were removed in the vegindex calculation are removed here
##doing a simple inner_join should solve this

EightMile_data_HDW<-inner_join(EightMile_VIs_A   ,EightMile_HDW_010nm,by=c("x","y"))
EightMile_data_HDW<-inner_join(EightMile_data_HDW,EightMile_HDW_050nm,by=c("x","y"))
EightMile_data_HDW<-inner_join(EightMile_data_HDW,EightMile_HDW_100nm,by=c("x","y"))

###Lets save dataframe
write.csv(EightMile_data_HDW    ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_data_HDW.csv",row.names = FALSE)




