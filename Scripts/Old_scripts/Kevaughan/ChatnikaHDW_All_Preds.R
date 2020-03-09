#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
Chatnika_HDW_010nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_010nm.csv")
Chatnika_HDW_050nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_050nm.csv")
Chatnika_HDW_100nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_100nm.csv")
Chatnika_VIs_A    <-read.csv("Outputs/2_HDW_Imagery/1_processing/Chatnika_HDW_VIs.csv"  )

##Make names for colnames in each df unique
colnames(Chatnika_HDW_010nm)[-1:-2]<-paste0(colnames(Chatnika_HDW_010nm)[-1:-2],"_010nm")
colnames(Chatnika_HDW_050nm)[-1:-2]<-paste0(colnames(Chatnika_HDW_050nm)[-1:-2],"_050nm")
colnames(Chatnika_HDW_100nm)[-1:-2]<-paste0(colnames(Chatnika_HDW_100nm)[-1:-2],"_100nm")
colnames(Chatnika_VIs_A    )[-1:-2]<-paste0(colnames(Chatnika_VIs_A    )[-1:-2],"_VIs"  )

##Need to make sure that those pixels that were removed in the vegindex calculation are removed here
##doing a simple inner_join should solve this

Chatnika_data_HDW<-inner_join(Chatnika_VIs_A   ,Chatnika_HDW_010nm,by=c("x","y"))
Chatnika_data_HDW<-inner_join(Chatnika_data_HDW,Chatnika_HDW_050nm,by=c("x","y"))
Chatnika_data_HDW<-inner_join(Chatnika_data_HDW,Chatnika_HDW_100nm,by=c("x","y"))

###Lets save dataframe
write.csv(Chatnika_data_HDW    ,"Outputs/2_HDW_Imagery/1_Processing/Chatnika_data_HDW.csv",row.names = FALSE)




