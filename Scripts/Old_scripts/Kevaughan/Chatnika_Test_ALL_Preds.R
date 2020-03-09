#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
Chatnika_HDW_df  <-read.csv("Outputs/2_HDW_Imagery/1_Processing/ChatnikaN_HDW_dfN.csv"  )
Chatnika_HDW_05nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/ChatnikaN_HDW_05nmN.csv")
Chatnika_HDW_10nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/ChatnikaN_HDW_10nmN.csv")
Chatnika_VIs_A   <-read.csv("Outputs/2_HDW_Imagery/1_processing/ChatnikaN_HDW_VIsN.csv" )

##Make names for colnames in each df unique
colnames(Chatnika_HDW_df  )[-1:-2]<-paste0(colnames(Chatnika_HDW_df  )[-1:-2],"_raw" )
colnames(Chatnika_HDW_05nm)[-1:-2]<-paste0(colnames(Chatnika_HDW_05nm)[-1:-2],"_05nm")
colnames(Chatnika_HDW_10nm)[-1:-2]<-paste0(colnames(Chatnika_HDW_10nm)[-1:-2],"_10nm")
colnames(Chatnika_VIs_A   )[-1:-2]<-paste0(colnames(Chatnika_VIs_A   )[-1:-2],"_VIs" )

##Let's merge these dataframes
Chatnika_data_HDW<-Reduce(cbind,list(Chatnika_HDW_df  
                                      ,Chatnika_HDW_05nm[-1:-2]
                                      ,Chatnika_HDW_10nm[-1:-2]
                                      ,Chatnika_VIs_A   [-1:-2]))
###Lets save dataframe
write.csv(Chatnika_data_HDW    ,"Outputs/2_HDW_Imagery/1_processing/Chatnika_data_HDWN.csv",row.names = FALSE)




