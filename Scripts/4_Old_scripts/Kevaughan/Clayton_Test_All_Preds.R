#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
EightMile_HDW_df  <-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_df.csv"  )
EightMile_HDW_05nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_05nm.csv")
EightMile_HDW_10nm<-read.csv("Outputs/2_HDW_Imagery/1_Processing/EightMile_HDW_10nm.csv")
EightMile_VIs_A   <-read.csv("Outputs/2_HDW_Imagery/1_processing/EightMile_HDW_VIs.csv" )

##Make names for colnames in each df unique
colnames(EightMile_HDW_df  )[-1:-2]<-paste0(colnames(EightMile_HDW_df  )[-1:-2],"_raw" )
colnames(EightMile_HDW_05nm)[-1:-2]<-paste0(colnames(EightMile_HDW_05nm)[-1:-2],"_05nm")
colnames(EightMile_HDW_10nm)[-1:-2]<-paste0(colnames(EightMile_HDW_10nm)[-1:-2],"_10nm")
colnames(EightMile_VIs_A   )[-1:-2]<-paste0(colnames(EightMile_VIs_A   )[-1:-2],"_VIs" )

##Let's merge these dataframes
EightMile_data_HDW<-Reduce(cbind,list(EightMile_HDW_df  
                                      ,EightMile_HDW_05nm[-1:-2]
                                      ,EightMile_HDW_10nm[-1:-2]
                                      ,EightMile_VIs_A   [-1:-2]))
###Lets save dataframe
write.csv(EightMile_data_HDW    ,"Outputs/2_HDW_Imagery/1_processing/EightMile_data_HDW.csv",row.names = FALSE)




