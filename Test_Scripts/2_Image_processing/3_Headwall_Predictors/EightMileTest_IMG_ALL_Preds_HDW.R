#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
EightMileTest_HDW_010nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/EightMileTest_IMG_HDW_010nm.csv")
EightMileTest_HDW_050nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/EightMileTest_IMG_HDW_050nm.csv")
EightMileTest_HDW_100nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/EightMileTest_IMG_HDW_100nm.csv")
EightMileTest_VIs_A    <-read.csv("Test_Outputs/2_HDW_Imagery/1_processing/EightMileTest_HDW_VIs.csv"      )

##Make names for colnames in each df unique
colnames(EightMileTest_HDW_010nm)[-1:-2]<-paste0(colnames(EightMileTest_HDW_010nm)[-1:-2],"_010nm")
colnames(EightMileTest_HDW_050nm)[-1:-2]<-paste0(colnames(EightMileTest_HDW_050nm)[-1:-2],"_050nm")
colnames(EightMileTest_HDW_100nm)[-1:-2]<-paste0(colnames(EightMileTest_HDW_100nm)[-1:-2],"_100nm" )
colnames(EightMileTest_VIs_A    )[-1:-2]<-paste0(colnames(EightMileTest_VIs_A    )[-1:-2],"_VIs" )

##Let's merge these dataframes
EightMileTest_data_HDW<-Reduce(cbind,list(EightMileTest_HDW_010nm
                                         ,EightMileTest_HDW_050nm[-1:-2]
                                         ,EightMileTest_HDW_100nm[-1:-2]
                                         ,EightMileTest_VIs_A    [-1:-2]))
###Lets save dataframe
write.csv(EightMileTest_data_HDW    ,"Test_Outputs/2_HDW_Imagery/1_processing/EightMileTest_data_HDW.csv",row.names = FALSE)




