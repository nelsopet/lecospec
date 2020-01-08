#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
Test_IMG_HDW_df   <-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/Test_IMG_HDW_df.csv"   )
Test_IMG_HDW_010nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/Test_IMG_HDW_010nm.csv")
Test_IMG_HDW_050nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/Test_IMG_HDW_050nm.csv")
Test_IMG_HDW_100nm<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/Test_IMG_HDW_100nm.csv")
Test_IMG_VIs_A    <-read.csv("Test_Outputs/2_HDW_Imagery/1_processing/Test_IMG_HDW_VIs.csv"  )

##Make names for colnames in each df unique
colnames(Test_IMG_HDW_df   )[-1:-2]<-paste0(colnames(Test_IMG_HDW_df   )[-1:-2],"_raw" )
colnames(Test_IMG_HDW_010nm)[-1:-2]<-paste0(colnames(Test_IMG_HDW_010nm)[-1:-2],"_010nm")
colnames(Test_IMG_HDW_050nm)[-1:-2]<-paste0(colnames(Test_IMG_HDW_050nm)[-1:-2],"_050nm")
colnames(Test_IMG_HDW_100nm)[-1:-2]<-paste0(colnames(Test_IMG_HDW_100nm)[-1:-2],"_100nm" )
colnames(Test_IMG_VIs_A    )[-1:-2]<-paste0(colnames(Test_IMG_VIs_A    )[-1:-2],"_VIs" )

##Let's merge these dataframes
Test_IMG_data_HDW<-Reduce(cbind,list(Test_IMG_HDW_df  
                                      ,Test_IMG_HDW_010nm[-1:-2]
                                      ,Test_IMG_HDW_050nm[-1:-2]
                                      ,Test_IMG_HDW_100nm[-1:-2]
                                      ,Test_IMG_VIs_A    [-1:-2]))
###Lets save dataframe
write.csv(Test_IMG_data_HDW    ,"Test_Outputs/2_HDW_Imagery/1_processing/Test_IMG_data_HDW.csv",row.names = FALSE)




