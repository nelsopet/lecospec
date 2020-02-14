#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
Test_IMG_AV_df   <-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/Test_IMG_AV_df.csv"   )
Test_IMG_VIs_A   <-read.csv("Test_Outputs/3_AV_Imagery/1_processing/Test_IMG_AV_VIs.csv"  )

##Make names for colnames in each df unique
colnames(Test_IMG_AV_df   )[-1:-2]<-paste0(colnames(Test_IMG_AV_df    )[-1:-2],"_raw" )
colnames(Test_IMG_VIs_A   )[-1:-2]<-paste0(colnames(Test_IMG_VIs_A    )[-1:-2],"_VIs" )

##Let's merge these dataframes
Test_IMG_data_AV<-Reduce(cbind,list(Test_IMG_AV_df  
                                   ,Test_IMG_VIs_A    [-1:-2]))
###Lets save dataframe
write.csv(Test_IMG_data_AV    ,"Test_Outputs/3_AV_Imagery/1_processing/Test_IMG_data_AV.csv",row.names = FALSE)




