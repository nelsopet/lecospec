#######################Combines all predictiors into one dataframe######################################
library(spectrolab)
library(tidyverse)
library(hsdar)

###Reads in all predictors for Imagery
EightMileTest_AV    <-read.csv("Outputs/2_Imagery/AVIRIS/Processing/EightMileTest_AV_df.csv" )
EightMileTest_AV_VIs<-read.csv("Outputs/2_Imagery/AVIRIS/Processing/EightMileTest_AV_VIs.csv")

##Make names for colnames in each df unique
colnames(EightMileTest_AV    )[-1:-2]<-paste0(colnames(EightMileTest_AV    )[-1:-2],"_raw" )
colnames(EightMileTest_AV_VIs)[-1:-2]<-paste0(colnames(EightMileTest_AV_VIs)[-1:-2],"_VIs" )

##Let's merge these dataframes
EightMileTest_IMG_data_AV<-Reduce(cbind,list(EightMileTest_AV    
                                            ,EightMileTest_AV_VIs[-1:-2]))
###Lets save dataframe
write.csv(EightMileTest_IMG_data_AV    ,"Outputs/2_Imagery/AVIRIS/Processing/EightMileTest_IMG_data_AV.csv",row.names = FALSE)




