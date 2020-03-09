####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe
Clayton_Test_HDW<-brick("Original_data/Test_imagery_HDW/Clayton_Test_clipped_HDW")
##%>%rasterToPoints()%>%as.data.frame()

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##lets remove all those bads that had noise
Clayton_Test_HDW[275:328]<-NULL

##change colnames to correct band names
colnames(Clayton_Test_HDW)[-1:-2]<-HDW_ng_wv

##create a datframe with the coordinates for imagery to be used later
cords<-Clayton_Test_HDW%>%dplyr::select(1,2)

##Lets remove this row
#alaskaSpeclib_HDW_10nm<-alaskaSpeclib_HDW_10nm%>%subset(`529.444`>0) ##dim()  1974  333
##you could run logical test above just to check the dataset before moving on
##Do the same steps above for imagery
Clayton_Test_HDW_05nm<-Clayton_Test_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,5 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
Clayton_Test_HDW_10nm<-Clayton_Test_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

####Lets run logical test for both datarames
tst2<-lapply(Clayton_Test_HDW_05nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2

####Lets run that test on "Clayton_Test_HDW_10nm"
tst3<-lapply(Clayton_Test_HDW_10nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2

###Lets save our new dfs
write.csv(Clayton_Test_HDW     ,"Outputs/2_HDW_Imagery/1_Processing/Clayton_Test_HDW_df.csv"  ,row.names = FALSE)
write.csv(Clayton_Test_HDW_05nm,"Outputs/2_HDW_Imagery/1_Processing/Clayton_Test_HDW_05nm.csv",row.names = FALSE)
write.csv(Clayton_Test_HDW_10nm,"Outputs/2_HDW_Imagery/1_Processing/Clayton_Test_HDW_10nm.csv",row.names = FALSE)
