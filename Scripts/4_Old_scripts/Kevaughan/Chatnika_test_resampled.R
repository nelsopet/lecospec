####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe
ChatnikaN_HDW<-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW_N")%>%rasterToPoints()%>%as.data.frame()

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##lets remove all those bads that had noise
ChatnikaN_HDW[275:328]<-NULL

##change colnames to correct band names
colnames(ChatnikaN_HDW)[-1:-2]<-HDW_ng_wv

##create a datframe with the coordinates for imagery to be used later
cords<-ChatnikaN_HDW%>%dplyr::select(1,2)

##Lets remove this row
#alaskaSpeclib_HDW_10nm<-alaskaSpeclib_HDW_10nm%>%subset(`529.444`>0) ##dim()  1974  333
##you could run logical test above just to check the dataset before moving on
##Do the same steps above for imagery
ChatnikaN_HDW_05nm<-ChatnikaN_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,5 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
ChatnikaN_HDW_10nm<-ChatnikaN_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

####Lets run logical test for both datarames
tst2<-lapply(ChatnikaN_HDW_05nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2

####Lets run that test on "ChatnikaN_HDW_10nm"
tst3<-lapply(ChatnikaN_HDW_10nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2

###Lets save our new dfs
write.csv(ChatnikaN_HDW     ,"Outputs/2_HDW_Imagery/1_Processing/ChatnikaN_HDW_dfN.csv"  ,row.names = FALSE)
write.csv(ChatnikaN_HDW_05nm,"Outputs/2_HDW_Imagery/1_Processing/ChatnikaN_HDW_05nmN.csv",row.names = FALSE)
write.csv(ChatnikaN_HDW_10nm,"Outputs/2_HDW_Imagery/1_Processing/ChatnikaN_HDW_10nmN.csv",row.names = FALSE)
