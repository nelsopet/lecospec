####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe 
Chatnika_HDW<-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##lets remove all those bads that had noise
Chatnika_HDW[275:328]<-NULL

##change colnames to correct band names
colnames(Chatnika_HDW)[-1:-2]<-HDW_ng_wv

##Now lets check the range of the values in the image
test<-lapply(Chatnika_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#test%>%View()
test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values

Chatnika_HDW<-na.omit(Chatnika_HDW)

##create a datframe with the coordinates for imagery to be used later
cords<-Chatnika_HDW%>%dplyr::select(1,2)

##Lets remove this row
#alaskaSpeclib_HDW_50nm<-alaskaSpeclib_HDW_50nm%>%subset(`529.444`>0) ##dim()  1974  333
##you could run logical test above just to check the dataset before moving on
##Do the same steps above for imagery
Chatnika_HDW_010nm<-Chatnika_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
Chatnika_HDW_050nm<-Chatnika_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,50 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
Chatnika_HDW_100nm<-Chatnika_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

####Lets run logical test for all dataframes
#tst2<-lapply(Chatnika_HDW_010nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2%>%subset(V1<0)%>%view()

#Chatnika_HDW_010nm[-1:-2]%>%
#  dplyr::select(`397.593`)%>% 
#  subset(`397.593`<0)%>% nrow() ##58842 have negative values

#DF[!rowSums(DF < 0), ]

####Lets run that test on "Chatnika_HDW_50nm"
#tst3<-lapply(Chatnika_HDW_050nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3%>%subset(V1<0)%>%view()

#Chatnika_HDW_050nm[-1:-2]%>%
#  dplyr::select(`397.593`)%>% 
#  subset(`397.593`<0)%>% nrow() ##58842 rows have negative values

#DF[!rowSums(DF < 0), ]

####Lets run that test on "Chatnika_HDW_50nm"
#tst4<-lapply(Chatnika_HDW_100nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4%>%subset(V1<0)%>%view()
#DF[!rowSums(DF < 0), ]

###Lets save our new dfs
write.csv(Chatnika_HDW       ,"Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_df.csv"    ,row.names = FALSE)
write.csv(Chatnika_HDW_010nm ,"Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_010nm.csv" ,row.names = FALSE)
write.csv(Chatnika_HDW_050nm ,"Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_050nm.csv" ,row.names = FALSE)
write.csv(Chatnika_HDW_100nm ,"Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_100nm.csv" ,row.names = FALSE)










