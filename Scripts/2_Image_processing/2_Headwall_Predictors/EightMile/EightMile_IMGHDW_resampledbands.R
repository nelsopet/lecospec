####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe 
EightMile_IMG_HDW<-brick("Original_data/EightMile/EightMile_clippedHalf")%>%rasterToPoints()%>%as.data.frame()

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Field_Spec/Headwall_wv", numeric())

##lets remove all those bads that had noise
EightMile_IMG_HDW[275:328]<-NULL

##change colnames to correct band names
colnames(EightMile_IMG_HDW)[-1:-2]<-HDW_ng_wv

##Remove all pixels with NA values
EightMile_IMG_HDW<-na.omit(EightMile_IMG_HDW)

##Now lets check the range of the values in the image
test<-lapply(EightMile_IMG_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
#test%>%View()
test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values

##create a datframe with the coordinates for imagery to be used later
cords<-EightMile_IMG_HDW%>%dplyr::select(1,2)

##Lets remove this row
#alaskaSpeclib_HDW_50nm<-alaskaSpeclib_HDW_50nm%>%subset(`529.444`>0) ##dim()  1974  333
##you could run logical test above just to check the dataset before moving on
##Do the same steps above for imagery
EightMile_IMG_HDW_010nm<-EightMile_IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,10 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
EightMile_IMG_HDW_050nm<-EightMile_IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,50 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
EightMile_IMG_HDW_100nm<-EightMile_IMG_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

###Lets run logical test for all dataframes
tst2<-lapply(EightMile_IMG_HDW_010nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2%>%subset(V1<0)%>%view()

#Don't need to run 3 lines below unless there are weird values in dataset above
EightMile_IMG_HDW_010nm[-1:-2]%>%
  dplyr::select(`399.444`)%>% 
  subset(`399.444`<0)%>% nrow() ##2 rows have negative values

##Lets remove these rows
EightMile_IMG_HDW_010nm<-EightMile_IMG_HDW_010nm%>%subset(`399.444`>0)

###Lets run that test on "EightMile_IMG_HDW_50nm"
tst3<-lapply(EightMile_IMG_HDW_050nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3%>%subset(V1<0)%>%view()

#Don't need to run 3 lines below unless there are weird values in dataset above
EightMile_IMG_HDW_050nm[-1:-2]%>%
  dplyr::select(`399.444`)%>% 
  subset(`399.444`<0)%>% nrow() ##2 rows have negative values

##Lets remove these rows
EightMile_IMG_HDW_050nm<-EightMile_IMG_HDW_010nm%>%subset(`399.444`>0)

###Lets run that test on "EightMile_IMG_HDW_100nm"
tst4<-lapply(EightMile_IMG_HDW_100nm[-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4%>%subset(V1<0)%>%view()

#Don't need to run 3 lines below unless there are weird values in dataset above
EightMile_IMG_HDW_100nm[-1:-2]%>%
  dplyr::select(`399.444`)%>% 
  subset(`399.444`<0)%>% nrow() ##2 rows have negative values

###Lets save our new dfs
write.csv(EightMile_IMG_HDW       ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_df.csv"    ,row.names = FALSE)
write.csv(EightMile_IMG_HDW_010nm ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_010nm.csv" ,row.names = FALSE)
write.csv(EightMile_IMG_HDW_050nm ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_050nm.csv" ,row.names = FALSE)
write.csv(EightMile_IMG_HDW_100nm ,"Outputs/2_HDW_Imagery/1_Processing/EightMile_IMG/EightMile_IMG_HDW_100nm.csv" ,row.names = FALSE)










