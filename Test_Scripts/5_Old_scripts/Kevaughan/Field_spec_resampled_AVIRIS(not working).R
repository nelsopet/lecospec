####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)

##Reads in spectral library as a dataframe
##this is the spectral library that had all uncalibrated bands removed
alaskaSpeclib_AV_df<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpecLib_AV_df_equal25.csv",check.names = F)

alaskaSpeclib_AV<-alaskaSpeclib_AV_df[-1:-7]%>%as.spectra()

##Now lets resample every 5nm and 10nm
##we need to do this for the image as well
alaskaSpeclib_AV_010nm<-alaskaSpeclib_AV%>%spectrolab::resample(seq(399.444,899.424,10 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_AV_050nm<-alaskaSpeclib_AV%>%spectrolab::resample(seq(399.444,899.424,50 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_AV_100nm<-alaskaSpeclib_AV%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%dplyr::select(-sample_name)

alaskaSpeclib_AV_010nm<-cbind(alaskaSpeclib_AV_df[1:7],alaskaSpeclib_AV_010nm)
alaskaSpeclib_AV_050nm<-cbind(alaskaSpeclib_AV_df[1:7],alaskaSpeclib_AV_050nm)
alaskaSpeclib_AV_100nm<-cbind(alaskaSpeclib_AV_df[1:7],alaskaSpeclib_AV_100nm)

####Lets run logical test for both datarames
tst2<-lapply(alaskaSpeclib_AV_010nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2 %>% subset(V1 <0) %>% View()

#alaskaSpeclib_AV_05nm[-1:-7] %>% #dim() ] 1975  333
#  dplyr::select(`444.444`) %>% 
#  subset(`444.444`<0) %>% nrow() ##There is only one row here that has negative values, we could try this on multiple columns
##all those columns that we know have rows that have negative values

##Lets remove this row
#alaskaSpeclib_AV_05nm<-alaskaSpeclib_AV_05nm%>%subset(`444.444`>0) ##dim()  1974  333
##you could run logical test above just to check the dataset before moving on

####Lets run that test on "alaskaSpeclib_AV_10nm"
tst3<-lapply(alaskaSpeclib_AV_050nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this

####Lets run that test on "alaskaSpeclib_AV_10nm"
tst4<-lapply(alaskaSpeclib_AV_100nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this

#alaskaSpeclib_AV_10nm[-1:-7] %>% #dim() ] 1975  333
#  dplyr::select(`529.444`) %>% 
#  subset(`529.444`<0) %>% nrow() ##There is only one row here that has negative values, we could try this on multiple columns
##all those columns that we know have rows that have negative values

###Lets save our new dfs
write.csv(alaskaSpeclib_AV_010nm,"Test_Outputs/2_AV_Imagery/1_Processing/alaskaSpeclib_AV_010nm_equal25.csv",row.names = FALSE)
write.csv(alaskaSpeclib_AV_050nm,"Test_Outputs/2_AV_Imagery/1_Processing/alaskaSpeclib_AV_050nm_equal25.csv",row.names = FALSE)
write.csv(alaskaSpeclib_AV_100nm,"Test_Outputs/2_AV_Imagery/1_Processing/alaskaSpeclib_AV_100nm_equal25.csv",row.names = FALSE)
