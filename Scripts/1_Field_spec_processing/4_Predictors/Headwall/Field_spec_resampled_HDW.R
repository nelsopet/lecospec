####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)

##Reads in spectral library as a dataframe
##this is the spectral library that had all uncalibrated bands removed
alaskaSpeclib_HDW_df<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpecLib_HDW_df.csv",check.names = F)

alaskaSpeclib_HDW<-alaskaSpeclib_HDW_df[-1:-7]%>%as.spectra()

##Now lets resample every 5nm and 10nm
##we need to do this for the image as well
alaskaSpeclib_HDW_005nm<-alaskaSpeclib_HDW%>%spectrolab::resample(seq(399.444,899.424,5  )) %>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_HDW_010nm<-alaskaSpeclib_HDW%>%spectrolab::resample(seq(399.444,899.424,10 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_HDW_050nm<-alaskaSpeclib_HDW%>%spectrolab::resample(seq(399.444,899.424,50 )) %>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_HDW_100nm<-alaskaSpeclib_HDW%>%spectrolab::resample(seq(399.444,899.424,100))%>%as.data.frame()%>%dplyr::select(-sample_name)

alaskaSpeclib_HDW_005nm<-cbind(alaskaSpeclib_HDW_df[1:7],alaskaSpeclib_HDW_005nm)
alaskaSpeclib_HDW_010nm<-cbind(alaskaSpeclib_HDW_df[1:7],alaskaSpeclib_HDW_010nm)
alaskaSpeclib_HDW_050nm<-cbind(alaskaSpeclib_HDW_df[1:7],alaskaSpeclib_HDW_050nm)
alaskaSpeclib_HDW_100nm<-cbind(alaskaSpeclib_HDW_df[1:7],alaskaSpeclib_HDW_100nm)

####Lets run logical test for all datarames
tst5<-lapply(alaskaSpeclib_HDW_050nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst5$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst5$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2 %>% subset(V1 <0) %>% View()

####Lets run logical test for all datarames
tst2<-lapply(alaskaSpeclib_HDW_010nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2 %>% subset(V1 <0) %>% View()

####Lets run that test on "alaskaSpeclib_HDW_10nm"
tst3<-lapply(alaskaSpeclib_HDW_050nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this

####Lets run that test on "alaskaSpeclib_HDW_10nm"
tst4<-lapply(alaskaSpeclib_HDW_100nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst4$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst4$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst4 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this

###Lets save our new dfs
write.csv(alaskaSpeclib_HDW_005nm,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_005nm.csv",row.names = FALSE)
write.csv(alaskaSpeclib_HDW_010nm,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_010nm.csv",row.names = FALSE)
write.csv(alaskaSpeclib_HDW_050nm,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_050nm.csv",row.names = FALSE)
write.csv(alaskaSpeclib_HDW_100nm,"Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_100nm.csv",row.names = FALSE)
