####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe
Clayton_test_HDW<-brick("Original_data/Test_imagery/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in spectral library as a dataframe
##this is the spectral library that had all uncalibrated bands removed
alaskaSpeclib<-readRDS("Outputs/1_Field_Spec/1_Processing/alaskaSpeclib.rds")

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##change colnames to correct band names
colnames(Clayton_test_HDW)[-1:-2]<-HDW_ng_wv

cords<-Clayton_test_HDW%>%dplyr::select(1,2)

##Now we want to resample alsakSpeclib based on the band passes
alaskaSpeclib_HDW<-spectrolab::resample(alaskaSpeclib,HDW_ng_wv)

##Now lets smooth the spectral library and resample every 5nm and 10nm
##we need to do this for the image as well
alaskaSpeclib_HDW_05nm<-smooth(alaskaSpeclib_HDW)%>%spectrolab::resample(seq(399.444,999.42,5 ))%>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_HDW_10nm<-smooth(alaskaSpeclib_HDW)%>%spectrolab::resample(seq(399.444,999.42,10))%>%as.data.frame()%>%dplyr::select(-sample_name)

Clayton_test_HDW_05nm<-Clayton_test_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%smooth()%>%spectrolab::resample(seq(399.444,999.42,5 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
Clayton_test_HDW_10nm<-Clayton_test_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%smooth()%>%spectrolab::resample(seq(399.444,999.42,10))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

##Now lets make sure we have equal scans per functional group in our df
alaskaSpeclib_HDW_05nm_equal25<-alaskaSpeclib_HDW_05nm%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
alaskaSpeclib_HDW_10nm_equal25<-alaskaSpeclib_HDW_10nm%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)

###Lets save our new dfs
write.csv(alaskaSpeclib_HDW_05nm_equal25,"Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_05nm_equal25.csv",row.names = FALSE)
write.csv(alaskaSpeclib_HDW_10nm_equal25,"Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_10nm_equal25.csv",row.names = FALSE)
write.csv(Clayton_test_HDW_05nm         ,"Outputs/2_HDW_Imagery/1_Processing/Clayton_test_HDW_05nm.csv"         ,row.names = FALSE)
write.csv(Clayton_test_HDW_10nm         ,"Outputs/2_HDW_Imagery/1_Processing/Clayton_test_HDW_10nm.csv"         ,row.names = FALSE)













































