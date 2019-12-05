####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image as dataframe
Clayton_test_HDW<-brick("Original_data/Test_imagery_HDW/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in spectral library as a dataframe
##this is the spectral library that had all uncalibrated bands removed
alaskaSpeclib_HDW<-readRDS("Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW.rds")

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##change colnames to correct band names
colnames(Clayton_test_HDW)[-1:-2]<-HDW_ng_wv

##create a datframe with the coordinates for imagery to be used later
cords<-Clayton_test_HDW%>%dplyr::select(1,2)

##Now lets resample every 5nm and 10nm
##we need to do this for the image as well
alaskaSpeclib_HDW_05nm<-alaskaSpeclib_HDW%>%spectrolab::resample(seq(399.444,999.42,5 ))%>%as.data.frame()%>%dplyr::select(-sample_name)
alaskaSpeclib_HDW_10nm<-alaskaSpeclib_HDW%>%spectrolab::resample(seq(399.444,999.42,10))%>%as.data.frame()%>%dplyr::select(-sample_name)

####Lets run logical test for both datarames
tst2<-lapply(alaskaSpeclib_HDW_05nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst2 %>% subset(V1 <0) %>% View()

#alaskaSpeclib_HDW_05nm[-1:-7] %>% #dim() ] 1975  333
#  dplyr::select(`444.444`) %>% 
#  subset(`444.444`<0) %>% nrow() ##There is only one row here that has negative values, we could try this on multiple columns
                                 ##all those columns that we know have rows that have negative values

##Lets remove this row
#alaskaSpeclib_HDW_05nm<-alaskaSpeclib_HDW_05nm%>%subset(`444.444`>0) ##dim()  1974  333
                                                                     ##you could run logical test above just to check the dataset before moving on

####Lets run that test on "alaskaSpeclib_HDW_10nm"
tst3<-lapply(alaskaSpeclib_HDW_10nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2
#tst3 %>% subset(V1 <0) %>% View() ##There a bunch of negative values across 128 columns, this might be one row, lets test this

#alaskaSpeclib_HDW_10nm[-1:-7] %>% #dim() ] 1975  333
#  dplyr::select(`529.444`) %>% 
#  subset(`529.444`<0) %>% nrow() ##There is only one row here that has negative values, we could try this on multiple columns
                                 ##all those columns that we know have rows that have negative values

##Lets remove this row
#alaskaSpeclib_HDW_10nm<-alaskaSpeclib_HDW_10nm%>%subset(`529.444`>0) ##dim()  1974  333
                                                                     ##you could run logical test above just to check the dataset before moving on
##Do the same steps above for imagery
Clayton_test_HDW_05nm<-Clayton_test_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,999.42,5 ))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)
Clayton_test_HDW_10nm<-Clayton_test_HDW%>%dplyr::select(-x,-y)%>%spectrolab::as.spectra()%>%spectrolab::resample(seq(399.444,999.42,10))%>%as.data.frame()%>%cbind(cords)%>%dplyr::select(x,y,everything())%>%dplyr::select(-sample_name)

####Lets run logical test for both datarames
tst2<-lapply(Clayton_test_HDW_05nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst2$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst2$V2%>%range()##There are no weird values, those are values outside of 0 and 2

####Lets run that test on "Clayton_test_HDW_10nm"
tst3<-lapply(Clayton_test_HDW_10nm[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst3$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst3$V2%>%range()##There are no weird values, those are values outside of 0 and 2

##Now lets make sure we have equal scans per functional group in our df
alaskaSpeclib_HDW_05nm_equal25<-alaskaSpeclib_HDW_05nm%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
alaskaSpeclib_HDW_10nm_equal25<-alaskaSpeclib_HDW_10nm%>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)

###Lets save our new dfs
write.csv(alaskaSpeclib_HDW_05nm_equal25,"Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_05nm_equal25.csv",row.names = FALSE)
write.csv(alaskaSpeclib_HDW_10nm_equal25,"Outputs/2_HDW_Imagery/1_Processing/alaskaSpeclib_HDW_10nm_equal25.csv",row.names = FALSE)
write.csv(Clayton_test_HDW_05nm         ,"Outputs/2_HDW_Imagery/1_Processing/Clayton_test_HDW_05nm.csv"         ,row.names = FALSE)
write.csv(Clayton_test_HDW_10nm         ,"Outputs/2_HDW_Imagery/1_Processing/Clayton_test_HDW_10nm.csv"         ,row.names = FALSE)













































