library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in spectral library....dim 1974  333
alaskaSpeclib_AV<-read.csv("Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpecLib_AV_df_equal25.csv")

####Lets run that test again
tst<-lapply(alaskaSpeclib_AV[-1:-7],range)%>%as.data.frame%>%t()%>%as.data.frame
tst$V1%>%range()##There are no weird values, those are values outside of 0 and 2
tst$V2%>%range()##There are no weird values, those are values outside of 0 and 2

##Reads in bandpasses for imagery to be used later
AV_ng_wv<-scan("Test_Outputs/3_AV_Imagery/1_Processing/AVIRIS_wv", numeric())

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
alaskaSpeclib_AV_matrix<-as.matrix(alaskaSpeclib_AV[-1:-7])

##lets check the column attributes to see if any weird values were introduced
alaskaSpeclib_AV_matrix%>%max()##values are fine you may proceed, i.e no negative values or values grater than 2,you'll ned to check min values using the function min()

##Now that we have our matrix we can create our spectralib object that will be used to create a df with all the veg indices
alaskaSpeclib_AV_speclib<-speclib(alaskaSpeclib_AV_matrix,AV_ng_wv)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()
VIs<-VIs[-58] ##Vegitation indices mREIP won't work so remove it from list

##Creates dataframe with Vegitation indices
alaskaSpeclib_AV_VIs<-vegindex(alaskaSpeclib_AV_speclib,index=VIs)

##lets do a logical test on alaskaSpeclib_AV_VIs to see if strange values exist
test4<-lapply(alaskaSpeclib_AV_VIs,range)%>%as.data.frame%>%t()%>%as.data.frame
test4%>%View()##There are NO columns where NaNs exist

##we need to combine the other columns with our new VI variables
alaskaSpeclib_AV_VIs<-cbind(alaskaSpeclib_AV[1:7],alaskaSpeclib_AV_VIs)

##Now that we have our VIs calculated we can go ahead and export these dataframes
write.csv(alaskaSpeclib_AV_VIs,"Test_Outputs/3_AV_Imagery/1_Processing/alaskaSpeclib_AV_VIs_equal25.csv",row.names = FALSE)
