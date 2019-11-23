##################################Calculates Principal Components for the spectral library that was resampled on our headwall bandpasses#####################################
library(spectrolab)
library(raster)
library(tidyverse)

##Reads in Imagery as a dataframe...
Clayton_test_HDW<-brick("Original_data/Test_imagery/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in spectral library....dim 1975  333
alaskaSpeclib_HDW<-readRDS("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW.rds")%>%as.data.frame()%>%dplyr::select(-sample_name)

##Please note there are the number of samples per functional group within our spectral library
#Abiotic Dwarf Shrub    Forb    Graminoid     Lichen        Moss       Shrub        Tree 
#58         992          80          25         464          90         241          25 

###First we want to make the amount of scans among functional groups equivalent...I'll make it 25 since that's the lowest amount of samples we have
alaskaSpecLib_HDW_equal25<-alaskaSpeclib_HDW %>% group_by(PFT_3) %>% sample_n(25,replace = TRUE)
       
##Do PCA calculation on spectral library and image..this calculates the PCA
##this function generates the principal components for "alaskaSpeclib_HDW"
HDW_PCAspeclib <- prcomp(alaskaSpecLib_HDW_equal25[-1:-7], retx=TRUE, center=TRUE, scale=TRUE)
HDW_PCAimage   <- prcomp(Clayton_test_HDW         [-1:-2], retx=TRUE, center=TRUE, scale=TRUE)

##Lets look at the explaine variance by all the PCAs calculated
expl.var_speclib     <- round(HDW_PCAspeclib$sdev^2/sum(HDW_PCAspeclib$sdev^2)*100)####Firts 4 PCs best explains the variability
expl.var_image       <- round(HDW_PCAimage  $sdev^2/sum(HDW_PCAimage$sdev^2)*100)###Firts 7 PCs best explains the variability

####fist 4 PC's best explains the variance
#creates dataframe with first 4 PCA vales for each species
HDW_PCAspeclib<-cbind(as.data.frame(alaskaSpecLib_HDW_equal25[1:7]),as.data.frame(HDW_PCAspeclib  $x[,1:7]))
HDW_PCAimage  <-cbind(as.data.frame(Clayton_test_HDW[1:2])         ,as.data.frame(HDW_PCAimage    $x[,1:7]))


##Lets save these new PCA dataframes
write.csv(HDW_PCAspeclib,"Outputs/2_HDW_Imagery/1_Processing/HDW_PCAspeclib.csv",row.names = FALSE)
write.csv(HDW_PCAimage  ,"Outputs/2_HDW_Imagery/1_Processing/HDW_PCAimage.csv"  ,row.names = FALSE)
