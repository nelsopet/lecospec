##################################Calculates Principal Components for the spectral library that was resampled on our headwall bandpasses#####################################
library(spectrolab)
library(raster)
library(tidyverse)

##Reads in image as dataframe
Chatnika_HDW  <-brick("Original_data/Test_imagery_HDW/Chatnika_clipped_HDW"      )%>%rasterToPoints()%>%as.data.frame()

##This function generates the principal components for "Chatnika_HDW"
Chatnika_HDW_PCA     <- prcomp(Chatnika_HDW           [-1:-2], retx=TRUE, center=TRUE, scale=TRUE)

##Lets use 7 PC's
##Creates dataframe with first 4 PCA vales for each species
Chatnika_HDW_PCA  <-cbind(as.data.frame(Chatnika_HDW  [1:2]),as.data.frame(Chatnika_HDW_PCA  $x[,1:7]))

##Lets save these new PCA dataframes
write.csv(Chatnika_HDW_PCA    ,"Outputs/2_HDW_Imagery/1_Processing/Chatnika_HDW_PCA.csv"    ,row.names = FALSE)
