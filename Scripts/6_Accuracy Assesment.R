# ------------------------------------------- Accuracy assesment ---------------------------------------------------
library(tidyverse)
library(raster)
library(hsdar)
library(sf)

# Reads in raster
Predlayer<-raster("Output/FullDatacube/FullDatacube_PredLayer.tif")

# Plots Predicted layer
# plot(Predlayer)

# Grabs attribute table from image
AttributeTab<-Predlayer@data@attributes%>%as.data.frame()

# Change column name so we can join later
names(AttributeTab)[1]<-"Class"
AttributeTab$category<-AttributeTab$category%>%as.character()
AttributeTab$category[AttributeTab$category == ""] <- "NODATA"

# Imports polygons (quadrats) as shapefiles
Quadrats<-readOGR("Data/Quadrats.shp")

# Extract pixel values based on polygons (for each quadrat)
Masklayer<-extract(Predlayer,Quadrats, df =T)

# Changes column name so we can join later
names(Masklayer)[2]<-"Class"
Masklayer[is.na(Masklayer)]<- 0

# Joins atrribute table info with quadrat table info
Masklayer2<-inner_join(Masklayer,AttributeTab, by = "Class" )

# Name of all the classes
classes<-c(AttributeTab$category%>%unique())

# cREATES AN EMPTY LIST
listofquads<-list()

# Function calculates percent cover for each functional group
for (i in 1:length(Masklayer2$ID%>%unique())) {
  
  # Subsets the quadrat based on a functional group
  listofquads[[i]]<-subset(Masklayer2,Masklayer2$ID == i)
  
  # Calculates the percentage of each functional group within each quadrat
  listoferrors<-lapply(1:length(classes), function(x){
    
    Fungroup_percent<-count(subset(listofquads[[i]],listofquads[[i]]$category == classes[x]))%>%dplyr::select(freq)/nrow(listofquads[[i]])*100
    
    names(Fungroup_percent)[1]<-classes[x]
    
    if (nrow(Fungroup_percent)==0){
      Fungroup_percent[1,1]<-"NA"
    }
    
    return(Fungroup_percent)
  })
  
  # Combines all the functional groups into a dataframe
  eachquad<-do.call("cbind",listoferrors)
  
  # Adds a column that represents quadrat number 
  eachquad$Quadrat<-paste0("Quad ",i)
  
  listofquads[[i]]<-eachquad
  
}

# Combines all the quadrat infor into one dataframe
Accuracy_AssesDF<-do.call("rbind",listofquads)%>%dplyr::select(Quadrat,everything())

# Writes out dataframe
write.csv(Accuracy_AssesDF,"Output/Accuracy_AssesDF.csv",row.names = F)



