# ------------------------------------------- Accuracy assesment ---------------------------------------------------
library(tidyverse)
library(raster)
library(hsdar)
library(sf)
require(readxl)

AKValid2019<-read_xlsx(path = "Data/Ground_Validation/Quadrat_2019_AK.xlsx", sheet ="Raw")
AKValid2019_flat <- AKValid2019 %>%
  group_by(Plot, meters, UID, PFT) %>%
  summarise(TotCov = sum(cover)) %>%
  mutate(PFT = ifelse(PFT=="Litter", "Abiotic", ifelse(PFT=="Dwarf shrub", "Dwarf Shrub", PFT)))

unique(AKValid2019_flat$PFT) %>% as.data.frame() %>% rename(FNC_grp1 = ".") %>% anti_join(fnc_grp2_colors, by = "FNC_grp1")
fnc_grp2_colors$FNC_grp1

Bison_seg_path = "Data/Ground_Validation/Bison_Quadrats/Bison_Quadrats.shp"
BisonQuads<-readOGR(dsn = Bison_seg_path)
BisonGulchQuads_FncGrp2_out<-raster("Output/Prediction/V2/FncGrp2/BisonGulchQuads.envi/BisonQuadOut.tif")
BisonExtract<-raster::extract(BisonGulchQuads_FncGrp2_out, BisonQuads)
BisonQuadNames<-as(BisonQuads, "data.frame")

BisonAccuracy<-
lapply(1:length(BisonExtract), function(x)
  BisonExtract[[x]] %>% 
as.data.frame() %>% 
  dplyr::rename(PFT_num = ".") %>% 
  dplyr::mutate(Pix_cnt = n()) %>% 
  group_by(PFT_num) %>% 
  dplyr::mutate(PFT_pct_ML = 100*(n()/Pix_cnt), Plot = "Bisongulch") %>% 
  unique() %>%
  left_join(fnc_grp2_colors, by = c("PFT_num" = "ColorNum")) %>%
  mutate(UID = paste("Q",(as.numeric(BisonQuadNames$CLASS_ID[x])-1), sep="")) %>%
  dplyr::rename(PFT = FNC_grp1) %>% 
  ungroup() %>%
  #dplyr::select()
  left_join(AKValid2019_flat, by = c("UID"="meters","PFT"="PFT", "Plot"="Plot"), keep=FALSE) %>%
  dplyr::rename(PFT_Pct_Human = TotCov) %>%
 dplyr::select(PFT, Pix_cnt, PFT_pct_ML, PFT_Pct_Human))

TwelveMile1_seg_path = "Data/Vectors/TwelveMileQ0_10_20_30_40m.shp"
TwelveMile1Quads<-readOGR(dsn = TwelveMile1_seg_path)
#plot(TwelveMile1Quads)
TwelveMile1Quads_FncGrp2_out<-raster("Output/Prediction/V2/FncGrp2/TwelveMileGulchQuads1.envi/TwelveMile1TestOut.tif")
#plot(TwelveMile1Quads_FncGrp2_out)
TwelveMile1Extract<-raster::extract(TwelveMile1Quads_FncGrp2_out, TwelveMile1Quads)
TwelveMile1Names<-as(TwelveMile1Quads, "data.frame")


TwelveMile1Accuracy<-
  lapply(1:length(TwelveMile1Extract), function(x)
    TwelveMile1Extract[[x]] %>% 
      as.data.frame() %>% 
      dplyr::rename(PFT_num = ".") %>% 
      dplyr::mutate(Pix_cnt = n()) %>% 
      group_by(PFT_num) %>% 
      dplyr::mutate(PFT_pct_ML = 100*(n()/Pix_cnt), Plot = "Twelvemile") %>% 
      unique() %>%
      left_join(fnc_grp2_colors, by = c("PFT_num" = "ColorNum")) %>%
      mutate(UID = paste("Q",(as.numeric(TwelveMile1Names$CLASS_ID[x])-1), sep="")) %>%
      dplyr::rename(PFT = FNC_grp1) %>% 
      ungroup() %>%
      #dplyr::select()
      left_join(AKValid2019_flat, by = c("UID"="meters","PFT"="PFT", "Plot"="Plot"), keep=FALSE) %>%
      dplyr::rename(PFT_Pct_Human = TotCov) %>%
      dplyr::select(PFT, Pix_cnt, PFT_pct_ML, PFT_Pct_Human))


###OLD
# Reads in raster
Predlayer<-raster("Output/Prediction/V2/FncGrp2/BisonGulchQuads.envi/BisonQuadOut.tif")
projection(Predlayer)
# Plots Predicted layer
# plot(Predlayer)

# Grabs attribute table from image
#AttributeTab<-Predlayer@data@values @data@attributes%>%as.data.frame()

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



