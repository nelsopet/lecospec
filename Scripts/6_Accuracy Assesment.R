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

#> as(BisonQuads, "data.frame")
#CLASS_ID CLASS_NAME AREA
#0        1    Bison0m  612
#1        7   Bison70m  649
#2        8   Bison80m  661
#3        2   Bison10m  649
#4        9   Bison90m  648
#5        3   Bison20m  574
#6        6   Bison50m  673
#7        4   Bison30m  673
#8        5   Bison40m  674

BisonGulchQuads_FncGrp2_out<-raster("Output/Prediction/V2/FncGrp2/BisonGulchQuads.envi/BisonQuadOut.tif")
#Check to see why the tabular output of the extraction doesn't match the visual estimate of cover by class in the map
#Maybe the order of the extracion scrambles which quadrat is which
BisonExtract<-raster::extract(BisonGulchQuads_FncGrp2_out, BisonQuads)


hist(BisonExtract[[2]])

Bison_q8<-BisonExtract[[3]] %>% 
as.data.frame() %>% 
  dplyr::rename(PFT_num = ".") %>% 
  dplyr::mutate(Pix_cnt = n()) %>% 
  group_by(PFT_num) %>% 
  dplyr::mutate(PFT_pct_ML = 100*(n()/Pix_cnt)) %>% 
  unique() %>%
  left_join(fnc_grp2_colors, by = c("PFT_num" = "ColorNum")) %>%
  mutate(UID = "BisongulchQ8") %>%
  dplyr::rename(Plant_Functional_Type = FNC_grp1) %>% 
  ungroup() %>%
  #dplyr::select()
  left_join(AKValid2019_flat, by = c("UID"="UID","Plant_Functional_Type"="PFT"), keep=FALSE) %>%
  dplyr::rename(PFT_Pct_Human = TotCov) %>%
 dplyr::select(Plant_Functional_Type, Pix_cnt, PFT_pct_ML, PFT_Pct_Human)



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



