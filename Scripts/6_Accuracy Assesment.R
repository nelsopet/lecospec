# ------------------------------------------- Accuracy assesment ---------------------------------------------------
library(tidyverse)
library(raster)
library(hsdar)
library(sf)
require(readxl)

#test change

AKValid2019<-read.csv("./Data/Ground_Validation/QuadratEstimates/Lab_quadrat_cover_2019_Raw.csv", header=TRUE)

AKValid2019<-
AKValid2019 %>%
mutate(Plant=ifelse(Plant=="Vaccinium vitis-idaea","Vaccinium vitis-idea",Plant),
       Plant=ifelse(Plant=="Bare soil","Bare Soil",Plant),
       Plant=ifelse(Plant==            "Hedysarum","ForbFlower",Plant),
       Plant=ifelse(Plant==            "Thamnolia","LightTerrestrialMacrolichen",Plant),
       Plant=ifelse(Plant==           "Astragulus","ForbFlower",Plant),
       Plant=ifelse(Plant==                "Phlox","ForbFlower",Plant),
       Plant=ifelse(Plant==            "Oxytropis","ForbFlower",Plant),
       Plant=ifelse(Plant==              "Anenome","ForbFlower",Plant),
       Plant=ifelse(Plant== "Potentilla fruticosa","ShrubDecidOther",Plant),
       Plant=ifelse(Plant==            "Polygonum","ForbFlower",Plant))
  

Species_groups<-read.csv("./Data/SpeciesTable_20220125.csv", encoding = 'UTF-8')

core_names<-colnames(AKValid2019)


#Try to make Validation taller for a single join between Plant and SpeciesName
Species_groups_tall<-Species_groups %>% 
  pivot_longer(cols=Species_name:Functional_group1) %>%
  rename(PFT=value) %>%
  mutate(PFT = str_remove(PFT,"sp. "))

AKValid2019 %>%
  dplyr::filter(cover_prn>0)%>%
  anti_join(Species_groups_tall, by=c("Plant"="PFT")) %>% 
  dplyr::select(Plant) %>% 
  unique()

#Find PFTs found in Validation but not SpeciesTable

AKValid2019 %>% #dim #[1] 400   7
  dplyr::filter(is.na(cover_prn)==FALSE) %>% 
  anti_join(Species_groups_tall, by=c("Plant"="PFT")) %>%
  dplyr::select(Plant, PFT) %>%
  unique()
  

AKValidSpecies_missing<-AKValid2019 %>% #dim #[1] 400   7
  dplyr::filter(is.na(cover_prn)==FALSE & PFT == "Species") %>% 
  anti_join(Species_groups, by=c("Plant"="Species_name")) #%>%  

AKValidGenus_missing<-AKValid2019 %>%
  dplyr::filter(is.na(cover_prn)==FALSE & PFT == "Genus") %>% 
  anti_join(Species_groups, by=c("Plant"="Genus")) 

AKValidFncGrp2_missing<-AKValid2019 %>%
  dplyr::filter(is.na(cover_prn)==FALSE & PFT == "FncGrp2") %>% 
  anti_join(Species_groups, by=c("Plant"="Functionalgroup2")) #%>% dim

AKValidAbiotic_missing<-AKValid2019 %>%
  dplyr::filter(is.na(cover_prn)==FALSE & PFT == "Abiotic") %>% 
  anti_join(Species_groups, by=c("Plant"="Functional_group1")) #%>% dim


AKValid2019_flat_missing<-bind_rows(AKValidSpecies_missing,AKValidGenus_missing,AKValidFncGrp2_missing,AKValidAbiotic_missing)

AKValid2019_flat_missing %>% dplyr::select(Plant) %>% unique()

#Join each column one PFT level at a time. Would be more efficient to make the AKValid2019 tall
  AKValidSpecies<-AKValid2019 %>% #dim #[1] 400   7
  dplyr::filter(is.na(cover_prn)==FALSE & PFT == "Species") %>% 
  left_join(Species_groups, by=c("Plant"="Species_name")) #%>%  
  
  AKValidGenus<-AKValid2019 %>%
    dplyr::filter(is.na(cover_prn)==FALSE & PFT == "Genus") %>% 
    left_join(Species_groups, by=c("Plant"="Genus")) 
  
  AKValidFncGrp2<-AKValid2019 %>%
    dplyr::filter(is.na(cover_prn)==FALSE & PFT == "FncGrp2") %>% 
    dplyr::select(core_names) %>% #dim
    left_join(Species_groups, by=c("Plant"="Functionalgroup2")) #%>% dim
  
  AKValidAbiotic<-AKValid2019$Plant %>%
    dplyr::filter(is.na(cover_prn)==FALSE & PFT == "Abiotic") %>% 
    dplyr::select(core_names) %>% #dim
    left_join(Species_groups, by=c("Plant"="Functional_group1")) #%>% dim

  
  AKValid2019_flat<-bind_rows(AKValidSpecies,AKValidGenus,AKValidFncGrp2,AKValidAbiotic)
  

  
####LEFT OFF HERE 2022015 PRN  
unique(AKValid2019_flat$PFT) %>% as.data.frame() %>% rename(FNC_grp1 = ".") %>% anti_join(fnc_grp2_colors, by = "FNC_grp1")
fnc_grp2_colors$FNC_grp1

Bison_seg_path = "Data/Ground_Validation/Bison_Quadrats/Bison_Quadrats.shp"
BisonQuads<-readOGR(dsn = Bison_seg_path)
BisonGulchQuads_FncGrp2_out<-raster("Output/bison_gulch_outputs_par.grd")
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


#TwelveMile1Accuracy<-
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
      left_join(AKValid2019_flat, by = c("meters"="UID","PFT"="PFT", "Plot"="Plot"), keep=TRUE) %>%
      dplyr::rename(PFT_Pct_Human = TotCov) %>%
      dplyr::select(PFT.x, Pix_cnt, PFT_pct_ML, PFT_Pct_Human, meters, Plot.x))




