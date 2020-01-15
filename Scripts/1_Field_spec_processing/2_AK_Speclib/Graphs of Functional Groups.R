#################################Creates Plots for functional groups###################################
###Extracts specrtral data from field scans###
library(tidyverse)

alaskaSpecLib<-read.csv("Test_Outputs/1_Field_spec/1_Processing/alaskaSpecLib_df.csv",check.names = F)

#We want to create dataframes that have all the scans of each functional groups
##This can be used to make graphs of all the species within each functional group
alaskaSpecLib_Lichen    <-subset(alaskaSpecLib,PFT_3=="Lichen")
alaskaSpecLib_Tree      <-subset(alaskaSpecLib,PFT_3=="Tree")
alaskaSpecLib_Dwarfshrub<-subset(alaskaSpecLib,PFT_3=="Dwarf Shrub")
alaskaSpecLib_shrub     <-subset(alaskaSpecLib,PFT_3=="Shrub")
alaskaSpecLib_Moss      <-subset(alaskaSpecLib,PFT_3=="Moss")
alaskaSpecLib_Graminoid <-subset(alaskaSpecLib,PFT_3=="Graminoid")
alaskaSpecLib_Forb      <-subset(alaskaSpecLib,PFT_3=="Forb")

##You'll need to use the gather function to prepare the data set to be used in ggplot
alaskaSpecLib_Lichen    <-gather(alaskaSpecLib_Lichen    ,Wavelength,Reflectance,-1:-7)
alaskaSpecLib_Tree      <-gather(alaskaSpecLib_Tree      ,Wavelength,Reflectance,-1:-7)
alaskaSpecLib_Dwarfshrub<-gather(alaskaSpecLib_Dwarfshrub,Wavelength,Reflectance,-1:-7)
alaskaSpecLib_shrub     <-gather(alaskaSpecLib_shrub     ,Wavelength,Reflectance,-1:-7)
alaskaSpecLib_Moss      <-gather(alaskaSpecLib_Moss      ,Wavelength,Reflectance,-1:-7)
alaskaSpecLib_Graminoid <-gather(alaskaSpecLib_Graminoid ,Wavelength,Reflectance,-1:-7)
alaskaSpecLib_Forb      <-gather(alaskaSpecLib_Forb      ,Wavelength,Reflectance,-1:-7)

##Find the median of all the species within each functional group
names(alaskaSpecLib_Lichen    )[3]<-"Species" 
names(alaskaSpecLib_Tree      )[3]<-"Species" 
names(alaskaSpecLib_Dwarfshrub)[3]<-"Species"
names(alaskaSpecLib_shrub     )[3]<-"Species"
names(alaskaSpecLib_Moss      )[3]<-"Species"
names(alaskaSpecLib_Graminoid )[3]<-"Species"
names(alaskaSpecLib_Forb      )[3]<-"Species"

##Make column name Wavelength numeric
alaskaSpecLib_Lichen$Wavelength    <-as.numeric(alaskaSpecLib_Lichen$Wavelength)    
alaskaSpecLib_Tree$Wavelength      <-as.numeric(alaskaSpecLib_Tree$Wavelength      )    
alaskaSpecLib_Dwarfshrub$Wavelength<-as.numeric(alaskaSpecLib_Dwarfshrub$Wavelength)
alaskaSpecLib_shrub$Wavelength     <-as.numeric(alaskaSpecLib_shrub$Wavelength     )  
alaskaSpecLib_Moss$Wavelength      <-as.numeric(alaskaSpecLib_Moss$Wavelength      )  
alaskaSpecLib_Graminoid$Wavelength <-as.numeric(alaskaSpecLib_Graminoid$Wavelength )  
alaskaSpecLib_Forb$Wavelength      <-as.numeric(alaskaSpecLib_Forb$Wavelength      )  

###Create Group of lichens
#Cladonia<-c(
#  "claama"
#  ,"clacor"
#  ,"clacuc"
#  ,"clagra"
#  ,"clamit"
#  ,"claran"
#  ,"claste"
#  ,"clasty"
#  ,"clasul"
#  ,"claunc"
#)
#
#Dark_lichen<-c(
#  "bryoria"
#  ,"cetisl"
#  ,"cetlae"
#  ,"masric"
#  ,"melanelia"
#  ,"melhep"
#  ,"tragra"
#  ,"umbarc"
#  ,"umbhyp"
#  ,"grey_rhizocarpon"
#  ,"orange_Porpidia"
#)
#
#Grey_Lichen<-c(
#  "hypaus"
#  ,"icmeri"
#  ,"paromp"
#  ,"parsul"
#  ,"stepas"
#  ,"stetas"
#  ,"pelapt"
#  ,"pelleu"
#  ,"pelmal"
#  ,"pelsca"
#)
#
#Yellow_Lichen<-c(
#  "paramb"
#  ,"usnlap"
#  ,"usnsca"
#  ,"neparc"
#  ,"evemes"
#  ,"flacuc"
#  ,"flaniv"
#  ,"vulpin"
#  ,"aleoch"
#  ,"arccen"
#  ,"asachr")


##Make plots for each Functional group
##Lichen 
##Cladonia
#jpeg("Cladonia.jpg", units="px", height = 1400, width=2400, res=350)
#alaskaSpecLib_Lichen%>%
#  subset(PFT %in% Cladonia)%>%
#  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
#  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
#  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Cladonia Spectral signatures")
#dev.off()
#
###Dark Lichen
#jpeg("Dark Lichen.jpg", units="px", height = 1400, width=2400, res=350)
#alaskaSpecLib_Lichen%>%
#  subset(PFT %in% Dark_lichen)%>%
#  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
#  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
#  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Dark Lichen Spectral signatures")
#dev.off()
#
###Grew Lichen
#jpeg("Grey Licnen.jpg", units="px", height = 1400, width=2400, res=350)
#alaskaSpecLib_Lichen%>%
#  subset(PFT %in% Grey_Lichen)%>%
#  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
#  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
#  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Grey Licnen signatures")
#dev.off()
#
###Yellow Lichen
#jpeg("Yellow Lichen.jpg", units="px", height = 1400, width=2400, res=350)
#alaskaSpecLib_Lichen%>%
#  subset(PFT %in% Yellow_Lichen)%>%
#  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
#  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
#  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Yellow Lichen signatures")
#dev.off()

##Lichen
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Lichen.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Lichen%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+ 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.key.size = unit(0.3, "cm"),legend.text = element_text(size=3))+
  labs(title="Lichen signatures")
dev.off()

##Tree
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Tree.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Tree%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Tree signatures")
dev.off()

##Dwarf Shrub
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Dwarf Shrub.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Dwarfshrub%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Dwarf Shrub Spectral signatures")
dev.off()

##Shrub
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Shrub.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_shrub%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Shrub Spectral signatures")
dev.off()


##Moss
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Moss.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Moss%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Moss Spectral signatures")
dev.off()

##Graminoid
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Graminoid.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Graminoid%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Gramonoid Spectral signatures")
dev.off()

##Forb
jpeg("Test_Outputs/1_Field_spec/1_Processing/Graphs/Forb.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Forb%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Forb Spectral signatures")
dev.off()




