#################################Creates Plots for functional groups###################################
###Extracts specrtral data from field scans###
library(tidyverse)

alaskaSpecLib<-read.csv("Outputs/1_Field_spec/1_Processing/PSR_data/PSR_ProcessedSpec/alaskaSpecLib_df.csv",check.names = F)

#We want to create dataframes that have all the scans of each functional group
##This can be used to make Graphs_SpecProfiles of all the species within each functional group
##I could create a function to do this
alaskaSpecLib_Lichen    <-subset(alaskaSpecLib,PFT_4=="Lichen")
alaskaSpecLib_Tree      <-subset(alaskaSpecLib,PFT_4=="Tree")
alaskaSpecLib_Dwarfshrub<-subset(alaskaSpecLib,PFT_4=="Dwarf Shrub")
alaskaSpecLib_shrub     <-subset(alaskaSpecLib,PFT_4=="Shrub")
alaskaSpecLib_Moss      <-subset(alaskaSpecLib,PFT_4=="Moss")
alaskaSpecLib_Graminoid <-subset(alaskaSpecLib,PFT_4=="Graminoid")
alaskaSpecLib_Forb      <-subset(alaskaSpecLib,PFT_4=="Forb")

##You'll need to use the gather function to prepare the data set to be used in ggplot
##Use functional coding here
alaskaSpecLib_Lichen    <-gather(alaskaSpecLib_Lichen    ,Wavelength,Reflectance,-1:-9)
alaskaSpecLib_Tree      <-gather(alaskaSpecLib_Tree      ,Wavelength,Reflectance,-1:-9)
alaskaSpecLib_Dwarfshrub<-gather(alaskaSpecLib_Dwarfshrub,Wavelength,Reflectance,-1:-9)
alaskaSpecLib_shrub     <-gather(alaskaSpecLib_shrub     ,Wavelength,Reflectance,-1:-9)
alaskaSpecLib_Moss      <-gather(alaskaSpecLib_Moss      ,Wavelength,Reflectance,-1:-9)
alaskaSpecLib_Graminoid <-gather(alaskaSpecLib_Graminoid ,Wavelength,Reflectance,-1:-9)
alaskaSpecLib_Forb      <-gather(alaskaSpecLib_Forb      ,Wavelength,Reflectance,-1:-9)

##Find the median of all the species within each functional group
##Use fucntional coding here
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

##lets figure out a way to make plots for each functional groups using loops
##We could do this for both PFT_3 AND PFT_4
##Lichen
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Lichen.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Lichen%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+ 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), legend.key.size = unit(0.3, "cm"),legend.text = element_text(size=3))+
  labs(title="Lichen signatures")
dev.off()

##Tree
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Tree_MSGC.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Tree%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Spectral Signatures of Trees Collected")
dev.off()

##Dwarf Shrub
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Dwarf Shrub.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Dwarfshrub%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Dwarf Shrub Spectral signatures")
dev.off()

##Shrub
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Shrub.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_shrub%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Shrub Spectral signatures")
dev.off()


##Moss
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Moss.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Moss%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
  labs(title="Moss Spectral signatures")
dev.off()

##Graminoid
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Graminoid.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Graminoid%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Gramonoid Spectral signatures")
dev.off()

##Forb
jpeg("Outputs/1_Field_spec/1_Processing/Graphs_SpecProfiles/Forb.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Forb%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Forb Spectral signatures")
dev.off()




