#################################Creates Plots for functional groups###################################
###Extracts specrtral data from field scans###
library(tidyverse)

alaskaSpecLib_Lichen    <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_Lichen.csv"    ,check.names = F)
alaskaSpecLib_Tree      <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_Tree.csv"      ,check.names = F)
alaskaSpecLib_Dwarfshrub<-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_Dwarfshrub.csv",check.names = F)
alaskaSpecLib_shrub     <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_shrub.csv"     ,check.names = F)
alaskaSpecLib_Moss      <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_Moss.csv"      ,check.names = F)
alaskaSpecLib_Graminoid <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_Graminoid.csv" ,check.names = F)
alaskaSpecLib_Forb      <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_Forb.csv"      ,check.names = F)

##You'll need to use the gather function to prepare the data set to be used in ggplot
alaskaSpecLib_Lichen    <-gather(alaskaSpecLib_Lichen    ,Wavelength,Reflectance,-1:-6)
alaskaSpecLib_Tree      <-gather(alaskaSpecLib_Tree      ,Wavelength,Reflectance,-1:-6)
alaskaSpecLib_Dwarfshrub<-gather(alaskaSpecLib_Dwarfshrub,Wavelength,Reflectance,-1:-6)
alaskaSpecLib_shrub     <-gather(alaskaSpecLib_shrub     ,Wavelength,Reflectance,-1:-6)
alaskaSpecLib_Moss      <-gather(alaskaSpecLib_Moss      ,Wavelength,Reflectance,-1:-6)
alaskaSpecLib_Graminoid <-gather(alaskaSpecLib_Graminoid ,Wavelength,Reflectance,-1:-6)
alaskaSpecLib_Forb      <-gather(alaskaSpecLib_Forb      ,Wavelength,Reflectance,-1:-6)

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

##Create Group of lichens
Cladonia<-c(
  "claama"
  ,"clacor"
  ,"clacuc"
  ,"clagra"
  ,"clamit"
  ,"claran"
  ,"claste"
  ,"clasty"
  ,"clasul"
  ,"claunc"
)

Dark_lichen<-c(
  "bryoria"
  ,"cetisl"
  ,"cetlae"
  ,"masric"
  ,"melanelia"
  ,"melhep"
  ,"tragra"
  ,"umbarc"
  ,"umbhyp"
  ,"grey_rhizocarpon"
  ,"orange_Porpidia"
)

Grey_Lichen<-c(
  "hypaus"
  ,"icmeri"
  ,"paromp"
  ,"parsul"
  ,"stepas"
  ,"stetas"
  ,"pelapt"
  ,"pelleu"
  ,"pelmal"
  ,"pelsca"
)

Yellow_Lichen<-c(
  "paramb"
  ,"usnlap"
  ,"usnsca"
  ,"neparc"
  ,"evemes"
  ,"flacuc"
  ,"flaniv"
  ,"vulpin"
  ,"aleoch"
  ,"arccen"
  ,"asachr")


##Make plots for each Functional group
##Lichen 
##Cladonia
jpeg("Cladonia.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Lichen%>%
  subset(PFT %in% Cladonia)%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Cladonia Spectral signatures")
dev.off()

##Dark Lichen
jpeg("Dark Lichen.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Lichen%>%
  subset(PFT %in% Dark_lichen)%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Dark Lichen Spectral signatures")
dev.off()

##Grew Lichen
jpeg("Grey Licnen.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Lichen%>%
  subset(PFT %in% Grey_Lichen)%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Grey Licnen signatures")
dev.off()

##Yellow Lichen
jpeg("Yellow Lichen.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Lichen%>%
  subset(PFT %in% Yellow_Lichen)%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Yellow Lichen signatures")
dev.off()

##Tree
jpeg("Tree.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Tree%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Tree signatures")
dev.off()

##Dwarf Shrub
jpeg("Dwarf Shrub.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Dwarfshrub%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Dwarf Shrub Spectral signatures")
dev.off()

##Shrub
jpeg("Shrub.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_shrub%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Shrub Spectral signatures")
dev.off()


##Moss
jpeg("Moss.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Moss%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Moss Spectral signatures")
dev.off()

##Graminoid
jpeg("Graminoid.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Graminoid%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Gramonoid Spectral signatures")
dev.off()

##Forb
jpeg("Forb.jpg", units="px", height = 1400, width=2400, res=350)
alaskaSpecLib_Forb%>%
  group_by(Species, Wavelength) %>% mutate(Median_Reflectance = median(Reflectance)) %>% 
  ggplot(aes(Wavelength,Median_Reflectance))+geom_line(aes(color=Species))+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Forb Spectral signatures")
dev.off()




