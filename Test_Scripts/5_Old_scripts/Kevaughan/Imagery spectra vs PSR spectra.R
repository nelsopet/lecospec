####################Calculates the resampled bands for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in image of lichen pixels as dataframe 
Lichens_refl_1<-brick("Original_data/Test_imagery_HDW/White_lichen1")%>%rasterToPoints()%>%as.data.frame()
Lichens_refl_2<-brick("Original_data/Test_imagery_HDW/White_lichen2")%>%rasterToPoints()%>%as.data.frame()

##combines those pixels into one dataframe
Lichens_refl<-rbind(Lichens_refl_1,Lichens_refl_2)

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Test_Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##lets remove all those bads that had noise
Lichens_refl[275:328]<-NULL

##change colnames to correct band names
colnames(Lichens_refl)[-1:-2]<-HDW_ng_wv

##Add column category
Lichens_refl<-Lichens_refl%>%
  mutate(Category= "Light lichen_HDW")%>%dplyr::select(Category,everything(),-x,-y)

##Lets read in our spectral library
alaskaSpeclib_HDW_df<-read.csv("Test_Outputs/2_HDW_Imagery/1_Processing/alaskaSpecLib_HDW_df.csv",check.names = F)

##Lets get the reflectances of all those lichens that are either grew or white lichen
##First lets create objects 
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
  ,"pelsca")

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

alaskaSpeclib_light_lichen<-alaskaSpeclib_HDW_df%>%
  subset(PFT %in% Grey_Lichen | PFT %in% Yellow_Lichen)

alaskaSpeclib_light_lichen<-alaskaSpeclib_light_lichen%>%
  mutate(Category = "Light lichen_PSR")%>%dplyr::select(Category, everything(),-ScanID,-PFT,-PFT_2,-PFT_3,-Freq1,-Freq2,-area)
  
  
##wE NEED TO GATHER DATA 
alaskaSpeclib_light_lichen<-gather(alaskaSpeclib_light_lichen    ,Wavelength,Reflectance,-1)
Lichens_refl              <-gather(Lichens_refl                  ,Wavelength,Reflectance,-1)

alaskaSpeclib_light_lichen<-alaskaSpeclib_light_lichen%>%group_by(Category, Wavelength)%>%mutate(median_refl=median(Reflectance))
Lichens_refl              <-Lichens_refl              %>%group_by(Category, Wavelength)%>%mutate(median_refl=median(Reflectance))

#LICHENS<-rbind(alaskaSpeclib_light_lichen,Lichens_refl)%>%mutate(median_refl=median(Reflectance))  
#
#jpeg("Test_Outputs/Light lichen4.jpg", units="px", height = 1400, width=2400, res=350)
#LICHENS%>%
#  group_by(Category, Wavelength)%>%
#  ggplot(aes(Wavelength,median_refl))+geom_line(aes(color=Category))+
#  theme(panel.background = element_rect(fill = "white", colour = "grey50"),legend.key.size = unit(0.4, "cm"),legend.text = element_text(size=5))+
#  labs(title="Light Lichen")
#dev.off()  

jpeg("Test_Outputs/Light lichen2.jpg", units="px", height = 1400, width=2400, res=350)
plot(data=alaskaSpeclib_light_lichen,median_refl~Wavelength,
     main="PSR MEDIAN Refl(Light Lichens)")
dev.off()  

jpeg("Test_Outputs/Light lichen3.jpg", units="px", height = 1400, width=2400, res=350)
plot(data=Lichens_refl,median_refl~Wavelength,
     main="HDW MEDIAN Refl (Light Lichens)")
dev.off()  
  
plot(data=alaskaSpeclib_light_lichen,median_refl~Wavelength,
     main="PSR MEDIAN Refl(Light Lichens)")
par(new=TRUE)
plot(data=Lichens_refl,median_refl~Wavelength)
  