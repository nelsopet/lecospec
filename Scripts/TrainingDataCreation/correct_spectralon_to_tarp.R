#Load packages
source("Functions/lecospectR.R")
Spectralon_df<-read.csv( "Data/Ground_Validation/WhiteRefs/Spectralon/radiance/Spectralon_all_sites.csv")  %>% dplyr::select(-X)
CorFact<-read.csv("Output/SpectralonCorrection.csv") %>% dplyr::select(-X)
single_pixel_tarp<-read.csv("Data/Ground_Validation/WhiteRefs/Tarp/SinglePixelWhiteRefs/Single_Tarp_pixel_white_refs.csv", header=TRUE)
single_pixel_tarp$Level<-"Single_Pixel_55pct"
single_pixel_tarp<-single_pixel_tarp[,-1]


head(Spectralon_df)
Spectralon_df$Level<-"99"
boxplot(CorFact$diff_median)
sts<-single_pixel_tarp %>% filter(Level == "Single_Pixel_55pct") %>% filter(Site == "Chatanika")
head(sts)
tst1<-rbind(Spectralon_df, sts)
tst2<-inner_join(tst1, CorFact, by="Wavelength") %>% 
    dplyr::filter(Site=="Chatanika") %>%
    mutate(Rad_Cor_55pct = Radiance*diff_median) 

plot(tst2$Wavelength, tst2$Rad_Cor_55pct)

Chat_Tarp55pct<-tst2 %>% dplyr::filter(Level == "Single_Pixel_55pct")
Chat_RadCor55pct<-tst2 %>% dplyr::filter(Level == "99")

R2(Chat_RadCor55pct$Rad_Cor_55pct, Chat_Tarp55pct$Radiance)
