#Load packages
source("Functions/lecospectR.R")
#devtools::install_github("nelsopet/lecospectR.R")

Spectralon_df<-read.csv( "Data/Ground_Validation/WhiteRefs/Spectralon/radiance/Spectralon_all_sites.csv")  %>% dplyr::select(-X)
Spectralon_df$Level<-"99"

CorFact<-read.csv("Output/SpectralonCorrection.csv") %>% dplyr::select(-X)
head(CorFact)

Sites<-c("Bison","Chatanika","Eightmile")

single_pixel_tarp<-read.csv("Data/Ground_Validation/WhiteRefs/Tarp/SinglePixelWhiteRefs/Single_Tarp_pixel_white_refs.csv", header=TRUE)
single_pixel_tarp$Level<-"Single_Pixel_55pct"
single_pixel_tarp<-single_pixel_tarp[,-1]
unique(single_pixel_tarp$Site)
head(Spectralon_df)

#Select a single site for both tarp and spectralong
sts<-single_pixel_tarp %>% subset(Site %in% Sites) 
    #filter(Level == "Single_Pixel_55pct") %>% 
    #filter(Site == "Eightmile")

head(sts)
unique(sts$Site)
sts %>% group_by(Site, Level) %>% tally

spln<-Spectralon_df %>% subset(Site %in% Sites)
head(spln)
spln %>% group_by(Site, Level) %>% tally


sts_spln<-rbind(sts,spln)
str(sts_spln)
sts_spln %>% group_by(Site, Level) %>% tally

sts_spln_flat<-sts_spln %>% 
    group_by(Site, Wavelength) %>% 
    tidyr::spread(key=Level, value=Radiance) %>%
    mutate(CorFact = Single_Pixel_55pct/`99`)

sts_spln_flat_median<-sts_spln_flat %>% 
    group_by(Wavelength) %>% dplyr::summarise(Radiance = median(CorFact))


head(sts_spln_flat_median)
windows()
hist(sts_spln_flat$CorFact)
ggplot(sts_spln_flat,aes(`99`,Single_Pixel_55pct))+geom_line(aes(color=Site))

#Write correction factor to asssets
write.csv(sts_spln_flat_median, "assets/radiance_55pct_over_Spectralon.csv")


