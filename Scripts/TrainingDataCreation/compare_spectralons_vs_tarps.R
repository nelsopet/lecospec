source("Functions/lecospectR.R")
spectralon<-read.csv("Data/Ground_Validation/WhiteRefs/Spectralon/radiance/Spectralon_all_sites.csv",header=TRUE)
    head(spectralon)
tarp_dir<-"Data/Ground_Validation/WhiteRefs/Tarp/radiance/"
single_pixel_tarp<-read.csv("Data/Ground_Validation/WhiteRefs/Tarp/SinglePixelWhiteRefs/Single_Tarp_pixel_white_refs.csv", header=TRUE)
single_pixel_tarp$Level<-"Single_Pixel_55pct"
single_pixel_tarp<-single_pixel_tarp[,-1]

sts<-single_pixel_tarp %>% filter(Level == "Single_Pixel_55pct") %>% filter(Site == "Chatanika")
base::plot(Radiance~Wavelength, data = sts)

windows()
ggplot(single_pixel_tarp, aes(Wavelength, Radiance, group = Site))+geom_line(aes(color=Site))
#Read in the band names
band_names <- read.csv("./assets/bands.csv")$x[1:326] %>% as.vector()

#List all tarp files
tarp_rad<-list.files(tarp_dir)[grepl("*.hdr",list.files(tarp_dir))]
#Make a list of just the file names to read
tarp_rad<-gsub(".hdr",".ENVI", tarp_rad)
#Make a list of sites the same length as the data
tarp_names<-c(rep("Bison",3), rep("Bonanza", 3), rep("Chatanika",3),rep("Eightmile",3), rep("Murphy",3),rep("TwelveMile",3))
#Make a list of reflectance levels the same length as the data
tarp_levels<-rep(c(c("55","11","32")),length(tarp_names))

#Read in each tarp, summarize it to the median by band and make tall like the spectralon data
tarp_all<-lapply(1:length(tarp_names), function(x) {
band_path<-terra::rast(paste(tarp_dir,tarp_rad[x], sep=""))
  #  head(as.data.frame(tst))
band_count<-names(band_path) %>% length()
tst<-band_path
df <- as.data.frame(tst) 
colnames(df)<-band_names
df<-df %>%
  pivot_longer(cols = `397.593`:`997.568`,  names_to  = "Wavelength", values_to = "Radiance") %>%
  group_by(Wavelength) %>%
  dplyr::summarise(Radiance = median(Radiance)) %>% 
   mutate(Wavelength = as.numeric(Wavelength)) %>%
  as.data.frame() 
df$Site<-tarp_names[x]
df$Level<-tarp_levels[x]
return(df)
})

tarp_all<-Reduce(rbind, tarp_all)
##Look at variability within the tarp values to make sure the right pixels were cropped


#Prepare data to merge
spectralon<-spectralon[,-1]
spectralon$Level<-"99"
str(spectralon)
str(single_pixel_tarp)
str(tarp_all)
all_white_refs<-rbind(spectralon,tarp_all, single_pixel_tarp)
all_white_refs<-as.data.frame(all_white_refs)

str(all_white_refs)

SpectralonCorrection<-all_white_refs %>% tidyr::spread(key = "Level", value = "Radiance") %>% #head
  mutate(diff = `Single_Pixel_55pct`/`99`)%>%# plot(diff)
  group_by(Wavelength) %>% summarize(diff_median = median(diff, na.rm=T))# %>% plot(diff)

write.csv(SpectralonCorrection,"Output/SpectralonCorrection.csv")
head(tst)
ggplot(SpectralonCorrection,aes(Wavelength,diff_median)) + geom_line()
dev.off()

windows()
jpeg("figures/all_white_refs_radiance.jpeg")
ggplot(all_white_refs,aes(Wavelength, Radiance, group = Site))+geom_point(aes(color=Level)) +
    facet_grid(vars(Site))
dev.off()
all_white_refs %>% dplyr::filter(Site == "Chatanika") %>% dplyr::filter(Level == 55) %>% head


