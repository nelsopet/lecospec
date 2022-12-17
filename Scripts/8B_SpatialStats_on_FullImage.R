require(landscapetools)
require(landscapemetrics)
require(sf)
source("Functions/lecospectR.R")

#List all images that are predictions

Output_files<-list.files("Output/") # %>% 
Output_PFT_names<-read.csv("assets/fg1RAT.csv") 
Output_file_names<-
str_match(Output_files, ".*fncgrp1_PREDICTIONS.tif") %>%
  as.data.frame() %>% dplyr::filter(is.na(V1)==FALSE)
  #dplyr::select(V2) #%>% 
  #as.data.frame()
str_match(Output_files, ".*fncgrp1_PREDICTIONS.tif") %>%
  as.data.frame() %>% dplyr::filter(is.na(V1)==FALSE) %>% unique()
 
#Read in images and project to NAD83 Alaska Albers so the units are meters
pft_rst<-terra::rast(paste("Output/",Output_file_names[1,], sep=""))
unique(values(pft_rst))
pft_rst_proj<-terra::project(pft_rst, "epsg:6393")
pft_rst_proj_int<- setValues(pft_rst_proj, as.integer(values(pft_rst_proj)))
terra::writeRaster(pft_rst_proj_int,paste("Output/Projected/",Output_file_names[1,], sep=""))

lapply(1:nrow(Output_file_names),function(x) {
pft_rst<-terra::rast(paste("Output/",Output_file_names[x,], sep=""))
pft_rst_proj<-terra::project(pft_rst, "epsg:6393")
pft_rst_proj_int<- setValues(pft_rst_proj, as.integer(values(pft_rst_proj)))
terra::writeRaster(pft_rst_proj_int,paste("Output/Projected/",Output_file_names[x,], sep=""), overwrite = T)
rm(pft_rst)
rm(pft_rst_proj)
rm(pft_rst_proj_int)
#return(pft_rst_proj_int)
})

pft_rst<-terra::rast(paste("Output/Projected/",Output_file_names[1,], sep=""))
img_rst_tst_area<-landscapemetrics::lsm_p_area(pft_rst)

rm(pft_rst)
  rm(img_rst_tst_area)

pft_area_all<-lapply(1:nrow(Output_file_names), function(x){
  pft_rst<-terra::rast(paste("Output/Projected/",Output_file_names[x,], sep=""))
  img_rst_tst_area<-landscapemetrics::lsm_p_area(pft_rst)
  img_rst_tst_area$image<-Output_file_names[x,]
  
  return(img_rst_tst_area)
  
  })

#names(pft_area_all)<-Output_file_names$V1

pft_area_all<-Reduce(rbind, pft_area_all)

pft_area_all_wNames<-pft_area_all %>% inner_join(Output_PFT_names, by=c("class"="ID"), keep=FALSE)

write.csv(pft_area_all, "Output/pft_area_all.csv")

#img_rst_tst_area<-landscapemetrics::lsm_p_area(bg_pft_rst_proj_int) #How are NAs handled?

img_rst_tst_area %>% group_by(class) %>% summarize(Min_Area = min(value)*10000,
                                                    Lowest_5pct_Area = quantile(value, probs = 0.05)*10000,
                                                   Median_Patch_Area = quantile(value, probs = 0.5)*10000, 
                                                    Upper_95pct_Area = quantile(value, probs = 0.95)*10000,
                                                    Max_Area = max(value)*10000)

class_dist<-lst_img_rst_tst %>% dplyr::filter(class==10) %>% dplyr::select(value)
class_area<-img_rst_tst_area %>% dplyr::filter(class==10) %>% dplyr::select(value)

hist(log10(class_val$value))
hist(log10(class_area$value))

values(bg_pft_rst)

range(img_rst_tst_area$value)
quantile(class_dist$value, probs = c(0.05, 0.5, 0.95))
quantile(class_area$value, probs = c(0.05, 0.5, 0.95))
range(class_area$value)
pft_area_all %>% filter(class == 2) %>% dplyr::filter(value>min_patch_size) %>% dim
dim(pft_area_all)


min_patch_size = min(log10(pft_area_all$value*100000)) 
median_patch_size = median(log10(pft_area_all$value*100000))

pfts<-unique(pft_area_all$class)
pft_area_all_wNames_filt<- pft_area_all_wNames %>% dplyr::filter(value>min_patch_size)
jpeg("figures/PatchSize_all.jpg", width = 10000, height = 10000)
ggplot(pft_area_all_wNames, aes(x = log10(value*100000))) + geom_density(aes(color=image)) + 
facet_wrap(vars(CAT), scales = "fixed", ncol = 3) + 
theme(panel.background = element_rect(fill = "black"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=50),
        strip.text = element_text(size = 50),
        axis.text = element_text(size = 50)
        #axis.text.x = element_text(angle = 90)) +
  ) + 
  xlab("Patch size log10 m2") +
  geom_vline(xintercept = log10(0.038^2), color="green") +
  geom_vline(xintercept = log10(1), color="yellow") +
  geom_vline(xintercept = log10(4^2), color="red") +
  geom_vline(xintercept = log10(10^2), color = "blue") +
  geom_vline(xintercept = log10(30^2), color= "grey") 

  dev.off()

lapply(1:length(pfts),
function(x){
jpeg(paste("figures/PatchSize_", pfts[x],".jpg", sep=""), width = 10000, height = 10000)
ggplot((pft_area_all %>% 
  dplyr::filter(class == pfts[x]) %>% 
  dplyr::filter(value>min_patch_size)), aes(x = log10(value*10000))) + 
  geom_histogram(alpha=0.5) + 
  geom_density(alpha=0.2)+
  facet_wrap(vars(image), scales = "fixed", ncol = 9) 
      dev.off() 
      })

