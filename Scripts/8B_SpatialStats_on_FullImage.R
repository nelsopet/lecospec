require(landscapetools)
require(landscapemetrics)
require(sf)
source("Functions/lecospectR.R")

#List all images that are predictions

Output_files<-list.files("Output/") # %>% 
Output_PFT_names<-read.csv("assets/fg1RAT.csv") 
Output_file_names<-str_match(Output_files, ".*fncgrp1_PREDICTIONS.tif") %>%
  as.data.frame() %>% 
  dplyr::filter(is.na(V1)==FALSE) %>% 
  unique()
Output_file_names_valid_cubes<-c(
bg_01_07_3511_fncgrp1_PREDICTIONS.tif

) 
#Read in images and project to NAD83 Alaska Albers so the units are meters

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


#Test landscape metrics out on one site
rst<-terra::rast("M:/Alaska_Datacubes/Raw_files/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or")
dim(rst)
terra::plot(rst)
#rst <-terra::rast(system.file("M:/Alaska_Datacubes/Raw_files/BisonGulch_2019_08_12_01_07_28_1511_rd_rf_or", package="terra"))   

pft_rst_tst<-terra::rast(paste("Output/Projected/",Output_file_names[4,], sep=""))
img_rst_tst_area_tst<-landscapemetrics::lsm_p_area(pft_rst_tst)
img_rst_tst_patch_centers_tst<-landscapemetrics::get_centroids(pft_rst_tst)
head(img_rst_tst_patch_centers_tst)

#Summarize patch metrics by PFT
min_patch_size = min(img_rst_tst_area_tst$value*100000) 

img_rst_tst_area_tst %>% 
group_by(class) %>%
#dplyr::filter(value*100000>min_patch_size) %>% 
dplyr::filter(value*100000>0.05) %>% 

#summarize(total_area = sum(value*10000))) 
summarize(
       Min = min(value*100000),
       Lower5 = quantile(value*100000, probs = 0.05),
       Median = quantile(value*100000, probs = 0.5),
       Upper95= quantile(value*100000, probs = 0.95),
       Max = max(value*100000))


jpeg("Output/test_patch_boxplot.jpg")
#boxplot(log(value*10000, base = 10) ~ class, data = img_rst_tst_area_tst)
boxplot(log(value*10000, base = 10) ~ class, data = img_rst_tst_area_tst %>% 
group_by(class) %>%
#dplyr::filter(value*100000>min_patch_size) %>% 
dplyr::filter(value*100000<16))

dev.off() 

#Plot RGB of one site
jpeg("Output/test_RGB.jpg", width=5000, height=4000)
terra::plotRGB(rst, r="640.175 nm", g="569.808 nm", b="469.812 nm", stretch = "linear")
#terra::plot(rst[c(132,83,40)])
dev.off()

#Plot the PFT predictions from lecospec
jpeg("Output/test_PFT_output.jpg", width=5000, height=4000)
plot(pft_rst_tst)
dev.off()

#Plot patch centers made by landscapemetrics::lsm_p_area
jpeg("Output/test_patch_centers.jpg", width=5000, height=4000)
plot(img_rst_tst_patch_centers_tst$x,img_rst_tst_patch_centers_tst$y)
dev.off()

#Show patches using landscapemetric
jpeg("Output/test_patches.jpg", width=5000, height=4000)
landscapemetrics::show_patches(pft_rst_tst) 
dev.off()

img_rst_tst_area_tst$log_value<-log10(img_rst_tst_area_tst$value*10000)

#Histogram or violin plot of frequency of patch sizes by PFT
jpeg("figures/PatchFrac_single_site_test.jpg", width=5000, height=4000)
ggplot(img_rst_tst_area_tst, aes(group=class))+ 
geom_violin(aes(y= log_value, x=class), adjust = 1.25)
#geom_histogram(aes(x=log10(value*100000)))+
#geom_histogram(aes(x=log_value))+

#facet_grid(vars(class))

dev.off()


#Calculate a bunch of landscapemetrics, which ends up being way too big .. .several Gb
pft_area_frac_all<-lapply(1:nrow(Output_file_names), function(x){
  pft_rst<-terra::rast(paste("Output/Projected/",Output_file_names[x,], sep=""))
  img_rst_tst_area<-landscapemetrics::lsm_p_area(pft_rst)
  img_rst_tst_area$image<-Output_file_names[x,]
  #img_rst_tst_area$PFT<-Output_PFT_names$CAT[x]
  img_rst_tst_frac<-landscapemetrics::lsm_p_frac(pft_rst)
  img_rst_tst_frac$image<-Output_file_names[x,]
  #img_rst_tst_frac$PFT<-Output_PFT_names$CAT[x]
  img_rst_tst_area_frac<-rbind(img_rst_tst_area,img_rst_tst_frac)
  
  return(img_rst_tst_area_frac)
  
  })


pft_area_frac_all<-Reduce(rbind, pft_area_frac_all)

head(pft_area_frac_all)
dim(pft_area_frac_all)
pft_area_frac_all_wNames<-pft_area_frac_all %>% inner_join(Output_PFT_names, by=c("class"="ID"), keep=FALSE)

write.csv(pft_area_all, "Output/pft_area_all.csv")

unique(pft_area_frac_all_wNames$metric)
pft_area_frac_all_wNames %>% group_by(CAT) %>% filter(metric == "frac" ) %>% group_by(CAT) %>% dplyr::filter(value==1) %>% dim
pft_area_frac_all_wNames %>%  filter(metric == "area") %>% dplyr::filter(value==1) %>% group_by(PFT) #dplyr::filter(value<2) %>% dim

minFrac<-pft_area_frac_all_wNames %>% filter(metric == "frac") %>% summarize(min(value))
pft_area_frac_all_wNames$value_pos<-pft_area_frac_all_wNames$value+48208
jpeg("figures/PatchFrac_all.jpg", width = 1000, height = 700)
ggplot(pft_area_frac_all_wNames %>% group_by(CAT) %>% filter(metric == "frac"), aes(x=CAT, y=sqrt(value_pos)))+ 
geom_violin(aes(fill=CAT))
dev.off()

pfts<-unique(pft_area_all$class)
#pft_area_frac_all_wNames_filt<- pft_area_frac_all_wNames %>% dplyr::filter(value>min_patch_size)  %>% dim

jpeg("figures/PatchSize_all.jpg", width = 4000, height = 3000)
ggplot(pft_area_frac_all_wNames %>% dplyr::filter(metric == "area"), aes(y = log10(value*100000), x=CAT)) + geom_violin(aes(fill=CAT)) +#
#geom_density(aes(color=image)) + 
#facet_wrap(vars(CAT), scales = "free", ncol = 3) + 
theme(panel.background = element_rect(fill = "white"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=50),
        strip.text = element_text(size = 50),
        axis.text = element_text(size = 50)
        #axis.text.x = element_text(angle = 90)) +
  ) + 
  ylab("Patch size log10 m2") +
  geom_hline(yintercept = log10(0.038^2), color="green") +
  geom_hline(yintercept = log10(0.1^2), color="purple") +
  geom_hline(yintercept = log10(1), color="yellow") +
  geom_hline(yintercept = log10(4^2), color="red") +
  geom_hline(yintercept = log10(10^2), color = "blue") +
  geom_hline(yintercept = log10(30^2), color= "grey") 

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

