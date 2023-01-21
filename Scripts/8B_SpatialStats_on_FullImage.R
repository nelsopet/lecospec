require(landscapetools)
require(landscapemetrics)
require(sf)
source("Functions/lecospectR.R")

#List all images that are predictions

Output_files<-list.files("Output/") # %>% 
Output_PFT_names<-read.csv("assets/fg1RAT.csv") 
Output_file_names<-
str_match(Output_files, ".*fncgrp1_PREDICTIONS.tif") %>%
  as.data.frame() %>% dplyr::filter(is.na(V1)==FALSE) %>% unique()
  #dplyr::select(V2) #%>% 
  #as.data.frame()
str_match(Output_files, ".*fncgrp1_PREDICTIONS.tif") %>%
  as.data.frame() %>% dplyr::filter(is.na(V1)==FALSE) %>% unique()

#UNIT TEST: PASS 
#Read in images and project to NAD83 Alaska Albers so the units are meters
#pft_rst<-terra::rast(paste("Output/",Output_file_names[1,], sep=""))

#unique(values(pft_rst))

#UNIT TEST: PASS
#pft_rst_proj<-terra::project(pft_rst, "epsg:6393")
#pft_rst_proj_int<- setValues(pft_rst_proj, as.integer(values(pft_rst_proj)))
#terra::writeRaster(pft_rst_proj_int,paste("Output/Projected/",Output_file_names[1,], sep=""))

#
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

#UNIT TEST: PASS
#pft_rst<-terra::rast(paste("Output/Projected/",Output_file_names[1,], sep=""))
#img_rst_tst_area<-landscapemetrics::lsm_p_area(pft_rst)
#img_tst_lsm<-calculate_lsm(pft_rst)
#rm(pft_rst)
#rm(img_rst_tst_area)

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

write.csv(pft_area_frac_all, "Output/pft_area_frac_all.csv")

#Use when you don't want to rerun the code above which takes awhile
pft_area_frac_all<-read.csv( "Output/pft_area_frac_all.csv")


#This would take a very long time to run. One image takes >30 min and there are 77 images ... a few days worth of CPU time
#pft_lsm_all<-lapply(1:nrow(Output_file_names), function(x){
#  pft_rst<-terra::rast(paste("Output/Projected/",Output_file_names[x,], sep=""))
#  img_lsm<-calculat_lsm(pft_rst)
#  rm(pft_rst)  
#  return(img_lsm)  
#  })

head(pft_area_frac_all)
dim(pft_area_frac_all)

pft_area_frac_all_wNames<-pft_area_frac_all %>% 
  inner_join(Output_PFT_names, by=c("class"="ID"), keep=FALSE)

min_patch_size = min(pft_area_frac_all_wNames %>% 
  dplyr::filter(metric == "area") %>% 
  dplyr::select(value))

unique(pft_area_frac_all_wNames$metric)

minFrac<-pft_area_frac_all_wNames %>% filter(metric == "frac") %>% summarize(min(value))

#pft_area_frac_all_wNames$value_pos<-pft_area_frac_all_wNames$value+48208
pft_area_frac_total<-
pft_area_frac_all_wNames %>% 
dplyr::filter(metric == "area") %>%
#group_by(CAT, metric, value) %>%
group_by(CAT, value) %>%
summarize(TotalMetric = sum(value))

dim(pft_area_frac_total)

jpeg("figures/PatchFrac_all.jpg", width = 1000, height = 700)
ggplot(pft_area_frac_all_wNames %>% group_by(CAT) %>% filter(metric == "frac"), aes(x=CAT, y=sqrt(value_pos)))+ 
#geom_violin(aes(fill=CAT))
#dev.off()
#min_patch_size = min(log10(pft_area_frac_all$value*100000)) 
pfts<-unique(pft_area_all$class)
#pft_area_frac_all_wNames_filt<- pft_area_frac_all_wNames %>% dplyr::filter(value>min_patch_size)  %>% dim

jpeg("figures/PatchSize_all.jpg", width = 4000, height = 3000)
ggplot(pft_area_frac_all_wNames %>% dplyr::filter(metric == "area"), aes(x = log10(value*100000), group=CAT)) + #geom_violin(aes(fill=CAT)) +#
geom_histogram()+
#geom_density(aes(color=image)) + 
theme(panel.background = element_rect(fill = "white"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=50),
        strip.text = element_text(size = 50),
        axis.text = element_text(size = 50)
        #axis.text.x = element_text(angle = 90)) +
  ) + 
  xlab("Patch size log10 m2") +
  #scale_x_continuous(trans = "log", breaks=c(1,3,10,30,100,300,1000,3000), name="No. obs") +
  #geom_vline(yintercept = log10(0.038^2), color="green") +
  #geom_vline(yintercept = log10(0.1^2), color="purple") +
  geom_vline(xintercept = log10(1), color="yellow", size = 2) +
  geom_vline(xintercept = log10(4^2), color="red", size = 2) +
  geom_vline(xintercept = log10(10^2), color = "blue", size = 2) +
  geom_vline(xintercept = log10(30^2), color= "grey", size = 2) +
  facet_wrap(vars(CAT), scales = "free", ncol = 3) 

  dev.off()
 

jpeg("figures/PatchSize_TotalArea.jpg", width = 4000, height = 3000)
ggplot(pft_area_frac_total, aes(x = log10(TotalMetric*100000), group=CAT)) + 
#geom_violin(aes(fill=CAT)) +#
#geom_density(aes(color=image)) + 
geom_histogram()+
theme(panel.background = element_rect(fill = "white"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=50),
        strip.text = element_text(size = 50),
        axis.text = element_text(size = 50)
        #axis.text.x = element_text(angle = 90)) +
  ) + 
  #xlab("Patch size log10 m2") +
  #scale_x_continuous(trans = "log10", breaks=c(1,3^2,10^2,30^2,100^2,300^2, 1000^2), name="Pixel size") +
  #geom_vline(yintercept = log10(0.038^2), color="green") +
  #geom_vline(yintercept = log10(0.1^2), color="purple") +
  geom_vline(xintercept = log10(1), color="yellow") +
  geom_vline(xintercept = log10(3^2), color="red") +
  geom_vline(xintercept = log10(10^2), color = "blue") +
  geom_vline(xintercept = log10(30^2), color= "grey") +
  facet_wrap(vars(CAT), scales = "free_y", ncol = 3) 
  dev.off()
