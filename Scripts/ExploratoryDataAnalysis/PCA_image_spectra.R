require(Polychrome)
require(glue)
require(vegan)
require(glue)
source("Functions/lecospectR.R")

train_5nm<-read.csv("Data/data_v2/train_5nm_125_bands_only.csv")
head(train_5nm)
key<-rjson::fromJSON(file = "./assets/pft_adj_list.json")
train_5nm$FncGrp0<-change_aggregation(train_5nm$FncGrp1, 0, key)

fg0_names <- c(
    "Abiotic",
    "Forb",
    "Graminoid",
    "Lichen",
    "Moss",
    "BroadleafDecid",
    "ConiferEvergreen",
    "Unknown"
)
fg0_list<-cbind(fg0_names,fg0_palette) %>% as.data.frame()
train_5nm<-inner_join(train_5nm,fg0_list, by=c("FncGrp0"="fg0_names"), keep=FALSE)
img_mat<-train_5nm %>% 
  dplyr::select(-X, -UID, -FncGrp1, -FncGrp0, -Site, -fg0_palette) %>% 
  as.matrix() 
#Build PCA with and without sqrt transform
img_pca<-princomp(img_mat) #, center=FALSE, scale=FALSE)
summary(img_pca)
#img_pca_pr<-prcomp(img_mat[,25:500])#, center=FALSE, scale=FALSE)

#PCA figures with image spectra
seq(1:length(unique(train_5nm$FncGrp0))) %>% max()
cols<-palette.colors(n=6)
train_5nm_reclass<-train_5nm %>% 
dplyr::mutate(
        FncGrp0_num = case_when(
            FncGrp0 ==  "Abiotic" ~ 0,
            FncGrp0 ==  "BroadleafDecid" ~1,
            FncGrp0 ==  "ConiferEvergreen" ~2,
            FncGrp0 ==  "Forb" ~3,
            FncGrp0 ==  "Graminoid" ~4,
            FncGrp0 ==  "Lichen" ~5,
            FncGrp0 ==  "Moss" ~6,
            FncGrp0 ==  "Unknown" ~7
        ), .keep = "unused"
    )

##PCA plot Axes 1 vs 2
#windows()
jpeg("figures/PCA_125pixPerPFT_5nm.jpg")
#par(bg="#d2d2d2")
plot(scores(img_pca)[,1:2], col=train_5nm$fg0_palette, pch=train_5nm_reclass$FncGrp0_num)

#plot(scores(img_pca)[,1:2], col=fncgrp1_color_list$Color)#, pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))))

#biplot(tst_pca)
title(main="PCA of PFT Reflectance")
legend(x = -3, y =1.5, legend=unique(train_5nm$FncGrp0), lty=1, pch = unique(train_5nm_reclass$FncGrp0_num), col=unique(train_5nm$fg0_palette), cex=1)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
#legend(x = 300, y =100, legend=unique(PFT_IMG_SPEC_clean_merge$Area), pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))), cex=0.8)
dev.off()

##PCA plot Axes 2 vs 3
jpeg("figures/PCA_125pixPerPFT_5nm_Axes23.jpg")
#par(bg="#d2d2d2")
plot(scores(img_pca)[,2:3], col=train_5nm$fg0_palette, pch=train_5nm_reclass$FncGrp0_num)

#plot(scores(img_pca)[,1:2], col=fncgrp1_color_list$Color)#, pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))))

#biplot(tst_pca)
title(main="PCA of PFT Reflectance")
legend(x = -3, y =1.5, legend=unique(train_5nm$FncGrp0), lty=1, pch = unique(train_5nm_reclass$FncGrp0_num), col=unique(train_5nm$fg0_palette), cex=1)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
#legend(x = 300, y =100, legend=unique(PFT_IMG_SPEC_clean_merge$Area), pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))), cex=0.8)
dev.off()

##PCA plot Axes 1 vs 3
jpeg("figures/PCA_125pixPerPFT_5nm_Axes13.jpg")
#par(bg="#d2d2d2")
plot(scores(img_pca)[,c(1,3)], col=train_5nm$fg0_palette, pch=train_5nm_reclass$FncGrp0_num)

#plot(scores(img_pca)[,1:2], col=fncgrp1_color_list$Color)#, pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))))

#biplot(tst_pca)
title(main="PCA of PFT Reflectance")
legend(x = -3, y =1.5, legend=unique(train_5nm$FncGrp0), lty=1, pch = unique(train_5nm_reclass$FncGrp0_num), col=unique(train_5nm$fg0_palette), cex=1)
#legend(x = -200, y =-700, legend=unique(Speclib_merged$Source), pch=c(1:2), cex=0.5)
#legend(x = 300, y =100, legend=unique(PFT_IMG_SPEC_clean_merge$Area), pch=c(1:length(unique(PFT_IMG_SPEC_clean_merge$Area))), cex=0.8)
dev.off()

