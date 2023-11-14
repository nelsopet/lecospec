#Package load
source("./Functions/lecospectR.R")
require(Polychrome)
require(vegan)
require(glue)

#Load train and test split for a given max sample size per PFT and bandpass
dir<-list.files("./Data/data_v2") 
bands<-read.csv("./assets/band_cols.csv")
vegindices<-read.csv("./assets/vegIndicesUsed.csv")
train_dir<-dir[grepl("^train", dir)]
test_dir<-dir[grepl("^test", dir)]

train_file<-train_dir[53] #%>% gsub(".csv","")
train<-read.csv(paste("./Data/data_v2/", train_file, sep=""))

#Create a list of colors for plotting fnc grp 1 with the image data
train_fncgrp1_colors = createPalette(length(unique(train$FncGrp1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
  as.data.frame() %>%
  dplyr::rename(Color = ".") %>%
  mutate(FNC_grp1 = unique(train$FncGrp1)) %>%
  mutate(ColorNum = seq(1:length(unique(train$FncGrp1))))

train_fncgrp1_colors<-inner_join(train, train_fncgrp1_colors, by = c("FncGrp1"="FNC_grp1"), keep=TRUE) %>% 
    dplyr::select(colnames(train_fncgrp1_colors))

#Tally PFT spectra by site
train %>% group_by(Site, FncGrp1) %>% tally() %>% pivot_wider(values_from = n, names_from = Site) 

#Make a matrix of just bands
train_mat<-train[bands$names]

#PCA bands only PFT balanced PFT image spectra
train_pca<-prcomp(train_mat)

#Show how many axes are needed
#screeplot(train_pca)

#X11()
#Make biplot of first two PCA axes colored by FncGrp1
jpeg(paste("./figures/PCA_ImageSpectra", gsub(".csv","",train_file), "_AXIS1_2.jpg", sep=""))
plot(scores(train_pca)[,1:2], col=train_fncgrp1_colors$Color)
title(main="PCA reflectance of PFT image spectra only (no veg indices)")
legend(x = 1, y =-1, legend=unique(train$FncGrp1), lty=1, col=unique(train_fncgrp1_colors$Color), cex=1, pch=1)
dev.off()

jpeg(paste("./figures/PCA_ImageSpectra", gsub(".csv","",train_file), "_AXIS2_3.jpg", sep=""))
plot(scores(train_pca)[,3:2], col=train_fncgrp1_colors$Color)
dev.off()

#Test for differences between PFTs in balanced dataset

#Multivariate analysis of PFT groups 
key<-rjson::fromJSON(file = "./assets/pft_adj_list.json")
train$FncGrp0<-change_aggregation(train$FncGrp1, 0, key) 
train<- train$ %>% dplyr::select(X, UID, Site, FncGrp1, FncGrp0, everything())
#spectra_PFT_adonis<-adonis2(as.matrix(train[,6:ncol(train)])~as.factor(train$FncGrp0)*as.factor(train$Site), method="euclidean", permutations=500)
spectra_PFT_adonis<-adonis2(train[,6:ncol(train)]~as.factor(train$FncGrp0)*as.factor(train$Site), permutations = 1000, method = 'euclidean')

spectra_PFT_adonis
