#Load packages
source("./Functions/lecospectR.R")
require(vegan)
require(glue)
require(Polychrome)
require(PERMANOVA)
#Read in spectral libraries from both ground and image tagged with the same labels
#Ground spectra
Cleaned_Speclib <- read.csv("./Output/C_001_SC3_Cleaned_SpectralLib.csv") 
Cleaned_Speclib_derivs<-read.csv("./Output/D_002_SpecLib_Derivs.csv")

#List of column names to ignore 
#ignore<-colnames(Cleaned_Speclib[,1:40])
ground_cols_keep<-Cleaned_Speclib %>% dplyr::select(X:X2500) %>%
  colnames() 
  
  
ignore<-c(colnames(Cleaned_Speclib[,1:11]), "species_count")
ignore_derivs<-c(colnames(Cleaned_Speclib_derivs[,1:31]), "species_count")

#Rescale ground spectra to min max  
Cleaned_Speclib_tall_Fnc_grp1<-
    Cleaned_Speclib %>% 
    #Comment line below if data should be used before rescaling
    group_by(Functional_group1, Species_name) %>% 
    dplyr::select(Functional_group1, Species_name) %>% 
    unique() %>% 
    ungroup() %>% 
    group_by(Functional_group1) %>%
    tally() %>% 
    dplyr::rename(species_count = n) %>%
  inner_join(Cleaned_Speclib %>% dplyr::select(all_of(ground_cols_keep)), by="Functional_group1") %>% #colnames() %>% as.data.frame() %>% View()
  #columnwise_robust_scale(ignore_cols = ignore) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
  #standardize_df(ignore_cols = ignore) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"scans,"} {species_count} {"species"})')) %>%
  #mutate(Functional_group1_wN = Functional_group1) %>% 
  ungroup() %>% #colnames() %>% View()
  dplyr::select(X
                ,ScanID
                ,Area
                ,Code_name
                ,Species_name
                ,Functional_group1
                ,Functional_group2
                #,Species_name_Freq
                #,Functional_group1_Freq
                #,Functional_group2_Freq
                ,Functional_group1_wN
                ,sample_size
                ,species_count,
                everything()) %>%
  pivot_longer(cols = `X350`:`X2500`,  names_to  = "Wavelength", values_to = "Reflectance") %>%
  mutate(Wavelength = gsub("X","",Wavelength),
         Reflectance = Reflectance/100) %>% #dplyr::select(Wavelength) %>% unique() %>% as.data.frame() %>% View()
  group_by(Functional_group1_wN, Functional_group1,Wavelength) %>%  
  dplyr::summarise(Median_Reflectance = median(Reflectance),
                   Max_Reflectance = max(Reflectance),
                   Min_Reflectance = min(Reflectance),
                   Pct_87_5_Reflectance = quantile(Reflectance, probs = 0.875),
                   Pct_12_5_Reflectance = quantile(Reflectance, probs = 0.125),
                   Upper_Reflectance = quantile(Reflectance, probs = 0.95),
                   Lower_Reflectance = quantile(Reflectance, probs = 0.05))%>%
  mutate(Wavelength = as.numeric(Wavelength))  %>%
  as.data.frame() %>%
 dplyr::filter(Wavelength>397 & Wavelength<1000)

  
write.csv(Cleaned_Speclib_tall_Fnc_grp1, "./Data/C_001_SC3_Cleaned_SpectralLib_tall_Fnc_grp1.csv")

jpeg("Output/Fnc_grp1_spectral_profiles_AllSpectra.jpg", height = 10000, width = 9000, res = 350)
ggplot(Cleaned_Speclib_tall_Fnc_grp1, aes(Wavelength, Median_Reflectance, group = Functional_group1), scales = "fixed")+
  labs(title = c("Reflectance by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 2561 scans"), y="Reflectance")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_line(aes(Wavelength, Median_Reflectance,color = "red"),size = 2)+  
  geom_ribbon(aes(Wavelength, ymin = Pct_12_5_Reflectance, ymax = Pct_87_5_Reflectance), alpha = 0.3) +
  geom_ribbon(aes(Wavelength, ymin = Lower_Reflectance, ymax = Upper_Reflectance), alpha = 0.2) +
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
dev.off()


#Plot veg indices
Cleaned_Speclib_derivs_tall_Fnc_grp1<-
  Cleaned_Speclib_derivs %>% 
  #Comment line below if data should be used before rescaling
  #global_min_max_scale(ignore_cols = ignore) %>%  
  group_by(Functional_group1, Species_name) %>% 
  dplyr::select(Functional_group1, Species_name) %>% 
  unique() %>% 
  ungroup() %>% 
  group_by(Functional_group1) %>%
  tally() %>% 
  dplyr::rename(species_count = n) %>%
  inner_join((Cleaned_Speclib_derivs %>% dplyr::select(-X397.593_5nm:-X897.593_5nm)), by="Functional_group1") %>% #colnames()
  columnwise_min_max_scale(ignore_cols = ignore_derivs) %>% #head() %>% View()#colnames() #as.matrix() %>% hist()
  group_by(Functional_group1) %>% 
  dplyr::mutate(sample_size = n()) %>% 
  dplyr::mutate(Functional_group1_wN = glue('{Functional_group1} {"(n="} {sample_size} {"scans,"} {species_count} {"species"})')) %>%
  #mutate(Functional_group1_wN = Functional_group1) %>% 
  ungroup() %>% #colnames() %>% View()
  dplyr::select(#X
                ScanID
                ,Area
                ,Code_name
                ,Species_name
                ,Functional_group1
                ,Functional_group2
                #,Species_name_Freq
                #,Functional_group1_Freq
                #,Functional_group2_Freq
                ,Functional_group1_wN
                ,sample_size
                ,species_count,
                everything()) %>%
  pivot_longer(cols = Boochs:Vogelmann4,  names_to  = "Veg_Index", values_to = "Value") %>%
 # mutate(Wavelength = gsub("X","",Wavelength)) %>% #dplyr::select(Wavelength) %>% unique() %>% as.data.frame() %>% View()
  group_by(Functional_group1_wN, Functional_group1,Veg_Index) %>%  
  dplyr::summarise(Median_Value = median(Value),
                   Max_Value = max(Value, na.rm = TRUE),
                   Min_Value = min(Value, na.rm = TRUE),
                   Pct_87_5_Value = quantile(Value, probs = 0.875, na.rm = TRUE),
                   Pct_12_5_Value = quantile(Value, probs = 0.125, na.rm = TRUE),
                   Upper_Value = quantile(Value, probs = 0.95, na.rm = TRUE),
                   Lower_Value = quantile(Value, probs = 0.05, na.rm = TRUE)
  ) %>%
  #mutate(Wavelength = as.numeric(Wavelength),
  #       Source = "Ground")  %>%
  as.data.frame() #%>% #str()
  #dplyr::filter(Wavelength>397 & Wavelength<1000)


jpeg("Output/Fnc_grp1_veg_indices_AllSpectra.jpg", height = 10000, width = 16000, res = 350)
ggplot(Cleaned_Speclib_derivs_tall_Fnc_grp1 %>% dplyr::arrange(desc(Median_Value)), aes(Veg_Index, Median_Value, group = Functional_group1), scales = "fixed")+
  #geom_ribbon(aes(Veg_Index, ymin = Min_Value, ymax = Max_Value, alpha = 0.3))+
  geom_ribbon(aes(Veg_Index, ymin = Pct_12_5_Value, ymax = Pct_87_5_Value, alpha = 0.3))+
  geom_ribbon(aes(Veg_Index, ymin = Lower_Value, ymax = Upper_Value, alpha = 0.2))+
  labs(title = c("Value by plant functional group and sample size with median (red), 75% (dark) and 90% (grey) quantiles based on 2561 scans"), y="Value")+
  theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(angle = 90)) +
  geom_point(aes(Veg_Index, Median_Value,color = "red"),size = 2)+
  facet_wrap(vars(Functional_group1_wN), scales = "fixed", ncol = 3) 
dev.off()


                fnc_grp1_colors = createPalette(length(unique(Cleaned_Speclib_derivs$Functional_group1)),  c("#ff0000", "#00ff00", "#0000ff")) %>%
                  as.data.frame() %>%
                  dplyr::rename(Color = ".") %>%
                  mutate(Functional_group1 = unique(Cleaned_Speclib_derivs$Functional_group1)) %>%
                  mutate(ColorNum = seq(1:length(unique(Cleaned_Speclib_derivs$Functional_group1))));
                
                fnc_grp1_color_list<-Cleaned_Speclib_derivs %>% dplyr::select(Functional_group1) %>% inner_join(fnc_grp1_colors, by="Functional_group1", keep=FALSE)

                jpeg("Output/Fnc_grp1_veg_indices_AllSpectra_heatmap.jpg", height = 10000, width = 16000, res = 350)
                
            heatmap(as.matrix(Cleaned_Speclib_derivs %>% 
                                    dplyr::select(-X397.593_5nm:-X897.593_5nm, -1:-32) %>% 
                                  columnwise_min_max_scale()), 
                      dendrogram="row", 
                      trace="none", 
                      Colv = FALSE,
                      RowSideColors = fnc_grp1_color_list$Color)
            
            legend(x='bottomright', legend=unique(fnc_grp1_color_list$Functional_group1), fill=unique(fnc_grp1_color_list$Color), cex=0.7)
            



##Bind all raw spectral libraries from ground and image.
merge_ignore1<-c("ScanID", "Functional_group1", "Area") #, "Species_name")

Cleaned_Speclib_merge <-
Cleaned_Speclib %>%
  dplyr::select(ScanID, Functional_group1, Area, X398:X998) %>% #colnames()
  #columnwise_min_max_scale(ignore_cols = merge_ignore1) %>% #colnames() #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
  dplyr::rename(UID=ScanID) %>%
  mutate(Source = "Ground") %>%
  #mutate(UID=c(Source, ScanID,Functional_group1))
  #dplyr::select(UID,Source, Functional_group1, X398:X998) %>%
  as.data.frame()

Cleaned_Speclib %>% group_by(Area,Functional_group1) %>% tally() %>% pivot_wider(names_from = Functional_group1, values_from = n)

#PCA of ground spectra only
ground_PFT_spectra_mat<-Cleaned_Speclib_merge %>% 
  dplyr::select(-UID,-Source, -Functional_group1, -Area) %>% 
  as.matrix() 

hist(ground_PFT_spectra_mat)
#Replace any NAs or Zeros with very small value
#ground_PFT_spectra_mat[ground_PFT_spectra_mat==0]<-0.00000001
#ground_PFT_spectra_mat[is.na(ground_PFT_spectra_mat)]<-0.00000001

##Test for differences between PFTs in spectral space using PERMANOVA
D<- IniTransform(ground_PFT_spectra_mat)
DD<- DistContinuous(D)
DD_MANOVA<-MANOVA(ground_PFT_spectra_mat, Group = Cleaned_Speclib_merge$Functional_group1)
plot(DD_MANOVA)
DD_adonis<-adonis2(ground_PFT_spectra_mat~as.factor(Cleaned_Speclib_merge$Functional_group1), method="euclidean", permutations=500)
DD_adonis
#NMS very slow
#DD_metaMDS<-vegan::metaMDS(ground_PFT_spectra_mat, dist="euclidean")
plot(DD_metaMDS)
DD_manova<-manova(ground_PFT_spectra_mat~Cleaned_Speclib_merge$Functional_group1)
summary(DD_manova)
#Runs very slow
#DD_PERM<-PERMANOVA(DD, group = as.factor(Cleaned_Speclib_merge$Functional_group1))
#DD_PERM
system.time()
#Build PCA with and without sqrt transform
ground_pca<-princomp(ground_PFT_spectra_mat) #, center=FALSE, scale=FALSE)
#ground_pca_pr<-prcomp(sqrt(ground_PFT_spectra_mat)) #, center=FALSE, scale=FALSE)

cols<-palette.colors(n=9)
screeplot(ground_pca)
plot(ground_pca$scores[,1:2], col=fnc_grp1_color_list$Color)#, pch=c(1:2))
title(main = "PCA of ground spectra")
legend("topleft", legend=unique(Cleaned_Speclib_merge$Functional_group1), lty=1, col=c(1:9), cex=1)

jpeg("./Output/PCA_Ground_boxplot_PC2.jpeg", width = 1200, height =400)
boxplot((ground_pca$scores[,2])~Cleaned_Speclib_merge$Functional_group1)
title(main = "Boxplot PFTs PCA axis 2 of ground spectra")
dev.off()
##PCA of veg indices of ground data

              ##Bind all raw spectral libraries from ground and image.
              merge_ignore1<-c("ScanID", "Functional_group1") #, "Species_name")
              
              Cleaned_Speclib_Derivs_merge <-
                Cleaned_Speclib_derivs %>%
                dplyr::select(ScanID, Functional_group1, Boochs:Vogelmann4) %>% #colnames()
                #global_min_max_scale(ignore_cols = merge_ignore1) %>% #colnames() #dplyr::select(X398:X998) %>% as.matrix() %>% hist()
                dplyr::rename(UID=ScanID) %>%
                mutate(Source = "Ground") %>%
                #mutate(UID=c(Source, ScanID,Functional_group1))
                #dplyr::select(UID,Source, Functional_group1, X398:X998) %>%
                as.data.frame()
              
              str(Cleaned_Speclib_Derivs_merge)             
              
              ground_PFT_derivs_mat<-Cleaned_Speclib_Derivs_merge %>% 
              dplyr::select(-UID,-Source, -Functional_group1) %>% #colnames
              columnwise_min_max_scale() %>% #str()
              as.matrix() 
              rownames(ground_PFT_derivs_mat)<-Cleaned_Speclib_Derivs_merge$UID
            
            #Replace any NAs or Zeros with very small value
            ground_PFT_derivs_mat[ground_PFT_derivs_mat==0]<-0.00000001
            ground_PFT_derivs_mat[is.na(ground_PFT_derivs_mat)]<-0.00000001
            
            #Build PCA with and without sqrt transform
            ground_pca_derivs<-princomp(ground_PFT_derivs_mat) #, center=FALSE, scale=FALSE)
            summary(ground_pca_derivs)
            #ground_pca_pr<-prcomp(sqrt(ground_PFT_derivs_mat)) #, center=FALSE, scale=FALSE)
            
            cols<-palette.colors(n=9)
            screeplot(ground_pca_derivs)
            jpeg("./Output/PCA_Ground_Veg_Indices_boxplot_PC1.jpeg", width = 1200, height =400)
            boxplot((ground_pca_derivs$scores[,1])*-1~Cleaned_Speclib_Derivs_merge$Functional_group1)
            title(main = "Boxplot PFTs PCA axis 1 of min max rescaled ground veg indices")
            
            
            dev.off()
            plot(ground_pca_derivs$scores[,1:2], col=fnc_grp1_color_list$Color)#, pch=c(1:2))
            title(main = "PCA of min max rescaled ground derivs")
            legend("topleft", legend=unique(Cleaned_Speclib_merge$Functional_group1), lty=1, col=c(1:9), cex=0.5)
            
  

 