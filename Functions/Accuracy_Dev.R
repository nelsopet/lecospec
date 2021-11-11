
Accuracy<- function(QuadRaster, x)
    QuadRaster[[x]] %>% 
      as.data.frame() %>% 
      dplyr::rename(PFT_num = ".") %>% 
      dplyr::mutate(Pix_cnt = n()) %>% 
      group_by(PFT_num) %>% 
      dplyr::mutate(PFT_pct_ML = 100*(n()/Pix_cnt), Plot = "Bisongulch") %>% 
      unique() %>%
      left_join(fnc_grp2_colors, by = c("PFT_num" = "ColorNum")) %>%
      mutate(UID = paste("Q",(as.numeric(BisonQuadNames$CLASS_ID[x])-1), sep="")) %>%
      dplyr::rename(PFT = FNC_grp1) %>% 
      ungroup() %>%
      #dplyr::select()
      left_join(AKValid2019_flat, by = c("UID"="meters","PFT"="PFT", "Plot"="Plot"), keep=FALSE) %>%
      dplyr::rename(PFT_Pct_Human = TotCov) %>%
      dplyr::select(PFT, Pix_cnt, PFT_pct_ML, PFT_Pct_Human))