bc_pred<-raster("./Output/dev_FullCube/bc_north_6425_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
    writeRaster(bc_pred,"./Output/dev_FullCube/bc_north_6425_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif", overwrite=TRUE)
        bc_pred2<-raster("./Output/dev_FullCube/bz_6425_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.grd")
            writeRaster(bc_pred2,"./Output/dev_FullCube/bz_6425_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif", overwrite=TRUE)
bg_pred<-raster("./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_indices_only_no_treatment.grd")
    writeRaster(bg_pred,"./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_indices_only_no_treatment.tif", overwrite=TRUE)
bg_pred2<-raster("./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_img_indices_only_balanced.grd")
    writeRaster(bg_pred2,"./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_img_indices_only_balanced.tif", overwrite=TRUE)
bg_pred3<-raster("./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_raw_corrected_balanced.grd")
    writeRaster(bg_pred3,"./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_raw_corrected_balanced.tif", overwrite=TRUE)
        bg_pred4<-raster("./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
            writeRaster(bg_pred4,"./Output/dev_FullCube/bg_28_1511_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif", overwrite=TRUE)
                bg_pred5<-raster("./Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.grd")
                    writeRaster(bg_pred5,"./Output/dev_FullCube/bg_1511_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif", overwrite=TRUE)
em_pred<-raster("./Output/em_5968_fngrp1_PREDICTIONS_FINAL.grd")# %>% writeRaster("./Output/bg_1511_fngrp1_PREDICTIONS_FINAL.tif")
    em_pred2<-raster("./Output/dev_FullCube/em_17_5968_fncgrp1_PREDICTIONS_grd_corrected_balanced.grd")
            writeRaster(em_pred2,"./Output/dev_FullCube/em_17_5968_fncgrp1_PREDICTIONS_grd_corrected_balanced.tif", overwrite=TRUE)
        em_pred3<-raster("./Output/dev_FullCube/em_17_5968_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
            writeRaster(em_pred3,"./Output/dev_FullCube/em_17_5968_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif", overwrite=TRUE)
                em_pred4<-raster("./Output/dev_FullCube/em_17_5968_fncgrp1_PREDICTIONS_grd_corrected_balanced_15compPLS.grd")
                    writeRaster(em_pred4,"./Output/dev_FullCube/em_17_5968_fncgrp1_PREDICTIONS_grd_corrected_balanced_15compPLS.tif", overwrite=TRUE)
                em_pred5<-raster("./Output/dev_FullCube/em_5968_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.grd")
                    writeRaster(em_pred5,"./Output/dev_FullCube/em_5968_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif", overwrite=TRUE)

ch_pred<-raster("./Output/ch_0_fncgrp1_PREDICTIONS.grd") %>% writeRaster("./Output/ch_0_fncgrp1_PREDICTIONS.tif")
    ch_pred2<-raster("./Output/dev_FullCube/ch_59_0_fncgrp1_PREDICTIONS_grd_corrected_balanced.grd") #%>% 
        writeRaster(ch_pred2,"./Output/dev_FullCube/ch_59_0_fncgrp1_PREDICTIONS_grd_corrected_balanced.tif", overwrite=TRUE)
    ch_pred3<-raster("./Output/dev_FullCube/ch_59_0_fncgrp1_PREDICTIONS_img_indices_only_balanced.grd") #%>% 
        writeRaster(ch_pred3,"./Output/dev_FullCube/ch_59_0_fncgrp1_PREDICTIONS_img_indices_only_balanced.tif", overwrite=TRUE)
            ch_pred4<-raster("./Output/dev_FullCube/ch_59_0_fncgrp1_PREDICTIONS_grd_indices_only_balanced.grd")
                writeRaster(ch_pred4,"./Output/dev_FullCube/ch_59_0_fncgrp1_PREDICTIONS_grd_indices_only_balanced.tif", overwrite=TRUE)
                     ch_pred5<-raster("./Output/dev_FullCube/ch_59_10_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
                     writeRaster(ch_pred5,"./Output/dev_FullCube/ch_59_10_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif", overwrite=TRUE)
                        ch_pred6<-raster("./Output/dev_FullCube/ch_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.grd")
                            writeRaster(ch_pred6,"./Output/dev_FullCube/ch_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif", overwrite=TRUE)

md_pred<-raster("./Output/md_10350_fncgrp1_PREDICTIONS.grd")# %>% writeRaster("./Output/md_10350_fncgrp1_PREDICTIONS_FINAL.tif")
wd_pred<-raster("./Output/wd_0_fncgrp1_PREDICTIONS.grd") #%>% writeRaster("./Output/wd_0_fncgrp1_PREDICTIONS.tif")
    wd_pred<-raster("./Output/dev_FullCube/wd_51_0_fncgrp1_PREDICTIONS_img_indices_only_balanced.grd") #%>% writeRaster("./Output/wd_0_fncgrp1_PREDICTIONS.tif")
    writeRaster(wd_pred,"./Output/dev_FullCube/wd_51_0_fncgrp1_PREDICTIONS_img_indices_only_balanced.tif")
        wd_pred3<-raster("./Output/dev_FullCube/wd_51_0_fncgrp1_PREDICTIONS_img_indices_only_balanced.grd") #%>% writeRaster("./Output/wd_0_fncgrp1_PREDICTIONS.tif")
        writeRaster(wd_pred,"./Output/dev_FullCube/wd_51_0_fncgrp1_PREDICTIONS_img_indices_only_balanced.tif")

bt_pred<-raster("./Output/dev_FullCube/bt_58_0_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
        writeRaster(bt_pred,"./Output/dev_FullCube/bt_58_0_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif")
tm_pred<-raster("./Output/dev_FullCube/tm_52_0_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
        writeRaster(tm_pred,"./Output/dev_FullCube/tm_52_0_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif", overwrite=TRUE)
            tm_pred2<-raster("./Output/dev_FullCube/tm_22_2000_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.grd")
                writeRaster(tm_pred2,"./Output/dev_FullCube/tm_22_2000_fncgrp1_PREDICTIONS_grd_corrected_balanced_10tree.tif", overwrite=TRUE)
                    tm_pred3<-raster("./Output/dev_FullCube/tm_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.grd")
                        writeRaster(tm_pred3,"./Output/dev_FullCube/tm_0_fncgrp1_PREDICTIONS_img_balanced_1tree_ranger.tif", overwrite=TRUE)


raster("./Output/yk_11530_fncgrp1_PREDICTIONS.grd") %>% writeRaster("./Output/yk_11530_fncgrp1_PREDICTIONS.tiff")
raster( "./Output/ll_132_fncgrp1_PREDICTIONS.grd") %>% writeRaster( "./Output/ll_132_fncgrp1_PREDICTIONS.tiff")
raster("./Output/btl_o_fncgrp1_PREDICTIONS.grd")%>% writeRaster("./Output/btl_o_fncgrp1_PREDICTIONS.tif")

shapefile_path <- "Data/Vectors/EightMile_Quadrats_ALL.shp"
em_shapes <- sf::st_read(shapefile_path)


target_wkt <- sf::st_crs(em_pred)[[2]]
target_crs <- sp::CRS(target_wkt)
em_shapes_proj<-sf::st_transform(em_shapes,target_wkt) 
crs(em_shapes_proj)
st_write(obj=em_shapes_proj,dsn="./Data/Vectors/EightMile_Quadrats_ALL_proj.shp", append = FALSE)

as_tiff<-function(path) {raster::raster(paste0("./Output/",path,".grd")) %>% raster::writeRaster(paste0("./Output/",path,".tif"), overwrite=TRUE)}
path = "./Output/btl_o_fncgrp1_PREDICTIONS"

as_tiff(path)

imgs<-list.files("./Output/") %>% as.data.frame() %>% rename(file_name=".")
imgs_grd<-imgs %>% dplyr::filter(grepl(".grd", file_name)) 
imgs_grd<-str_split(imgs_grd$file_name,".grd", simplify = T)
imgs_grd<-imgs_grd[,1]

lapply(imgs_grd, as_tiff)

help(filter)
