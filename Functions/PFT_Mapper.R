PFT_Mapper<-function(path)
{
  tst<-raster(path) 
  tst_rat<-ratify(tst)
  rat <- levels(tst_rat)[[1]] %>% as.data.frame() 
  rat<-rat %>% left_join(species_colors, by=c("ID"="ColorNum"))
  levels(tst) <- rat
  levelplot(tst, col.regions=species_colors$Color, att="FNC_grp1", colorkey=list(space="left"))
}