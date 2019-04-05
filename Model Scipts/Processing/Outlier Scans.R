##r<-as.data.frame(cbind(alaskaSpecLib$PFT_2,alaskaSpecLib$PFT))
##napar/neparc??
AK2018_spectra %>% subset(PFT=='bryoria') %>% select(-sample_name,-ScanID, -area, -PFT) %>% nrow()
test<-AK2018_spectra %>% subset(PFT=='bryoria') %>% select(-sample_name,-ScanID, -area, -PFT) %>% nrow()
plot_interactive(test)