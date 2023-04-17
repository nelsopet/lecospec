source("Functions/lecospectR.R")
mod_stats<-read.csv("gs_manifest_rf_ntree.csv")
pls_mod_stats<-read.csv("gs_manifest_pls_ncomp.csv")

colnames(mod_stats)

#RF mod stats plots
mod_plot<-ggplot(mod_stats %>% dplyr::filter(n<61) %>% dplyr::filter(source!="grd_raw_raw.csv"), aes(n,r2))+
geom_line(aes(linetype=source), lwd=1.2) + 
labs(x = "Number of trees", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=10)) +
scale_color_manual(values = source, name = "Data Source", labels = c("ground corrected and image", "ground corrected", "image raw")) #+

windows();mod_plot
#windows();
mod_plot<-mod_plot+#theme_classic()
  mod_plot+
windows(); mod_plot+facet_wrap(vars(source), scales ="fixed")

#pls mod stats plots
pls_mod_plot<-ggplot(pls_mod_stats, aes(n,accuracy))+
geom_line(aes(linetype=source), lwd=1.2) + 
labs(x = "Number of components", x = "accuracy") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=10)) +
scale_color_manual(values = source, name = "Data Source", labels = c("ground corrected and image", "ground corrected", "image raw")) #+

windows();pls_mod_plot
#windows();
windows(); pls_mod_plot+facet_wrap(vars(source), scales ="fixed")
