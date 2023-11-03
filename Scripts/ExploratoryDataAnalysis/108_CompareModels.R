source("Functions/lecospectR.R")
rf_mod_stats<-read.csv("gs3_ranger_6.csv")
pls_mod_stats<-read.csv("gs3_pls_4.csv")
adb_mod_stats<-read.csv("gs3_adaboost_4.csv")
str(rf_mod_stats)
        range(rf_mod_stats$maxCount)
colnames(pls_mod_stats)
colnames(adb_mod_stats)

#Adaboost mod number of trees vs r2
adb_mod_plot<-ggplot(adb_mod_stats %>% mutate(Bandwith_nm = as.factor(bandwidth), Max_Pixels_per_PFT = as.factor(maxCount)), aes(log10(hyperparam1),r2))+
#geom_point(aes(lwd=1.2, color=Bandwith_nm, size = Max_Pixels_per_PFT)) + 
geom_jitter(aes(lwd=1.2, color=Bandwith_nm, size = Max_Pixels_per_PFT)) + 
#size = as.factor(maxCount, ordered=TRUE, levels=c(sort(unique(adb_mod_stats$maxCount))))
#geom_line(aes(linetype=as.factor(bandwidth), lwd=1.2)) + 
labs(x = "Number of trees", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(2, "cm"),
        legend.text =element_text(size=10)) +
        ggtitle("Adaboost r2 vs number of components")+
        guides(shape = guide_legend(override.aes = list(size = 10)))
#geom_point(aes(lwd=1.2, color=Bandwith_nm, size = Max_Pixels_per_PFT)) + 

        #scale_size_manual(values=c(sort(unique(adb_mod_stats$maxCount))), labels = c("125","300","500","750","1000","2000"))
#scale_color_manual(values = bandwidth, name = "bandwidth") +

windows();
adb_mod_plot#+geom_smooth()

#Adaboost mod accuracy vs r2
adb_mod_plot<-ggplot(adb_mod_stats, aes(accuracy,r2))+
#geom_point(aes(lwd=1.2, color=as.factor(bandwidth), size = as.factor(maxCount))) + 
geom_jitter(aes(lwd=1.2, color=as.factor(bandwidth), size = as.factor(maxCount))) + 

#geom_line(aes(linetype=as.factor(bandwidth), lwd=1.2)) + 
labs(x = "accuracy", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=10)) +
        ggtitle("Adaboost r2 vs accuracy")

#scale_color_manual(values = bandwidth, name = "bandwidth") +

windows();
adb_mod_plot



#RF mod number of trees vs r2
rf_mod_plot<-ggplot(rf_mod_stats %>% mutate(Bandwith_nm = as.factor(bandwidth), Max_Pixels_per_PFT = as.factor(maxCount)), aes(log10(hyperparam1),r2))+
#geom_point(aes(lwd=1.2, color=Bandwith_nm, size = Max_Pixels_per_PFT)) + 
geom_jitter(aes(lwd=1.2, color=Bandwith_nm, size = Max_Pixels_per_PFT)) + 
#size = as.factor(maxCount, ordered=TRUE, levels=c(sort(unique(rf_mod_stats$maxCount))))
#geom_line(aes(linetype=as.factor(bandwidth), lwd=1.2)) + 
labs(x = "Number of trees", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(2, "cm"),
        legend.text =element_text(size=10)) +
        ggtitle("Ranger r2 vs number of components")+
        guides(shape = guide_legend(override.aes = list(size = 10)))
#geom_point(aes(lwd=1.2, color=Bandwith_nm, size = Max_Pixels_per_PFT)) + 

        #scale_size_manual(values=c(sort(unique(rf_mod_stats$maxCount))), labels = c("125","300","500","750","1000","2000"))
#scale_color_manual(values = bandwidth, name = "bandwidth") +

windows();
rf_mod_plot#+geom_smooth()

#RF mod accuracy vs r2
rf_mod_plot<-ggplot(rf_mod_stats, aes(accuracy,r2))+
#geom_point(aes(lwd=1.2, color=as.factor(bandwidth), size = as.factor(maxCount))) + 
geom_jitter(aes(lwd=1.2, color=as.factor(bandwidth), size = as.factor(maxCount))) + 

#geom_line(aes(linetype=as.factor(bandwidth), lwd=1.2)) + 
labs(x = "accuracy", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=10)) +
        ggtitle("Ranger r2 vs accuracy")

#scale_color_manual(values = bandwidth, name = "bandwidth") +

windows();
rf_mod_plot


#windows();
mod_plot<-mod_plot+#theme_classic()
  mod_plot+
windows(); mod_plot+facet_wrap(vars(source), scales ="fixed")

#PLS mod number of copmonents vs r2
pls_mod_plot<-ggplot(pls_mod_stats, aes(log10(hyperparam1),r2))+
geom_point(aes(lwd=1.2, color=as.factor(bandwidth), size = as.factor(maxCount))) + 
#geom_line(aes(linetype=source), lwd=1.2) + 
labs(x = "Number of components", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=10)) +
        ggtitle("PLS r2 vs number of components")
#scale_color_manual(values = source, name = "Data Source", labels = c("ground corrected and image", "ground corrected", "image raw")) #+

windows();pls_mod_plot


#PLS mod accuracy vs r2
pls_mod_plot<-ggplot(pls_mod_stats, aes(accuracy,r2))+
geom_point(aes(lwd=1.2, color=as.factor(bandwidth), size = as.factor(maxCount))) + 
#geom_line(aes(linetype=as.factor(bandwidth), lwd=1.2)) + 
labs(x = "accuracy", x = "r2") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=25),
        strip.text = element_text(size = 25),
        axis.text = element_text(size = 20),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=10)) +
        ggtitle("PLS r2 vs accuracy")
#scale_color_manual(values = bandwidth, name = "bandwidth") +

windows();pls_mod_plot
#windows();
windows(); pls_mod_plot+facet_wrap(vars(source), scales ="fixed")
