source("Functions/lecospectR.R")
require(Metrics)
path = "./test/GridSearchResults/AdaBoost/862b797a-c8cb-485d-870c-e5f4742be5ce/862b797a-c8cb-485d-870c-e5f4742be5ce"
files=list.files(path)
files_keep<-files[grep(".csv",files)]
csvs<-lapply(1:length(files_keep),function (x) {read.csv(paste(path,files_keep[x],sep="/"))})
csv_merge<-Reduce(rbind,csvs)
sites<-substr(files_keep,17,17)
quadrats<-substr(files_keep,26,27)
quadrats<-gsub("\\.","",quadrats)
data<-cbind(sites, quadrats, csv_merge) %>% dplyr::select(-X) %>% subset(key != "Forb") %>% subset(key != "Unknown")
data_plot<-data %>% mutate(Site = case_when(sites == 1 ~ "Bison Gulch",
                        sites == 2  ~ "Chatanika",
                        sites ==3 ~ "Eight Mile",
                        sites == 4 ~ "Bonanza"))
str(data)
plot(data$validation_counts, data$predicted_counts, col=data$sites)

jpeg("figures/obs_vs_pred_best_mod.jpg", width = 2200, height = 2000)
ggplot(data_plot, aes(validation_prop, prediction_prop)) +
 facet_wrap(vars(key)) +
 #theme_bw()+
 geom_abline (slope=1, linetype = "solid", color="black") +
 geom_point(aes(color=Site, size = 48)) +
labs(y = "Predicted Cover", x = "Observed Cover") +
theme(panel.background = element_rect(fill = "white", colour = "grey50"), 
        #legend.key.size = unit(0.5, "cm"),legend.text = element_text(size=25),
        #legend.position = "none",
        title = element_text(size=55),
        strip.text = element_text(size = 45),
        axis.text = element_text(size = 45),
        legend.key.size = unit(1, "cm"),
        legend.text =element_text(size=45),
        legend.position="bottom")+
        #ggtitle("Adaboost R2 vs accuracy")+ 
        guides(color = guide_legend(override.aes = list(size = 15)), size = NULL)
dev.off()

Metrics::rmse()
data_stats<-data %>% 
group_by(sites,key) %>% 
summarise(R2 = cor(validation_counts,predicted_counts)^2,
            rsq = 1 - sum((validation_counts-predicted_counts) ^ 2)/sum((validation_counts - mean(validation_counts)) ^ 2),
            MSE = mse(validation_counts,predicted_counts),
            MAE = mae(validation_counts,predicted_counts),
            RMSE = rmse(validation_counts,predicted_counts),
            slope = cov(validation_counts,predicted_counts) / var(validation_counts),
            intercept = mean(predicted_counts) - slope * mean(validation_counts)
            ) 

write.csv(data_stats,"./figures/best_model_prediction_stats.csv")
