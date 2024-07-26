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
str(data)
plot(data$validation_counts, data$predicted_counts, col=data$sites)

ggplot(data, aes(validation_counts, predicted_counts)) + geom_point(aes(color=key))
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
