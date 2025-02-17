source("./Functions/lecospectR.R")


aggregation_key <- rjson::fromJSON(file = "./assets/pft_adj_list.json")


#Explore a single model

#Best adaboost model
#Model path but only useful for looking at model. Specify model in config.json
path = "./test/GridSearchResults/AdaBoost/862b797a-c8cb-485d-870c-e5f4742be5ce/862b797a-c8cb-485d-870c-e5f4742be5ce"
mod<-load_model(paste(path,"model.rda",sep="/"))

#Load testing dataset
test_data<-read.csv("Data/v2/test_50nm_500.csv")
test_data_fncGrp0<-change_aggregation(test_data$FncGrp1, 0, aggregation_key)
colnames(test_data)
test_data_pred<-predict(mod,test_data %>% subset(FncGrp1 != "Forb"))
test_data_pred_fncGrp0<-change_aggregation(test_data_pred, 0, aggregation_key)

accuracy(test_data_fncGrp0, test_data_pred_fncGrp0)
help(accuracy)
conf_mat<-caret::confusionMatrix(data = as.factor(test_data_pred_fncGrp0), as.factor(test_data_fncGrp0))


#Kappa stat is here
conf_mat$overall
#      Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull
#  8.437500e-01   8.073218e-01   7.780438e-01   8.962582e-01   2.500000e-01
#AccuracyPValue  McnemarPValue
#  4.772982e-56            NaN

write.csv(conf_mat$table, "figures/best_model_confusion_matrix.csv")
