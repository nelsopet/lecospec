##AVRIRS
################################Abiotic & All plants#######################################

##Creates training and testing dataset
dataset_size_all=floor(nrow(alaskaSpecLib_AV_all)*0.80)
index<-sample(1:nrow(alaskaSpecLib_AV_all),size=dataset_size_all)
training_all<-alaskaSpecLib_AV_all[index,]
testing_all<-alaskaSpecLib_AV_all[-index,]

###random forest model
rf_AV_all<-randomForest(PFT_2~.,data=training_all,mtry=5,ntree=2001,importance=TRUE)

#####################################Plants Only###########################################
##Creates training and testing dataset
dataset_size_plants=floor(nrow(alaskaSpecLib_AV_plants)*0.80)
index<-sample(1:nrow(alaskaSpecLib_AV_plants),size=dataset_size_plants)
training_plants<-alaskaSpecLib_AV_plants[index,]
testing_plants<-alaskaSpecLib_AV_plants[-index,]

###random forest model
rf_AV_plants<-randomForest(PFT_2~.,data=training_plants,mtry=5,ntree=2001,importance=TRUE)

######################################Lichens##############################################
##Creates training and testing dataset
dataset_size_lichens=floor(nrow(alaskaSpecLib_AV_lichens)*0.80)
index<-sample(1:nrow(alaskaSpecLib_AV_lichens),size=dataset_size_lichens)
training_lichens<-alaskaSpecLib_AV_lichens[index,]
testing_lichens<-alaskaSpecLib_AV_lichens[-index,]

###random forest model
rf_AV_lichen<-randomForest(PFT_2~.,data=training_lichen,mtry=5,ntree=2001,importance=TRUE)

#################################Lichens & Bryophytes######################################

##Creates training and testing dataset
dataset_size_lichen_bryo=floor(nrow(alaskaSpecLib_AV_lichen_bryo)*0.80)
index<-sample(1:nrow(alaskaSpecLib_AV_lichen_bryo),size=dataset_size_lichen_bryo)
training_lichen_bryo<-alaskaSpecLib_AV_lichen_bryo[index,]
testing_lichen_bryo<-alaskaSpecLib_AV_lichen_bryo[-index,]

###random forest model
rf_AV_lichen_bryo<-randomForest(PFT_2~.,data=training_lichen_bryo,mtry=5,ntree=2001,importance=TRUE)

###################################Vascular Plants#########################################

##Creates training and testing dataset
dataset_size_vascular=floor(nrow(alaskaSpecLib_vascular)*0.80)
index<-sample(1:nrow(alaskaSpecLib_vascular),size=dataset_size_vascular)
training_vascular<-alaskaSpecLib_vascular[index,]
testing_vascular<-alaskaSpecLib_vascular[-index,]

###random forest model
rf_AV_vascular<-randomForest(PFT_2~.,data=training_vascular,mtry=5,ntree=2001,importance=TRUE)