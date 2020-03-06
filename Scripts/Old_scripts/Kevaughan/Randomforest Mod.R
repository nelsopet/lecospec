library(randomForest)
library(tidyverse)

##Reads in aall the predictors
alaskaSpecLib_plants_equal10_smooth    <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal10_smooth.csv"    )
alaskaSpecLib_plants_equal15_smooth    <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal15_smooth.csv"    )
alaskaSpecLib_plants_equal20_smooth    <-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal20_smooth.csv"    )
alaskaSpecLib_plants_equal10_5nm_smooth<-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal10_5nm_smooth.csv")
alaskaSpecLib_plants_equal15_5nm_smooth<-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal15_5nm_smooth.csv")
alaskaSpecLib_plants_equal20_5nm_smooth<-read.csv("Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal20_5nm_smooth.csv")
PCA_preds_equal10                      <-read.csv("Seniorproj_outcomes/Rawdata/PCA_preds_equal10.csv"                      )
PCA_preds_equal15                      <-read.csv("Seniorproj_outcomes/Rawdata/PCA_preds_equal15.csv"                      )
PCA_preds_equal20                      <-read.csv("Seniorproj_outcomes/Rawdata/PCA_preds_equal20.csv"                      )
PCA_preds_equal10_smooth               <-read.csv("Seniorproj_outcomes/Rawdata/PCA_preds_equal10.csv"                      )
PCA_preds_equal15_smooth               <-read.csv("Seniorproj_outcomes/Rawdata/PCA_preds_equal15.csv"                      )
PCA_preds_equal20_smooth               <-read.csv("Seniorproj_outcomes/Rawdata/PCA_preds_equal20.csv"                      )
PCA_preds_equal10_smooth_05nm          <-read.csv("Seniorproj_outcomes/Rawdata/.csv"                                       )
PCA_preds_equal15_smooth_05nm          <-read.csv("Seniorproj_outcomes/Rawdata/.csv"                                       )
PCA_preds_equal20_smooth_05nm          <-read.csv("Seniorproj_outcomes/Rawdata/.csv"                                       )
VEG_Index_equal10                      <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal10.csv"                      )
VEG_Index_equal15                      <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal15.csv"                      )
VEG_Index_equal20                      <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal20.csv"                      )
VEG_Index_equal10_smooth               <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal10_smooth.csv"               )
VEG_Index_equal15_smooth               <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal15_smooth.csv"               )
VEG_Index_equal20_smooth               <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal20_smooth.csv"               )
VEG_Index_equal10_smooth_05nm          <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal10_smooth_05nm.csv"          )
VEG_Index_equal15_smooth_05nm          <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal15_smooth_05nm.csv"          )
VEG_Index_equal20_smooth_05nm          <-read.csv("Seniorproj_outcomes/Rawdata/VEG_Index_equal20_smooth_05nm.csv"          )

##Removes unwanted metadata from dataframes
alaskaSpecLib_plants_equal10_smooth    [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL 
alaskaSpecLib_plants_equal15_smooth    [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL 
alaskaSpecLib_plants_equal20_smooth    [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL 
alaskaSpecLib_plants_equal10_5nm_smooth[c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL 
alaskaSpecLib_plants_equal15_5nm_smooth[c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL 
alaskaSpecLib_plants_equal20_5nm_smooth[c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal10                      [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal15                      [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal20                      [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal10_smooth               [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal15_smooth               [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal20_smooth               [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal10_smooth_05nm          [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal15_smooth_05nm          [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL
VEG_Index_equal20_smooth_05nm          [c("ScanID","PFT","PFT_3","area","Freq","sample_name")] = NULL

##Creates randomforest model for each predictor
rf_alaskaSpecLib_plants_equal10_smooth    <-randomForest(PFT_2~.,data=alaskaSpecLib_plants_equal10_smooth    ,mtry=5,ntree=2001,importance=TRUE)
rf_alaskaSpecLib_plants_equal15_smooth    <-randomForest(PFT_2~.,data=alaskaSpecLib_plants_equal15_smooth    ,mtry=5,ntree=2001,importance=TRUE)
rf_alaskaSpecLib_plants_equal20_smooth    <-randomForest(PFT_2~.,data=alaskaSpecLib_plants_equal20_smooth    ,mtry=5,ntree=2001,importance=TRUE)
rf_alaskaSpecLib_plants_equal10_5nm_smooth<-randomForest(PFT_2~.,data=alaskaSpecLib_plants_equal10_5nm_smooth,mtry=5,ntree=2001,importance=TRUE)
rf_alaskaSpecLib_plants_equal15_5nm_smooth<-randomForest(PFT_2~.,data=alaskaSpecLib_plants_equal15_5nm_smooth,mtry=5,ntree=2001,importance=TRUE)
rf_alaskaSpecLib_plants_equal20_5nm_smooth<-randomForest(PFT_2~.,data=alaskaSpecLib_plants_equal20_5nm_smooth,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal10                      <-randomForest(PFT_2~.,data=PCA_preds_equal10                      ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal15                      <-randomForest(PFT_2~.,data=PCA_preds_equal15                      ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal20                      <-randomForest(PFT_2~.,data=PCA_preds_equal20                      ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal10_smooth               <-randomForest(PFT_2~.,data=PCA_preds_equal10_smooth               ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal15_smooth               <-randomForest(PFT_2~.,data=PCA_preds_equal15_smooth               ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal20_smooth               <-randomForest(PFT_2~.,data=PCA_preds_equal20_smooth               ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal10_smooth_05nm          <-randomForest(PFT_2~.,data=PCA_preds_equal10_smooth_05nm          ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal15_smooth_05nm          <-randomForest(PFT_2~.,data=PCA_preds_equal15_smooth_05nm          ,mtry=5,ntree=2001,importance=TRUE)
rf_PCA_preds_equal20_smooth_05nm          <-randomForest(PFT_2~.,data=PCA_preds_equal20_smooth_05nm          ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal10                      <-randomForest(PFT_2~.,data=VEG_Index_equal10                      ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal15                      <-randomForest(PFT_2~.,data=VEG_Index_equal15                      ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal20                      <-randomForest(PFT_2~.,data=VEG_Index_equal20                      ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal10_smooth               <-randomForest(PFT_2~.,data=VEG_Index_equal10_smooth               ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal15_smooth               <-randomForest(PFT_2~.,data=VEG_Index_equal15_smooth               ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal20_smooth               <-randomForest(PFT_2~.,data=VEG_Index_equal20_smooth               ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal10_smooth_05nm          <-randomForest(PFT_2~.,data=VEG_Index_equal10_smooth_05nm          ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal15_smooth_05nm          <-randomForest(PFT_2~.,data=VEG_Index_equal15_smooth_05nm          ,mtry=5,ntree=2001,importance=TRUE)
rf_VEG_Index_equal20_smooth_05nm          <-randomForest(PFT_2~.,data=VEG_Index_equal20_smooth_05nm          ,mtry=5,ntree=2001,importance=TRUE)

#Creates dataframe showing the OOB estimate of error rate of the model
OOB_alaskaSpecLib_plants_equal10_smooth    <-rf_alaskaSpecLib_plants_equal10_smooth    $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth"))%>%mutate(Category="allbands")
OOB_alaskaSpecLib_plants_equal15_smooth    <-rf_alaskaSpecLib_plants_equal15_smooth    $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth"))%>%mutate(Category="allbands")
OOB_alaskaSpecLib_plants_equal20_smooth    <-rf_alaskaSpecLib_plants_equal20_smooth    $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth"))%>%mutate(Category="allbands")
OOB_alaskaSpecLib_plants_equal10_5nm_smooth<-rf_alaskaSpecLib_plants_equal10_5nm_smooth$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth"))%>%mutate(Category="05nmbands")
OOB_alaskaSpecLib_plants_equal15_5nm_smooth<-rf_alaskaSpecLib_plants_equal15_5nm_smooth$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth"))%>%mutate(Category="05nmbands")
OOB_alaskaSpecLib_plants_equal20_5nm_smooth<-rf_alaskaSpecLib_plants_equal20_5nm_smooth$err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth"))%>%mutate(Category="05nmbands")
OOB_PCA_preds_equal10                      <-rf_PCA_preds_equal10                      $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw_pca"))%>%mutate(Category="allbands")
OOB_PCA_preds_equal15                      <-rf_PCA_preds_equal15                      $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw_pca"))%>%mutate(Category="allbands")
OOB_PCA_preds_equal20                      <-rf_PCA_preds_equal20                      $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw_pca"))%>%mutate(Category="allbands")
OOB_PCA_preds_equal10_smooth               <-rf_PCA_preds_equal10_smooth               $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_pca"))%>%mutate(Category="allbands")
OOB_PCA_preds_equal15_smooth               <-rf_PCA_preds_equal15_smooth               $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_pca"))%>%mutate(Category="allbands")
OOB_PCA_preds_equal20_smooth               <-rf_PCA_preds_equal20_smooth               $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_pca"))%>%mutate(Category="allbands")
OOB_PCA_preds_equal10_smooth_05nm          <-rf_PCA_preds_equal10_smooth_05nm          $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_pca"))%>%mutate(Category="05nmbands")
OOB_PCA_preds_equal15_smooth_05nm          <-rf_PCA_preds_equal15_smooth_05nm          $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_pca"))%>%mutate(Category="05nmbands")
OOB_PCA_preds_equal20_smooth_05nm          <-rf_PCA_preds_equal20_smooth_05nm          $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_pca"))%>%mutate(Category="05nmbands")
OOB_VEG_Index_equal10                      <-rf_VEG_Index_equal10                      $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw_VIs"))%>%mutate(Category="allbands")
OOB_VEG_Index_equal15                      <-rf_VEG_Index_equal15                      $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw_VIs"))%>%mutate(Category="allbands")
OOB_VEG_Index_equal20                      <-rf_VEG_Index_equal20                      $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Raw_VIs"))%>%mutate(Category="allbands")
OOB_VEG_Index_equal10_smooth               <-rf_VEG_Index_equal10_smooth               $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_VIs"))%>%mutate(Category="allbands")
OOB_VEG_Index_equal15_smooth               <-rf_VEG_Index_equal15_smooth               $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_VIs"))%>%mutate(Category="allbands")
OOB_VEG_Index_equal20_smooth               <-rf_VEG_Index_equal20_smooth               $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_VIs"))%>%mutate(Category="allbands")
OOB_VEG_Index_equal10_smooth_05nm          <-rf_VEG_Index_equal10_smooth_05nm          $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_VIs"))%>%mutate(Category="05nmbands")
OOB_VEG_Index_equal15_smooth_05nm          <-rf_VEG_Index_equal15_smooth_05nm          $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_VIs"))%>%mutate(Category="05nmbands")
OOB_VEG_Index_equal20_smooth_05nm          <-rf_VEG_Index_equal20_smooth_05nm          $err.rate[2001,1]%>%as.data.frame()%>%setNames(.,c("Smooth_VIs"))%>%mutate(Category="05nmbands")


##Combines all those dataframes that were created showing the OOB estimate of error rate of the model
equal       <-c("equal10"
               ,"equal15"
               ,"equal20")

equal_smooth<-c("equal10_smooth"
                ,"equal15_smooth"
                ,"equal20_smooth"
                ,"equal10_5nm_smooth"
                ,"equal15_5nm_smooth"
                ,"equal20_5nm_smooth")

ModelOOB_Raw_spec_pca<-rbind(OOB_PCA_preds_equal10
                            ,OOB_PCA_preds_equal15
                            ,OOB_PCA_preds_equal20)%>%mutate(Model=equal)

ModelOOB_Raw_spec_VIs<-rbind(OOB_VEG_Index_equal10
                            ,OOB_VEG_Index_equal15
                            ,OOB_VEG_Index_equal20)%>%mutate(Model=equal)

ModelOOB_Smooth_spec<-rbind(OOB_alaskaSpecLib_plants_equal10_smooth    
                           ,OOB_alaskaSpecLib_plants_equal15_smooth    
                           ,OOB_alaskaSpecLib_plants_equal20_smooth    
                           ,OOB_alaskaSpecLib_plants_equal10_5nm_smooth
                           ,OOB_alaskaSpecLib_plants_equal15_5nm_smooth
                           ,OOB_alaskaSpecLib_plants_equal20_5nm_smooth)%>%mutate(Model=equal_smooth)

ModelOOB_Smooth_spec_pca<-rbind(OOB_PCA_preds_equal10_smooth     
                               ,OOB_PCA_preds_equal15_smooth     
                               ,OOB_PCA_preds_equal20_smooth     
                               ,OOB_PCA_preds_equal10_smooth_05nm
                               ,OOB_PCA_preds_equal15_smooth_05nm
                               ,OOB_PCA_preds_equal20_smooth_05nm)%>%mutate(Model=equal_smooth)  

ModelOOB_Smooth_spec_VIs<-rbind(OOB_VEG_Index_equal10_smooth     
                               ,OOB_VEG_Index_equal15_smooth     
                               ,OOB_VEG_Index_equal20_smooth     
                               ,OOB_VEG_Index_equal10_smooth_05nm
                               ,OOB_VEG_Index_equal15_smooth_05nm
                               ,OOB_VEG_Index_equal20_smooth_05nm)%>%mutate(Model=equal_smooth)


##Create a datatable showing all the values listed above
Modle_OOB_smooth_data<-cbind(ModelOOB_Smooth_spec,ModelOOB_Smooth_spec_pca[1],ModelOOB_Smooth_spec_VIs[1])%>%dplyr::select(Model,Category,everything())

Modle_OOB_raw_data<-cbind(ModelOOB_Raw_spec_pca,ModelOOB_Raw_spec_VIs[1])%>%dplyr::select(Model,Category,everything())

##Now we want the confusion matrix of the model with the higest accuracy
##By looking at the overall model results from the dataframes created above we can decide what models performed the best
##In that case the models that used the vegitation indices performed the best
##For the sake of not loosing data we will save the ouputs of the VI models where we had 10 and 20 scans per species
VIs_species_error_equal10<-rf_VEG_Index_equal10$confusion%>%as.data.frame
VIs_species_error_equal20<-rf_VEG_Index_equal20$confusion%>%as.data.frame


##Write out these results
write.csv(VIs_species_error_equal10,"Seniorproj_outcomes/VIs_species_error_equal10.csv")
write.csv(VIs_species_error_equal20,"Seniorproj_outcomes/VIs_species_error_equal20.csv")

write.csv(Modle_OOB_smooth_data,"Seniorproj_outcomes/Modle_OOB_smooth_data.csv")
write.csv(Modle_OOB_raw_data   ,"Seniorproj_outcomes/Modle_OOB_raw_data.csv"   )  
  
  
  
  
  
  
  
  _VEG_Index_equal20_smooth_05nm          <-