#######################Creates models from field spec data resapmepled on AVIRIS bandpasses######################################
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

###Reads in all predictors for scans
alaskaSpeclib_AV_df <-read.csv("Outputs/1_Field_spec/1_Processing/AVIRIS_data/alaskaSpecLib_data_AV.csv" )
alaskaSpeclib_AV_VIs<-read.csv("Outputs/1_Field_spec/1_Processing/AVIRIS_data/alaskaSpeclib_AV_VIs.csv")

##we'll need to apply a function to all dataframes that omits unwanted metadata
alaskaSpeclib_AV_df        [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpeclib_AV_VIs       [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL

set.seed(2017)
##Lets run some different models so we can asses whiich ones are the best for prediction later
rf_bandsVIs<-randomForest(PFT_3~.,data=alaskaSpeclib_AV_df        ,mtry=sqrt(ncol(alaskaSpeclib_AV_df        )),ntree=1001,localImp = TRUE) 
rf_VIs     <-randomForest(PFT_3~.,data=alaskaSpeclib_AV_VIs       ,mtry=sqrt(ncol(alaskaSpeclib_AV_VIs       )),ntree=1001,localImp = TRUE) 

##lets save all these models
save(rf_VIs        , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_VIs     .rda")
save(rf_bandsVIs   , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_bandsVIs.rda") 

###Let's take a look at the important variable to see if we could optimize our model
##First we want to take a look at the distribution of minimal depth 
min_depth_frame_rf_VIs     <- min_depth_distribution(rf_VIs     )
min_depth_frame_rf_bandsVIs<- min_depth_distribution(rf_bandsVIs)

##Lest grab the most relevant/ important variables
rf_VIs_plot10     <-plot_min_depth_distribution(min_depth_frame_rf_VIs     , min_no_of_trees = 200, mean_sample = "relevant_trees", k= 10)
rf_VIs_plot25     <-plot_min_depth_distribution(min_depth_frame_rf_VIs     , min_no_of_trees = 200, mean_sample = "relevant_trees", k= 25)
rf_VIs_plot50     <-plot_min_depth_distribution(min_depth_frame_rf_VIs     , min_no_of_trees = 200, mean_sample = "relevant_trees", k= 50)
rf_bandsVIs_plot10<-plot_min_depth_distribution(min_depth_frame_rf_bandsVIs, min_no_of_trees = 200, mean_sample = "relevant_trees", k= 10)
rf_bandsVIs_plot25<-plot_min_depth_distribution(min_depth_frame_rf_bandsVIs, min_no_of_trees = 200, mean_sample = "relevant_trees", k= 25)
rf_bandsVIs_plot50<-plot_min_depth_distribution(min_depth_frame_rf_bandsVIs, min_no_of_trees = 200, mean_sample = "relevant_trees", k= 50)

##Lets take the 10,25 and 50 most important variables and rebuild our models
rf_VIs_plot10n     <-unique(rf_VIs_plot10     $data$variable)%>%as.character()
rf_VIs_plot25n     <-unique(rf_VIs_plot25     $data$variable)%>%as.character()
rf_VIs_plot50n     <-unique(rf_VIs_plot50     $data$variable)%>%as.character()
rf_bandsVIs_plot10n<-unique(rf_bandsVIs_plot10$data$variable)%>%as.character()
rf_bandsVIs_plot25n<-unique(rf_bandsVIs_plot25$data$variable)%>%as.character()
rf_bandsVIs_plot50n<-unique(rf_bandsVIs_plot50$data$variable)%>%as.character()

##Lets subsET our spectral library to have just those variables
alaskaSpeclib_AV_VIs10<-alaskaSpeclib_AV_VIs%>%dplyr::select(PFT_3,rf_VIs_plot10n     )
alaskaSpeclib_AV_VIs25<-alaskaSpeclib_AV_VIs%>%dplyr::select(PFT_3,rf_VIs_plot25n     )
alaskaSpeclib_AV_VIs50<-alaskaSpeclib_AV_VIs%>%dplyr::select(PFT_3,rf_VIs_plot50n     )
alaskaSpeclib_AV10    <-alaskaSpeclib_AV_df %>%dplyr::select(PFT_3,rf_bandsVIs_plot10n)
alaskaSpeclib_AV25    <-alaskaSpeclib_AV_df %>%dplyr::select(PFT_3,rf_bandsVIs_plot25n)
alaskaSpeclib_AV50    <-alaskaSpeclib_AV_df %>%dplyr::select(PFT_3,rf_bandsVIs_plot50n)

##Now we can rebuild our model
rf_VIs10   <-randomForest(PFT_3~.,data=alaskaSpeclib_AV_VIs10       ,mtry=sqrt(ncol(alaskaSpeclib_AV_VIs10       )),ntree=1001,localImp = TRUE)
rf_VIs25   <-randomForest(PFT_3~.,data=alaskaSpeclib_AV_VIs25       ,mtry=sqrt(ncol(alaskaSpeclib_AV_VIs25       )),ntree=1001,localImp = TRUE)
rf_VIs50   <-randomForest(PFT_3~.,data=alaskaSpeclib_AV_VIs50       ,mtry=sqrt(ncol(alaskaSpeclib_AV_VIs50       )),ntree=1001,localImp = TRUE)
rf_AV10    <-randomForest(PFT_3~.,data=alaskaSpeclib_AV10           ,mtry=sqrt(ncol(alaskaSpeclib_AV10           )),ntree=1001,localImp = TRUE)
rf_AV25    <-randomForest(PFT_3~.,data=alaskaSpeclib_AV25           ,mtry=sqrt(ncol(alaskaSpeclib_AV25           )),ntree=1001,localImp = TRUE)
rf_AV50    <-randomForest(PFT_3~.,data=alaskaSpeclib_AV50           ,mtry=sqrt(ncol(alaskaSpeclib_AV50           )),ntree=1001,localImp = TRUE)

##lets save all these models
save(rf_VIs10   , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV_VIs10.rda")
save(rf_VIs25   , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV_VIs25.rda")  
save(rf_VIs50   , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV_VIs50.rda")
save(rf_AV10    , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV10.rda"    )
save(rf_AV25    , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV25.rda"    )
save(rf_AV50    , file = "Outputs/1_Field_spec/2_Models/AVIRIS/rf_AV50.rda"    )

##Lets create a data frame that will combine the class.error of all the categories of each model
rf_VIs_ConfusionMatrixa     <-rf_VIs     $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs     =class.error )
rf_bandsVIs_ConfusionMatrixa<-rf_bandsVIs$confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_bandsVIs=class.error )
rf_VIs10_ConfusionMatrixa   <-rf_VIs10   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs10   =class.error )
rf_VIs25_ConfusionMatrixa   <-rf_VIs25   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs25   =class.error )
rf_VIs50_ConfusionMatrixa   <-rf_VIs50   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs50   =class.error )
rf_AV10_ConfusionMatrixa    <-rf_AV10    $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_AV10    =class.error )
rf_AV25_ConfusionMatrixa    <-rf_AV25    $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_AV25    =class.error )
rf_AV50_ConfusionMatrixa    <-rf_AV50    $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_AV50    =class.error )

##Lets combine these dataframes so we can compare them later
rf_ConfusionMatrix_all<-cbind(rf_VIs_ConfusionMatrixa     
                             ,rf_bandsVIs_ConfusionMatrixa
                             ,rf_VIs10_ConfusionMatrixa   
                             ,rf_VIs25_ConfusionMatrixa   
                             ,rf_VIs50_ConfusionMatrixa   
                             ,rf_AV10_ConfusionMatrixa    
                             ,rf_AV25_ConfusionMatrixa    
                             ,rf_AV50_ConfusionMatrixa)   
                               
write.csv(rf_ConfusionMatrix_all,"Outputs/1_Field_spec/2_Models/AVIRIS/rf_ConfusionMatrix_all.csv")

