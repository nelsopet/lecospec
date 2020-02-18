#######################Creates models from field spec data resapmepled on headwall bandpasses######################################
library(tidyverse)
library(randomForest)
library(randomForestExplainer)

###Reads in all predictors for scans
alaskaSpeclib_HDW_005nm       <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_005nm.csv"       )
alaskaSpeclib_HDW_010nm       <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_010nm.csv"       )
alaskaSpeclib_HDW_050nm       <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_050nm.csv"       )
alaskaSpeclib_HDW_100nm       <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_100nm.csv"       )
alaskaSpeclib_HDW_VIs         <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpeclib_HDW_VIs.csv"         )
alaskaSpecLib_data_HDWALLbands<-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpecLib_data_HDWALLbands.csv")
alaskaSpecLib_data_HDW        <-read.csv("Outputs/1_Field_spec/1_Processing/Headwall_data/alaskaSpecLib_data_HDW.csv"        )

##we'll need to apply a function to all dataframes that omits unwanted metadata
alaskaSpeclib_HDW_005nm       [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpeclib_HDW_010nm       [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpeclib_HDW_050nm       [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpeclib_HDW_100nm       [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpeclib_HDW_VIs         [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpecLib_data_HDWALLbands[c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL
alaskaSpecLib_data_HDW        [c("ScanID","PFT","PFT_2","PFT_4","area","Freq1","Freq2","Freq3")] = NULL

set.seed(2017)
##Lets run some different models so we can asses whiich ones are the best for prediction later
rf_005nm   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_005nm       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_005nm       )),ntree=1001,localImp = TRUE)
rf_010nm   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_010nm       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_010nm       )),ntree=1001,localImp = TRUE) 
rf_050nm   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_050nm       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_050nm       )),ntree=1001,localImp = TRUE) 
rf_100nm   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_100nm       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_100nm       )),ntree=1001,localImp = TRUE)
rf_VIs     <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs         ,mtry=sqrt(ncol(alaskaSpeclib_HDW_VIs         )),ntree=1001,localImp = TRUE)
rf_bands   <-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDWALLbands,mtry=sqrt(ncol(alaskaSpecLib_data_HDWALLbands)),ntree=1001,localImp = TRUE)
rf_bandsVIs<-randomForest(PFT_3~.,data=alaskaSpecLib_data_HDW        ,mtry=sqrt(ncol(alaskaSpecLib_data_HDW        )),ntree=1001,localImp = TRUE)
  
##lets save all these models
save(rf_005nm   , file = "Outputs/1_Field_spec/2_Models/Headwall/rf_005nm   .rda")
save(rf_010nm   , file = "Outputs/1_Field_spec/2_Models/Headwall/rf_010nm   .rda")  
save(rf_050nm   , file = "Outputs/1_Field_spec/2_Models/Headwall/rf_050nm   .rda")
save(rf_100nm   , file = "Outputs/1_Field_spec/2_Models/Headwall/rf_100nm   .rda")
save(rf_VIs     , file = "Outputs/1_Field_spec/2_Models/Headwall/rf_VIs     .rda")
save(rf_bands   , file = "Outputs/1_Field_spec/2_Models/Headwall/rf_bands   .rda")
save(rf_bandsVIs, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_bandsVIs.rda")

###Let's take a look at the important variable to see if we could optimize our model
##First we want to take a look at the distribution of minimal depth 
min_depth_frame_rf_005nm   <- min_depth_distribution(rf_005nm   )
min_depth_frame_rf_010nm   <- min_depth_distribution(rf_010nm   )
min_depth_frame_rf_050nm   <- min_depth_distribution(rf_050nm   )
min_depth_frame_rf_100nm   <- min_depth_distribution(rf_100nm   )
min_depth_frame_rf_VIs     <- min_depth_distribution(rf_VIs     )
min_depth_frame_rf_bands   <- min_depth_distribution(rf_bands   )
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
alaskaSpeclib_HDW_VIs10<-alaskaSpeclib_HDW_VIs %>%dplyr::select(PFT_3,rf_VIs_plot10n     )
alaskaSpeclib_HDW_VIs25<-alaskaSpeclib_HDW_VIs %>%dplyr::select(PFT_3,rf_VIs_plot25n     )
alaskaSpeclib_HDW_VIs50<-alaskaSpeclib_HDW_VIs %>%dplyr::select(PFT_3,rf_VIs_plot50n     )
alaskaSpeclib_HDW10    <-alaskaSpecLib_data_HDW%>%dplyr::select(PFT_3,rf_bandsVIs_plot10n)
alaskaSpeclib_HDW25    <-alaskaSpecLib_data_HDW%>%dplyr::select(PFT_3,rf_bandsVIs_plot25n)
alaskaSpeclib_HDW50    <-alaskaSpecLib_data_HDW%>%dplyr::select(PFT_3,rf_bandsVIs_plot50n)

##Now we can rebuild our model
rf_VIs10   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs10       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_VIs10       )),ntree=1001,localImp = TRUE)
rf_VIs25   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs25       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_VIs25       )),ntree=1001,localImp = TRUE)
rf_VIs50   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW_VIs50       ,mtry=sqrt(ncol(alaskaSpeclib_HDW_VIs50       )),ntree=1001,localImp = TRUE)
rf_HDW10   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW10           ,mtry=sqrt(ncol(alaskaSpeclib_HDW10           )),ntree=1001,localImp = TRUE)
rf_HDW25   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW25           ,mtry=sqrt(ncol(alaskaSpeclib_HDW25           )),ntree=1001,localImp = TRUE)
rf_HDW50   <-randomForest(PFT_3~.,data=alaskaSpeclib_HDW50           ,mtry=sqrt(ncol(alaskaSpeclib_HDW50           )),ntree=1001,localImp = TRUE)

##lets save all these models
save(rf_VIs10, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_VIs10.rda")
save(rf_VIs25, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_VIs25.rda")  
save(rf_VIs50, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_VIs50.rda")
save(rf_HDW10, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_HDW10.rda")
save(rf_HDW25, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_HDW25.rda")
save(rf_HDW50, file = "Outputs/1_Field_spec/2_Models/Headwall/rf_HDW50.rda")

##Lets create a data frame that will combine the class.error of all the categories of each model
rf_005nm_ConfusionMatrixa   <-rf_005nm   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_05nm    =class.error )
rf_010nm_ConfusionMatrixa   <-rf_010nm   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_010nm   =class.error )
rf_050nm_ConfusionMatrixa   <-rf_050nm   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_050nm   =class.error )
rf_100nm_ConfusionMatrixa   <-rf_100nm   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_100nm   =class.error )
rf_VIs_ConfusionMatrixa     <-rf_VIs     $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs     =class.error )
rf_bands_ConfusionMatrixa   <-rf_bands   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_bands   =class.error )
rf_bandsVIs_ConfusionMatrixa<-rf_bandsVIs$confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_bandsVIs=class.error )
rf_VIs10_ConfusionMatrixa   <-rf_VIs10   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs10   =class.error )
rf_VIs25_ConfusionMatrixa   <-rf_VIs25   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs25   =class.error )
rf_VIs50_ConfusionMatrixa   <-rf_VIs50   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_VIs50   =class.error )
rf_HDW10_ConfusionMatrixa   <-rf_HDW10   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_HDW10   =class.error )
rf_HDW25_ConfusionMatrixa   <-rf_HDW25   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_HDW25   =class.error )
rf_HDW50_ConfusionMatrixa   <-rf_HDW50   $confusion%>%as.data.frame()%>%dplyr::select(class.error)%>%rename(rf_HDW50   =class.error )

##Lets combine these dataframes so we can compare them later
rf_ConfusionMatrix_all<-cbind( rf_005nm_ConfusionMatrixa   
                              ,rf_010nm_ConfusionMatrixa   
                              ,rf_050nm_ConfusionMatrixa   
                              ,rf_100nm_ConfusionMatrixa   
                              ,rf_VIs_ConfusionMatrixa     
                              ,rf_bands_ConfusionMatrixa   
                              ,rf_bandsVIs_ConfusionMatrixa
                              ,rf_VIs10_ConfusionMatrixa   
                              ,rf_VIs25_ConfusionMatrixa   
                              ,rf_VIs50_ConfusionMatrixa   
                              ,rf_HDW10_ConfusionMatrixa   
                              ,rf_HDW25_ConfusionMatrixa   
                              ,rf_HDW50_ConfusionMatrixa)
write.csv(rf_ConfusionMatrix_all,"Outputs/1_Field_spec/2_Models/Headwall/rf_ConfusionMatrix_all.csv")
