###################################Headwall_FLIGHT_LINES_Model####################################
library(randomForest)
library(rgdal)
library(raster)
library(tidyverse)
library(hsdar)

##Reads in Imagery as multi-layer raster...a RasterBrick (214 rows,238 cols, 50932 cells)
Clayton_Headwall_Tiff_Spatial<-brick("Imagery/headwallclipnew")
###Take a look at the first layer in this raster to get an idea of what it looks like
##plot(Clayton_Headwall_Tiff_Spatial[[1]])

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Processed_imagery/Headwall/Bandpasses/Headwall_WVLs", numeric())

##Convert hyperspectral image into a dataframe....dataframe with 397510 rows and 328 columns  
Clayton_Headwall_Tiff_df<-rasterToPoints(Clayton_Headwall_Tiff_Spatial)%>%as.data.frame()

##change column names of the df above to be numeric
colnames(Clayton_Headwall_Tiff_df)[-1:-2]<-HDW_ng_wv

##Do PCA calculation
PCA_preds_HDW_image <- prcomp(Clayton_Headwall_Tiff_df[-1:-2], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants <- round(PCA_preds_HDW_image$sdev^2/sum(PCA_preds_HDW_image$sdev^2)*100)

####fist 6 PC's
pc6<-c(1:6)

x<-as.data.frame(Clayton_Headwall_Tiff_df[1:2])
PCA_preds_HDW_image<-as.data.frame(PCA_preds_HDW_image $x[,pc6])

#creates dataframe with first 6 PCA vales for each species
PCA_preds_HDW_image <-cbind(x,PCA_preds_HDW_image)

###reads in spectral library dataframes based on PCA
PCA_preds_HDW              <-read.csv("Processed_spec/Imagery/Raw/PCA_preds_HDW.csv"              )
PCA_preds_HDW_equal50_PFT3 <-read.csv("Processed_spec/Imagery/Raw/PCA_preds_HDW_equal50_PFT3 .csv")
PCA_preds_HDW_equal70_PFT3 <-read.csv("Processed_spec/Imagery/Raw/PCA_preds_HDW_equal70_PFT3 .csv")
PCA_preds_HDW_equal90_PFT3 <-read.csv("Processed_spec/Imagery/Raw/PCA_preds_HDW_equal90_PFT3 .csv")
PCA_preds_HDW_equal110_PFT3<-read.csv("Processed_spec/Imagery/Raw/PCA_preds_HDW_equal110_PFT3.csv")
PCA_preds_HDW_equal200_PFT3<-read.csv("Processed_spec/Imagery/Raw/PCA_preds_HDW_equal200_PFT3.csv")

##For now I will use the whole dataset to generate models
rfPCA_HDW_all                  <-randomForest(PFT_3~.,data=PCA_preds_HDW               ,mtry=5,ntree=2001,importance=TRUE)
rfPCA_HDW_equal50_PFT3         <-randomForest(PFT_3~.,data=PCA_preds_HDW_equal50_PFT3  ,mtry=5,ntree=2001,importance=TRUE)
rfPCA_HDW_equal70_PFT3         <-randomForest(PFT_3~.,data=PCA_preds_HDW_equal70_PFT3  ,mtry=5,ntree=2001,importance=TRUE)
rfPCA_HDW_equal90_PFT3         <-randomForest(PFT_3~.,data=PCA_preds_HDW_equal90_PFT3  ,mtry=5,ntree=2001,importance=TRUE)
rfPCA_HDW_equal110_PFT3        <-randomForest(PFT_3~.,data=PCA_preds_HDW_equal110_PFT3 ,mtry=5,ntree=2001,importance=TRUE)
rfPCA_HDW_equal200_PFT3        <-randomForest(PFT_3~.,data=PCA_preds_HDW_equal200_PFT3 ,mtry=5,ntree=2001,importance=TRUE)


#uses model from spectral library to predict images
Results_HDW_equal05_PFT3        <-predict(rfPCA_HDW_equal200_PFT3         ,PCA_preds_HDW_image[-(1:2)])
Results_HDW_equal10_PFT3        <-predict(rfPCA_HDW_equal70_PFT3         ,PCA_preds_HDW_image[-(1:2)])
Results_HDW_equal15_PFT3        <-predict(rfPCA_HDW_equal90_PFT3         ,PCA_preds_HDW_image[-(1:2)])
Results_HDW_equal20_PFT3        <-predict(rfPCA_HDW_equal110_PFT3        ,PCA_preds_HDW_image[-(1:2)])

##converts prediction from rf model to dataframe
Results_HDW             <-as.data.frame(Results_HDW             )
Results_HDW_equal05_PFT3<-as.data.frame(Results_HDW_equal05_PFT3)
Results_HDW_equal10_PFT3<-as.data.frame(Results_HDW_equal10_PFT3)
Results_HDW_equal15_PFT3<-as.data.frame(Results_HDW_equal15_PFT3)
Results_HDW_equal20_PFT3<-as.data.frame(Results_HDW_equal20_PFT3)

##Changes first column name to predicted
names(Results_HDW             )[1]<-"predicted"
names(Results_HDW_equal05_PFT3)[1]<-"predicted"
names(Results_HDW_equal10_PFT3)[1]<-"predicted"
names(Results_HDW_equal15_PFT3)[1]<-"predicted"
names(Results_HDW_equal20_PFT3)[1]<-"predicted"

## Grabs x, y values from original image and combines with unique values from prediction 
Results_HDW_dat             <-cbind(Results_HDW             ,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)
Results_HDW_dat_equal05_PFT3<-cbind(Results_HDW_equal05_PFT3,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)
Results_HDW_dat_equal10_PFT3<-cbind(Results_HDW_equal10_PFT3,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)
Results_HDW_dat_equal15_PFT3<-cbind(Results_HDW_equal15_PFT3,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)
Results_HDW_dat_equal20_PFT3<-cbind(Results_HDW_equal20_PFT3,Clayton_Headwall_Tiff_df) %>% dplyr::select(predicted,x,y)


###Creates Unique PFT_IDs
Unique_PFT_HDW             <-unique(as.data.frame(Results_HDW_dat             $predicted))
Unique_PFT_HDW_equal05_PFT3<-unique(as.data.frame(Results_HDW_dat_equal05_PFT3$predicted))
Unique_PFT_HDW_equal10_PFT3<-unique(as.data.frame(Results_HDW_dat_equal10_PFT3$predicted))
Unique_PFT_HDW_equal15_PFT3<-unique(as.data.frame(Results_HDW_dat_equal15_PFT3$predicted))
Unique_PFT_HDW_equal20_PFT3<-unique(as.data.frame(Results_HDW_dat_equal20_PFT3$predicted))


Unique_PFT_HDW$PFT_ID             <-seq(1:nrow(Unique_PFT_HDW             ))
Unique_PFT_HDW_equal05_PFT3$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_equal05_PFT3))
Unique_PFT_HDW_equal10_PFT3$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_equal10_PFT3))
Unique_PFT_HDW_equal15_PFT3$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_equal15_PFT3))
Unique_PFT_HDW_equal20_PFT3$PFT_ID<-seq(1:nrow(Unique_PFT_HDW_equal20_PFT3))

names(Unique_PFT_HDW             )[1]<-"predicted"
names(Unique_PFT_HDW_equal05_PFT3)[1]<-"predicted"
names(Unique_PFT_HDW_equal10_PFT3)[1]<-"predicted"
names(Unique_PFT_HDW_equal15_PFT3)[1]<-"predicted"
names(Unique_PFT_HDW_equal20_PFT3)[1]<-"predicted"

###Create dataframe with unique PFT_ID values and location info
Results_HDW_PFT             <-merge(Results_HDW_dat             ,Unique_PFT_HDW             , by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_PFT_equal05_PFT3<-merge(Results_HDW_dat_equal05_PFT3,Unique_PFT_HDW_equal05_PFT3, by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_PFT_equal10_PFT3<-merge(Results_HDW_dat_equal10_PFT3,Unique_PFT_HDW_equal10_PFT3, by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_PFT_equal15_PFT3<-merge(Results_HDW_dat_equal15_PFT3,Unique_PFT_HDW_equal15_PFT3, by="predicted")%>% dplyr::select(x,y,PFT_ID)
Results_HDW_PFT_equal20_PFT3<-merge(Results_HDW_dat_equal20_PFT3,Unique_PFT_HDW_equal20_PFT3, by="predicted")%>% dplyr::select(x,y,PFT_ID)

###This is another option however this combines all the layers into one dataframe to creat a rasterbrick
#Results_HDW_brick<-merge(Results_HDW_PFT,Clayton_Headwall_Tiff_df)


##Converts dataframe to a raster for predicted layer
Results_HDW_raster<-rasterFromXYZ(Results_HDW_PFT_equal05_PFT3, crs = crs(Clayton_Headwall_Tiff_Spatial))

##Creates a plot showing the output of the predicted ratser layer
par(xpd = FALSE)
plot(
  Results_HDW_raster,
  legend = FALSE,
  col = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  xaxt = 'n',
  yaxt = 'n'
)

par(xpd = TRUE)
legend(
  "right",
  legend = c("Dwarf Shrub","Shrub","moss","Lichen","Tree","Forb","Graminoid"),
  fill = c(
    "lightcyan3",
    "royalblue",
    "forestgreen",
    "coral3",
    "papayawhip",
    "yellow",
    "orange"
  ),
  cex=0.75,
  xjust =1,
  horiz = FALSE,
  inset = -0.1,
  par(cex=0.3)
)
