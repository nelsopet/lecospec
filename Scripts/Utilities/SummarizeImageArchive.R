source("./Functions/lecospectR.R")
require(raster)
require(spectrolab)
require(tidyverse)
require(hsdar)
require(sf)
require(mapview)
require(caTools)
require(terra)
require(tools)
require(OpenImageR)
require(tiff)

#List files in original submission
DAAC_old<-list.dirs("F:/ORNL_DAAC_DATA_ARCHIVE", recursive = F)
DAAC_old<-gsub("^SpectraByLocation","",DAAC_old)

Imgs<-lapply(1:length(DAAC_old), function(x) {
imgs<-list.files(DAAC_old[x])
hdr<-imgs[grepl("*hdr",imgs)]
return(hdr)    
}) %>% unlist()
Imgs_drop<-Imgs[grepl("^raw", Imgs)] #%>% t
#Imgs[Imgs=!Imgs_drop]
Imgs_df<-strsplit(Imgs, "_")
as.data.frame(Imgs_df)
lapply(1:length(Imgs_df), function(x) {(num<-unlist(Imgs_df[x]) %>% length)/11})
Imgs_df<-Imgs_df[-25:-26]
#(unlist(Imgs_df) %>% length)/10
Imgs_df<-do.call(rbind, Imgs_df) %>% as.data.frame()
Img_df_names<-c("Site","Year","Month","Day", "ID1","ID2","ID3", "DatacubeID","Radiance","Reflectance","Ortho_Extension")
colnames(Imgs_df)<-Img_df_names


#> unique(Imgs_df$Site) %>% as.matrix()
#      [,1]
# [1,] "BigTrailLake"
# [2,] "BirchLake" #7/30/2018
# [3,] "BisonGulch"
# [4,] "Chatnika" #100130
# [5,] "ClaytonLake"
# [6,] "EagleSummit"
# [7,] "EightMile"
# [8,] "LittleLake" #7/30/2018
# [9,] "MurphyDome" #7/31/2018
#[10,] "TwelveMile" 
#[11,] "VaultDrive" #7/29/2018
#[12,] "WickershamDome"
#[13,] "Yukon"


#List directories of raw imagary that need to be reprocessed
AK2018_dir<-"M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/"
AK2019_dir<-"M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/"

#List directories for each year's flights
AK2018_dir_dates<-list.dirs(AK2018_dir, recursive = F) 
# [1] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72418"
    path_72518 = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518"
    dirs_72518 = list.dirs(path_72518)
    dirs_72518[grep("100066",dirs_72518)][1]

# [3] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72618"
# [4] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72718"
dirs_72818 =list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818")
    dirs_72818[grep("100124",dirs_72818)][1]

dirs_72918 =list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918")
    dirs_72918[grep("100130",dirs_72918)][1]

dirs_73018 =list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73018")
    dirs_73018[grepl("100148",dirs_73018)]
    dirs_73018[grep("100152",dirs_73018)]
    dirs_73018[grep("100153",dirs_73018)]

#dirs_73118 =list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118")
#    dirs_73118[grepl("100158",dirs_73118)][1]

dirs_81018 = list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/81018")
    dirs_81018[grepl("100218",dirs_81018)][1]

dirs_80118=list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8118")
    dirs_80118[grepl("100158",dirs_80118)]
    dirs_80118[grepl("100164",dirs_80118)]
    dirs_80118[grepl("100168",dirs_80118)]

dirs_80218=list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8218")
    dirs_80218[grepl("100179",dirs_80218)]

dirs_80418=list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8418")
    dirs_80418[grep("100187",dirs_80418)]

dirs_80618=list.dirs("M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8618")
    dirs_80618[grepl("100199",dirs_80618)]

#[15] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8718"
#[16] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8818"
#[17] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8918"

    #Select usable dates from 2018
dirs_keep= c(dirs_72518[grep("100066",dirs_72518)][1]
,dirs_72818[grep("100124",dirs_72818)][1]
,dirs_72918[grep("100130",dirs_72918)][1]
,dirs_73018[grepl("100148",dirs_73018)]
,dirs_73018[grep("100152",dirs_73018)] 
,dirs_73018[grep("100153",dirs_73018)]
,dirs_81018[grepl("100218",dirs_81018)][1]
,dirs_80118[grepl("100158",dirs_80118)] 
,dirs_80118[grepl("100164",dirs_80118)]
,dirs_80118[grepl("100168",dirs_80118)]
,dirs_80218[grepl("100179",dirs_80218)]
,dirs_80418[grep("100187",dirs_80418)]
, dirs_80618[grepl("100199",dirs_80618)])

lapply(1:length(dirs_keep), function(x) {
#x = 9
path_parts<-str_split(dirs_keep[x], pattern = "/")
path_parts<-str_split(dirs_keep[x], pattern = "/")
flight_name = path_parts[[1]][length(path_parts[[1]])]

#Make a list off all the files in a target directory
files_all = list.files(dirs_keep[x])
#Make a list of the files that are binary data
imgs = files_all[grepl("rd_rf_or$", files_all)]
#Make a list of the files that are header files that go with each binary data file
hdrs = files_all[grepl("rd_rf_or.hdr$", files_all)]
#Make a complete list of files to keep and copy to the submisssion directory
files_keep = c(imgs,hdrs)
    #Make a quick look for each image
    #y=3 
    lapply(1:length(imgs), function(y){
    print(paste("M:/Alaska_DATA/ORNL_DAAC_SUBMISSION_V2/",flight_name,"_",imgs[y],"_RGB_quicklook.jpg",sep=""))
    tst<-terra::rast(paste(dirs_keep[x],"/",imgs[y], sep=""))
    jpeg(paste("M:/Alaska_DATA/ORNL_DAAC_SUBMISSION_V2/",flight_name,"_",imgs[y],"_RGB_quicklook.jpg",sep=""))
    terra::plotRGB(tst, r=160,g=80,b=25, stretch = "lin")
    dev.off()
    })
        #Copy each file to the submission directory
        lapply(1:length(files_keep), function(z) {
        #z=4
        Sys.time()
        file.copy(from=paste(dirs_keep[x],"/",files_keep[z], sep=""),to=paste("M:/Alaska_DATA/ORNL_DAAC_SUBMISSION_V2/",flight_name,"_",files_keep[z],sep=""), overwrite = TRUE)
        Sys.time()
        })
})

AK2019_dir_sites<-list.dirs(AK2019_dir, recursive = F)
#[1] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile"
dirs_12mile=list.dirs("M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile")
    dirs_12mile[grepl("100241",dirs_12mile)][1]
#[2] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Big_Trail_Lake"
dirs_BigTrailLake=list.dirs("M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Big_Trail_Lake")
    dirs_BigTrailLake[grepl("100199",dirs_BigTrailLake)]
    dirs_BigTrailLake[grepl("100200",dirs_BigTrailLake)]
    dirs_BigTrailLake[grepl("100201",dirs_BigTrailLake)]
    dirs_BigTrailLake[grepl("100211",dirs_BigTrailLake)]
#[3] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch"
dirs_Bison=list.dirs("M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch")
    dirs_Bison[grepl("100251",dirs_Bison)][1]
    dirs_Bison[grepl("100253",dirs_Bison)][1]
#[4] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bonanza_Creek"
#[5] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Eagle_Summit"
dirs_EagleSummit=list.dirs("M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Eagle_Summit")
    dirs_EagleSummit[grepl("100167",dirs_EagleSummit)]
    dirs_EagleSummit[grepl("100168",dirs_EagleSummit)]
#[6] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Healy"
#[7] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Murphy_dome"
#[8] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Wickersham_dome"
dirs_Wickersham=list.dirs("M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Wickersham_dome")
    dirs_Wickersham[grepl("100227",dirs_Wickersham)][1]
    dirs_Wickersham[grepl("100229",dirs_Wickersham)][1]
    dirs_Wickersham[grepl("100230",dirs_Wickersham)][1]
    dirs_Wickersham[grepl("100231",dirs_Wickersham)][1]

#Directories from 2019 to keep
dirs_keep_2019<-c( 
    dirs_12mile[grepl("100241",dirs_12mile)][1]
    ,dirs_BigTrailLake[grepl("100199",dirs_BigTrailLake)]
    ,dirs_BigTrailLake[grepl("100200",dirs_BigTrailLake)]
    ,dirs_BigTrailLake[grepl("100201",dirs_BigTrailLake)]
    ,dirs_BigTrailLake[grepl("100211",dirs_BigTrailLake)]
    ,dirs_Bison[grepl("100251",dirs_Bison)][1]
    ,dirs_Bison[grepl("100253",dirs_Bison)][1]
    ,dirs_EagleSummit[grepl("100167",dirs_EagleSummit)]
    ,dirs_EagleSummit[grepl("100168",dirs_EagleSummit)]
    ,dirs_Wickersham[grepl("100227",dirs_Wickersham)][1]
    ,dirs_Wickersham[grepl("100229",dirs_Wickersham)][1]
    ,dirs_Wickersham[grepl("100230",dirs_Wickersham)][1]
    ,dirs_Wickersham[grepl("100231",dirs_Wickersham)][1]
)

lapply(1:length(dirs_keep_2019), function(x) {
#x = 9
path_parts<-str_split(dirs_keep_2019[x], pattern = "/")
path_parts<-str_split(dirs_keep_2019[x], pattern = "/")
flight_name = path_parts[[1]][length(path_parts[[1]])]

#Make a list off all the files in a target directory
files_all = list.files(dirs_keep_2019[x])
#Make a list of the files that are binary data
imgs = files_all[grepl("rd_rf_or$", files_all)]
#Make a list of the files that are header files that go with each binary data file
hdrs = files_all[grepl("rd_rf_or.hdr$", files_all)]
#Make a complete list of files to keep and copy to the submisssion directory
files_keep = c(imgs,hdrs)
    #Make a quick look for each image
    #y=3 
    lapply(1:length(imgs), function(y){
    print(paste("M:/Alaska_DATA/ORNL_DAAC_SUBMISSION_V2/",flight_name,"_",imgs[y],"_RGB_quicklook.jpg",sep=""))
    tst<-terra::rast(paste(dirs_keep_2019[x],"/",imgs[y], sep=""))
    jpeg(paste("M:/Alaska_DATA/ORNL_DAAC_SUBMISSION_V2/",flight_name,"_",imgs[y],"_RGB_quicklook.jpg",sep=""))
    terra::plotRGB(tst, r=160,g=80,b=25, stretch = "lin")
    dev.off()
    })
        #Copy each file to the submission directory
        #lapply(1:length(files_keep), function(z) {
        ##z=4
        #Sys.time()
        #file.copy(from=paste(dirs_keep_2019[x],"/",files_keep[z], sep=""),to=paste("M:/Alaska_DATA/ORNL_DAAC_SUBMISSION_V2/",flight_name,"_",files_keep[z],sep=""), overwrite = TRUE)
        #Sys.time()
        #})
})



#Directories selected for use for manuscript
Bison_dir_raw ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/"
Bonanza_dir_raw ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/"
EightMile_dir_raw = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/"
Chatanika_dir_raw = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/"
#TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/"
#TwelveMile_path2 ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/"
