source("./Functions/lecospectR.R")

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
# [2,] "BirchLake"
# [3,] "BisonGulch"
# [4,] "Chatnika"
# [5,] "ClaytonLake"
# [6,] "EagleSummit"
# [7,] "EightMile"
# [8,] "LittleLake"
# [9,] "MurphyDome"
#[10,] "TwelveMile"
#[11,] "VaultDrive"
#[12,] "WickershamDome"
#[13,] "Yukon"


#List directories of raw imagary that need to be reprocessed
AK2018_dir<-"M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/"
AK2019_dir<-"M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/"

#List directories for each year's flights
AK2018_dir_dates<-list.dirs(AK2018_dir, recursive = F)
# [1] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72418"
"M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518"
# [3] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72618"
# [4] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72718"
"M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818"
"M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918"
# [7] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73018"
# [8] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/73118"
# [9] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/81018"
#[10] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8118"
#[11] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8218"
#[12] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8418"
#[13] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8518"
#[14] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8618"
#[15] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8718"
#[16] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8818"
#[17] "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/8918"

    #Select usable dates from 2018


AK2019_dir_sites<-list.dirs(AK2019_dir, recursive = F)
#[1] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile"
#[2] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Big_Trail_Lake"
#[3] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch"
#[4] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bonanza_Creek"
#[5] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Eagle_Summit"
#[6] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Healy"
#[7] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Murphy_dome"
#[8] "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Wickersham_dome"

    #Select usable sites from 2019. NOTE: No data in Healy folder   
    AK2019_dir_sites_sub<-lapply(1:length(AK2019_dir_sites), function(x){list.dirs(AK2019_dir_sites[x], recursive = F)})

#Directories selected for use for manuscript
Bison_dir_raw ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/Bison_Gulch/Imagery_60m/100251_Bison_Gulch_line2_2019_08_12_01_07_28/"
Bonanza_dir_raw ="M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72518/ImagingSpectrometer/DataFiles/100066_2018_07_25_21_18_45/"
EightMile_dir_raw = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72818/ImagingSpectrometer/DataFolders/100124_BlacktandardFlight2_2018_07_28_22_56_17/"
Chatanika_dir_raw = "M:/Alaska_DATA/Alaska_Summer2018/Workspaces/Alaska/DatabyDate/72918/ImagingSpectrometer/DataFolders/100130_ChatanikaFlight3_attempt2_2018_07_29_20_32_59/"
#TwelveMile_path = "M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/"
#TwelveMile_path2 ="M:/Alaska_DATA/Alaska_Summer2019/Data_by_site/12mile/Imagery/100241_12mile_line3_2019_08_09_21_28_52/"

list.files(Bison_dir_raw)
