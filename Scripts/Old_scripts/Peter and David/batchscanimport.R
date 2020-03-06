##set directory

setwd("C:/Users/Eclipse/Desktop/ssagqs/")

##list files in directory and save strings as object

temp<-c(paste(list.files()))

##read and name the txt files
ssagt1_0001<-read.table("ssagt1_00001.txt", header = FALSE, sep="", skip=27)
colnames(ssagt1_0001)<-c("Band","Refl")

temp %>% map_df(~read.table(.x, col_types = cols(.default = "c", header = FALSE, sep="", skip=27))) 

##cleave header and name as new object

ssagq_00001<-read.table("ssagt1_00001.txt", header = FALSE, sep="", skip=27)
ssagq_00002<-read.table("ssagt1_00002.txt", header = FALSE, sep="", skip=27)
ssagq_00003<-read.table("ssagt1_00003.txt", header = FALSE, sep="", skip=27)
ssagq_00004<-read.table("ssagt1_00004.txt", header = FALSE, sep="", skip=27)
ssagq_00005<-read.table("ssagt1_00005.txt", header = FALSE, sep="", skip=27)
ssagq_00006<-read.table("ssagt1_00006.txt", header = FALSE, sep="", skip=27)
ssagq_00007<-read.table("ssagt1_00007.txt", header = FALSE, sep="", skip=27)
ssagq_00008<-read.table("ssagt1_00008.txt", header = FALSE, sep="", skip=27)
ssagq_00009<-read.table("ssagt1_00009.txt", header = FALSE, sep="", skip=27)
ssagq_00010<-read.table("ssagt1_00010.txt", header = FALSE, sep="", skip=27)
ssagq_00011<-read.table("ssagt1_00011.txt", header = FALSE, sep="", skip=27)
ssagq_00012<-read.table("ssagt1_00012.txt", header = FALSE, sep="", skip=27)
ssagq_00013<-read.table("ssagt1_00013.txt", header = FALSE, sep="", skip=27)
ssagq_00014<-read.table("ssagt1_00014.txt", header = FALSE, sep="", skip=27)
ssagq_00015<-read.table("ssagt1_00015.txt", header = FALSE, sep="", skip=27)
ssagq_00016<-read.table("ssagt1_00016.txt", header = FALSE, sep="", skip=27)
ssagq_00017<-read.table("ssagt1_00017.txt", header = FALSE, sep="", skip=27)
ssagq_00018<-read.table("ssagt1_00018.txt", header = FALSE, sep="", skip=27)
ssagq_00019<-read.table("ssagt1_00019.txt", header = FALSE, sep="", skip=27)
ssagq_00020<-read.table("ssagt1_00020.txt", header = FALSE, sep="", skip=27)
ssagq_00021<-read.table("ssagt1_00021.txt", header = FALSE, sep="", skip=27)
ssagq_00022<-read.table("ssagt1_00022.txt", header = FALSE, sep="", skip=27)
ssagq_00023<-read.table("ssagt1_00023.txt", header = FALSE, sep="", skip=27)
ssagq_00024<-read.table("ssagt1_00024.txt", header = FALSE, sep="", skip=27)
ssagq_00025<-read.table("ssagt1_00025.txt", header = FALSE, sep="", skip=27)
ssagq_00026<-read.table("ssagt1_00026.txt", header = FALSE, sep="", skip=27)
ssagq_00027<-read.table("ssagt1_00027.txt", header = FALSE, sep="", skip=27)
ssagq_00028<-read.table("ssagt1_00028.txt", header = FALSE, sep="", skip=27)
ssagq_00029<-read.table("ssagt1_00029.txt", header = FALSE, sep="", skip=27)
ssagq_00030<-read.table("ssagt1_00030.txt", header = FALSE, sep="", skip=27)
ssagq_00031<-read.table("ssagt1_00031.txt", header = FALSE, sep="", skip=27)
ssagq_00032<-read.table("ssagt1_00032.txt", header = FALSE, sep="", skip=27)
ssagq_00033<-read.table("ssagt1_00033.txt", header = FALSE, sep="", skip=27)
ssagq_00034<-read.table("ssagt1_00034.txt", header = FALSE, sep="", skip=27)
ssagq_00035<-read.table("ssagt1_00035.txt", header = FALSE, sep="", skip=27)
ssagq_00036<-read.table("ssagt1_00036.txt", header = FALSE, sep="", skip=27)
ssagq_00037<-read.table("ssagt1_00037.txt", header = FALSE, sep="", skip=27)
ssagq_00038<-read.table("ssagt1_00038.txt", header = FALSE, sep="", skip=27)
ssagq_00039<-read.table("ssagt1_00039.txt", header = FALSE, sep="", skip=27)
ssagq_00040<-read.table("ssagt1_00040.txt", header = FALSE, sep="", skip=27)

ssagq_00001$quad = "1"
ssagq_00002$quad = "1"
ssagq_00003$quad = "1"
ssagq_00004$quad = "1"
ssagq_00005$quad = "2"
ssagq_00006$quad = "2"
ssagq_00007$quad = "2"
ssagq_00008$quad = "2"
ssagq_00009$quad = "3"
ssagq_00010$quad = "3"
ssagq_00011$quad = "3"
ssagq_00012$quad = "3"
ssagq_00013$quad = "4"
ssagq_00014$quad = "4"
ssagq_00015$quad = "4"
ssagq_00016$quad = "4"
ssagq_00017$quad = "5"
ssagq_00018$quad = "5"
ssagq_00019$quad = "5"
ssagq_00020$quad = "5"
ssagq_00021$quad = "6"
ssagq_00022$quad = "6"
ssagq_00023$quad = "6"
ssagq_00024$quad = "6"
ssagq_00025$quad = "7"
ssagq_00026$quad = "7"
ssagq_00027$quad = "7"
ssagq_00028$quad = "7"
ssagq_00029$quad = "8"
ssagq_00030$quad = "8"
ssagq_00031$quad = "8"
ssagq_00032$quad = "8"
ssagq_00033$quad = "9"
ssagq_00034$quad = "9"
ssagq_00035$quad = "9"
ssagq_00036$quad = "9"
ssagq_00037$quad = "10"
ssagq_00038$quad = "10"
ssagq_00039$quad = "10"
ssagq_00040$quad = "10"
##do the above for quad size in the event we used the .25m quadrats with single scans. ALSO, for species name

aultur_00001$species = "aultur"
aultur_00002$species = "aultur"
aultur_00003$species = "aultur"
aultur_00004$species = "aultur"
betann_00001$species = "betann"
betann_00002$species = "betann"
betann_00003$species = "betann"
betann_00004$species = "betann"
clasty_00001$species = "clasty"
clasty_00002$species = "clasty"
clasty_00003$species = "clasty"
clasty_00004$species = "clasty"
erivag_00001$species = "erivag"
erivag_00002$species = "erivag"
erivag_00003$species = "erivag"
erivag_00004$species = "erivag"
erivag_00005$species = "erivag"
erivag_00006$species = "erivag"
erivag_00007$species = "erivag"
erivag_00008$species = "erivag"
erivag_00009$species = "erivag"
erivag_00010$species = "erivag"
flacuc_00001$species = "flacuc"
flacuc_00002$species = "flacuc"
flacuc_00003$species = "flacuc"
flacuc_00004$species = "flacuc"
hypspl_00001$species = "hypspl"
hypspl_00002$species = "hypspl"
hypspl_00003$species = "hypspl"
hypspl_00004$species = "hypspl"
leddec_00001$species = "leddec"
leddec_00002$species = "leddec"
leddec_00003$species = "leddec"
leddec_00004$species = "leddec"
rubcha_00001$species = "rubcha"
rubcha_00002$species = "rubcha"
rubcha_00003$species = "rubcha"
rubcha_00004$species = "rubcha"
vacvit_00001$species = "vacvit"
vacvit_00002$species = "vacvit"
vacvit_00003$species = "vacvit"
vacvit_00004$species = "vacvit"
vacvit_00005$species = "vacvit" 

##Bind all the scans together into a collection

ssaglib<-rbind(
aultur_00001
,aultur_00002
,aultur_00003
,aultur_00004
,betann_00001
,betann_00002
,betann_00003
,betann_00004
,clasty_00001
,clasty_00002
,clasty_00003
,clasty_00004
,erivag_00001
,erivag_00002
,erivag_00003
,erivag_00004
,erivag_00005
,erivag_00006
,erivag_00007
,erivag_00008
,erivag_00009
,erivag_00010
,flacuc_00001
,flacuc_00002
,flacuc_00003
,flacuc_00004
,hypspl_00001
,hypspl_00002
,hypspl_00003
,hypspl_00004
,leddec_00001
,leddec_00002
,leddec_00003
,leddec_00004
,rubcha_00001
,rubcha_00002
,rubcha_00003
,rubcha_00004
,vacvit_00001
,vacvit_00002
,vacvit_00003
,vacvit_00004
,vacvit_00005
);
colnames(ssagq)<-c("Band","Refl","Quad")

##to check column names
colnames(ssagq) or colnames(ssagq_summ)

##Filter data such that there are no Refl values over 100
ssagq_filt<-filter(ssagq, Refl < 100)

##Make final matrix where each band has only one reflectance value per quad
## lost the quadrat column after this function, worked after re ran
ssagq_summ<-ssagq_filt %>% group_by(Quad, Band) %>% summarise(mean(Refl))

## add a column to indicate site
ssagq_summ$Site<-"Sagwon"

## plot data I get an error saying: Error in mean(Refl) : object 'Refl' not found rename column?
p<-ggplot(ssagq_summ,aes(Band, mean(Refl)))
p+geom_line(aes(color=Quad))
p

## rename column (This worked and graph exicuted)
colnames(data)[colnames(data)=="old_name"] <- "new_name"
colnames(ssaglib_summ)<-c("Species","Band","Refl")

##Found bug in Refl data after executed a summary(ssagq_summ). Refl had a max value of 122

write.csv(ssagq_summ, file="Sagwon_Quad_Scans.csv")

#######################################################ExtraCode
colnames(ssagl)<-c("Band","Refl","Site", "PFT")

ssagl_filt<-filter(ssagl, Refl < 100)

ssagl_summ<-ssagl_filt %>% group_by(Site, Band, PFT) %>% summarise(mean(Refl))

colnames(ssagl_summ)<-c("Site","Band","PFT","Refl")

p<-ggplot(ssagl_summ,aes(Band, Refl))

p+geom_line(aes(color=PFT))



write.csv(toolikq_summ, "toolik.csv")

jpeg("toolikq_summ.jpg")

p<-ggplot(toolikq_summ,aes(Band, Refl))

p+geom_line(aes(color=Quad))

dev.off();
########################################################

##SPECTRAL LIB: When using group function provided by dplyr, dont group both the species and the bands or you will average out the spectra. (maybe you dont have to use grouping?)
##Just skip straight to plotting filtered data to a ggplot

##change file name first, then object name, then site name

##NextSite

toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
toolikq_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
toolikq_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
toolikq_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
toolikq_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)


toolikq_00005<-read.table("toolikt1_00005.sed", header = FALSE, sep="", skip=27)
toolikq_00006<-read.table("toolikt1_00006.sed", header = FALSE, sep="", skip=27)
toolikq_00007<-read.table("toolikt1_00007.sed", header = FALSE, sep="", skip=27)
toolikq_00008<-read.table("toolikt1_00008.sed", header = FALSE, sep="", skip=27)
toolikq_00009<-read.table("toolikt1_00009.sed", header = FALSE, sep="", skip=27)
toolikq_00010<-read.table("toolikt1_00010.sed", header = FALSE, sep="", skip=27)
toolikq_00011<-read.table("toolikt1_00011.sed", header = FALSE, sep="", skip=27)
toolikq_00012<-read.table("toolikt1_00012.sed", header = FALSE, sep="", skip=27)
toolikq_00013<-read.table("toolikt1_00013.sed", header = FALSE, sep="", skip=27)
toolikq_00014<-read.table("toolikt1_00014.sed", header = FALSE, sep="", skip=27)
toolikq_00015<-read.table("toolikt1_00015.sed", header = FALSE, sep="", skip=27)
toolikq_00016<-read.table("toolikt1_00016.sed", header = FALSE, sep="", skip=27)
toolikq_00017<-read.table("toolikt1_00017.sed", header = FALSE, sep="", skip=27)
toolikq_00018<-read.table("toolikt1_00018.sed", header = FALSE, sep="", skip=27)
toolikq_00019<-read.table("toolikt1_00019.sed", header = FALSE, sep="", skip=27)
toolikq_00020<-read.table("toolikt1_00020.sed", header = FALSE, sep="", skip=27)
toolikq_00021<-read.table("toolikt1_00021.sed", header = FALSE, sep="", skip=27)
toolikq_00022<-read.table("toolikt1_00022.sed", header = FALSE, sep="", skip=27)
toolikq_00023<-read.table("toolikt1_00023.sed", header = FALSE, sep="", skip=27)
toolikq_00024<-read.table("toolikt1_00024.sed", header = FALSE, sep="", skip=27)
toolikq_00025<-read.table("toolikt1_00025.sed", header = FALSE, sep="", skip=27)
toolikq_00026<-read.table("toolikt1_00026.sed", header = FALSE, sep="", skip=27)
toolikq_00027<-read.table("toolikt1_00027.sed", header = FALSE, sep="", skip=27)
toolikq_00028<-read.table("toolikt1_00028.sed", header = FALSE, sep="", skip=27)
toolikq_00029<-read.table("toolikt1_00029.sed", header = FALSE, sep="", skip=27)
toolikq_00030<-read.table("toolikt1_00030.sed", header = FALSE, sep="", skip=27)
toolikq_00031<-read.table("toolikt1_00031.sed", header = FALSE, sep="", skip=27)
toolikq_00032<-read.table("toolikt1_00032.sed", header = FALSE, sep="", skip=27)
toolikq_00033<-read.table("toolikt1_00033.sed", header = FALSE, sep="", skip=27)
toolikq_00034<-read.table("toolikt1_00034.sed", header = FALSE, sep="", skip=27)
toolikq_00035<-read.table("toolikt1_00035.sed", header = FALSE, sep="", skip=27)
toolikq_00036<-read.table("toolikt1_00036.sed", header = FALSE, sep="", skip=27)
toolikq_00037<-read.table("toolikt1_00037.sed", header = FALSE, sep="", skip=27)
toolikq_00038<-read.table("toolikt1_00038.sed", header = FALSE, sep="", skip=27)
toolikq_00039<-read.table("toolikt1_00039.sed", header = FALSE, sep="", skip=27)
toolikq_00040<-read.table("toolikt1_00040.sed", header = FALSE, sep="", skip=27)

toolikq_00001$quad = "1"
toolikq_00002$quad = "1"
toolikq_00003$quad = "1"
toolikq_00004$quad = "1"
toolikq_00005$quad = "2"
toolikq_00006$quad = "2"
toolikq_00007$quad = "2"
toolikq_00008$quad = "2"
toolikq_00009$quad = "3"
toolikq_00010$quad = "3"
toolikq_00011$quad = "3"
toolikq_00012$quad = "3"
toolikq_00013$quad = "4"
toolikq_00014$quad = "4"
toolikq_00015$quad = "4"
toolikq_00016$quad = "4"
toolikq_00017$quad = "5"
toolikq_00018$quad = "5"
toolikq_00019$quad = "5"
toolikq_00020$quad = "5"
toolikq_00021$quad = "6"
toolikq_00022$quad = "6"
toolikq_00023$quad = "6"
toolikq_00024$quad = "6"
toolikq_00025$quad = "7"
toolikq_00026$quad = "7"
toolikq_00027$quad = "7"
toolikq_00028$quad = "7"
toolikq_00029$quad = "8"
toolikq_00030$quad = "8"
toolikq_00031$quad = "8"
toolikq_00032$quad = "8"
toolikq_00033$quad = "9"
toolikq_00034$quad = "9"
toolikq_00035$quad = "9"
toolikq_00036$quad = "9"
toolikq_00037$quad = "10"
toolikq_00038$quad = "10"
toolikq_00039$quad = "10"
toolikq_00040$quad = "10"

toolikq<-rbind(
toolikq_00001
,toolikq_00002
,toolikq_00003
,toolikq_00004
,toolikq_00005
,toolikq_00006
,toolikq_00007
,toolikq_00008
,toolikq_00009
,toolikq_00010
,toolikq_00011
,toolikq_00012
,toolikq_00013
,toolikq_00014
,toolikq_00015
,toolikq_00016
,toolikq_00017
,toolikq_00018
,toolikq_00019
,toolikq_00020
,toolikq_00021
,toolikq_00022
,toolikq_00023
,toolikq_00024
,toolikq_00025
,toolikq_00026
,toolikq_00027
,toolikq_00028
,toolikq_00029
,toolikq_00030
,toolikq_00031
,toolikq_00032
,toolikq_00033
,toolikq_00034
,toolikq_00035
,toolikq_00036
,toolikq_00037
,toolikq_00038
,toolikq_00039
,toolikq_00040
);

colnames(toolikq)<-c("Band","Refl","Quad")

toolikq_filt<-filter(toolikq, Refl < 100)

toolikq_summ<-toolikq_filt %>% group_by(Quad, Band) %>% summarise(mean(Refl))

toolikq_summ$Site<-"toolik"

colnames(toolikq_summ)<-c("Quad","Band","Refl","Site")



p<-ggplot(toolikq_summ,aes(Band, Refl))

p+geom_line(aes(color=Quad))

write.csv(toolikq_summ, "toolik.csv")

jpeg("toolikq_summ.jpg")

p<-ggplot(toolikq_summ,aes(Band, Refl))

p+geom_line(aes(color=Quad))

dev.off();

##

##SPECTRAL LIBRARY
##When doing spec Lib, to get all the unique names of the species in a dir quickly, use command prompt 'dir > desiredfilename.txt' command to print all files in directory out to a text file.

##bethelLib
arcnig_00001<-read.table("arcniglclip_00001.sed", header = FALSE, sep="", skip=27)
arcnig_00002<-read.table("arcniglclip_00002.sed", header = FALSE, sep="", skip=27)
arcnig_00003<-read.table("arcniglclip_00003.sed", header = FALSE, sep="", skip=27)
arcnig_00004<-read.table("arcniglclip_00004.sed", header = FALSE, sep="", skip=27)
arcsta_00001<-read.table("arcsta_00001.sed", header = FALSE, sep="", skip=27)
arcsta_00002<-read.table("arcsta_00002.sed", header = FALSE, sep="", skip=27)
arcsta_00003<-read.table("arcsta_00003.sed", header = FALSE, sep="", skip=27)
arcsta_00004<-read.table("arcsta_00004.sed", header = FALSE, sep="", skip=27)
betnan_00001<-read.table("betnanlclip_00001.sed", header = FALSE, sep="", skip=27)
betnan_00002<-read.table("betnanlclip_00002.sed", header = FALSE, sep="", skip=27)
betnan_00003<-read.table("betnanlclip_00003.sed", header = FALSE, sep="", skip=27)
betnan_00004<-read.table("betnanlclip_00004.sed", header = FALSE, sep="", skip=27)
claste_00001<-read.table("clastecprobe_00001.sed", header = FALSE, sep="", skip=27)
claste_00002<-read.table("clastecprobe_00002.sed", header = FALSE, sep="", skip=27)
claste_00003<-read.table("clastecprobe_00003.sed", header = FALSE, sep="", skip=27)
claste_00004<-read.table("clastecprobe_00004.sed", header = FALSE, sep="", skip=27)
clasty_00001<-read.table("clastycprobe_00001.sed", header = FALSE, sep="", skip=27)
clasty_00002<-read.table("clastycprobe_00002.sed", header = FALSE, sep="", skip=27)
clasty_00003<-read.table("clastycprobe_00003.sed", header = FALSE, sep="", skip=27)
clasty_00004<-read.table("clastycprobe_00004.sed", header = FALSE, sep="", skip=27)
empnig_00001<-read.table("empnig3_00001.sed", header = FALSE, sep="", skip=27)
empnig_00002<-read.table("empnig3_00002.sed", header = FALSE, sep="", skip=27)
empnig_00003<-read.table("empnig3_00003.sed", header = FALSE, sep="", skip=27)
empnig_00004<-read.table("empnig3_00004.sed", header = FALSE, sep="", skip=27)
empnig_00005<-read.table("empniglclip_00001.sed", header = FALSE, sep="", skip=27)
empnig_00006<-read.table("empniglclip_00002.sed", header = FALSE, sep="", skip=27)
empnig_00007<-read.table("empniglclip_00003.sed", header = FALSE, sep="", skip=27)
empnig_00008<-read.table("empniglclip_00004.sed", header = FALSE, sep="", skip=27)
icmeri_00002<-read.table("fairypuke_00001.sed", header = FALSE, sep="", skip=27)
icmeri_00003<-read.table("fairypuke_00002.sed", header = FALSE, sep="", skip=27)
icmeri_00004<-read.table("fairypuke_00003.sed", header = FALSE, sep="", skip=27)
icmeri_00005<-read.table("fairypuke_00004.sed", header = FALSE, sep="", skip=27)
icmeri_00006<-read.table("iceericprobe_00001.sed", header = FALSE, sep="", skip=27)
icmeri_00007<-read.table("iceericprobe_00002.sed", header = FALSE, sep="", skip=27)
icmeri_00008<-read.table("iceericprobe_00003.sed", header = FALSE, sep="", skip=27)
icmeri_00009<-read.table("iceericprobe_00004.sed", header = FALSE, sep="", skip=27)
leddec_00001<-read.table("leddec2lclip_00001.sed", header = FALSE, sep="", skip=27)
leddec_00002<-read.table("leddec2lclip_00002.sed", header = FALSE, sep="", skip=27)
leddec_00003<-read.table("leddec2lclip_00003.sed", header = FALSE, sep="", skip=27)
leddec_00004<-read.table("leddec2lclip_00004.sed", header = FALSE, sep="", skip=27)
rubcam_00001<-read.table("rubcam_00001.sed", header = FALSE, sep="", skip=27)
rubcam_00002<-read.table("rubcam_00002.sed", header = FALSE, sep="", skip=27)
rubcam_00003<-read.table("rubcam_00003.sed", header = FALSE, sep="", skip=27)
rubcam_00004<-read.table("rubcam_00004.sed", header = FALSE, sep="", skip=27)
rubcha_00001<-read.table("rubcha2lclip_00001.sed", header = FALSE, sep="", skip=27)
rubcha_00002<-read.table("rubcha2lclip_00002.sed", header = FALSE, sep="", skip=27)
rubcha_00003<-read.table("rubcha2lclip_00003.sed", header = FALSE, sep="", skip=27)
rubcha_00004<-read.table("rubcha2lclip_00004.sed", header = FALSE, sep="", skip=27)
sphfus_00001<-read.table("sphfuscprobe_00001.sed", header = FALSE, sep="", skip=27)
sphfus_00002<-read.table("sphfuscprobe_00002.sed", header = FALSE, sep="", skip=27)
sphfus_00003<-read.table("sphfuscprobe_00003.sed", header = FALSE, sep="", skip=27)
sphfus_00004<-read.table("sphfuscprobe_00004.sed", header = FALSE, sep="", skip=27)
vacvit_00001<-read.table("vacvit2_00001.sed", header = FALSE, sep="", skip=27)
vacvit_00002<-read.table("vacvit2_00002.sed", header = FALSE, sep="", skip=27)
vacvit_00003<-read.table("vacvit2_00003.sed", header = FALSE, sep="", skip=27)
vacvit_00004<-read.table("vacvit2_00004.sed", header = FALSE, sep="", skip=27)

arcnig_00001$PFT = "arcnig"
arcnig_00002$PFT = "arcnig"
arcnig_00003$PFT = "arcnig"
arcnig_00004$PFT = "arcnig"
arcsta_00001$PFT = "arcsta"
arcsta_00002$PFT = "arcsta"
arcsta_00003$PFT = "arcsta"
arcsta_00004$PFT = "arcsta"
betnan_00001$PFT = "betnan"
betnan_00002$PFT = "betnan"
betnan_00003$PFT = "betnan"
betnan_00004$PFT = "betnan"
claste_00001$PFT = "claste"
claste_00002$PFT = "claste"
claste_00003$PFT = "claste"
claste_00004$PFT = "claste"
clasty_00001$PFT = "clasty"
clasty_00002$PFT = "clasty"
clasty_00003$PFT = "clasty"
clasty_00004$PFT = "clasty"
empnig_00001$PFT = "empnig"
empnig_00002$PFT = "empnig"
empnig_00003$PFT = "empnig"
empnig_00004$PFT = "empnig"
empnig_00005$PFT = "empnig"
empnig_00006$PFT = "empnig"
empnig_00007$PFT = "empnig"
empnig_00008$PFT = "empnig"
icmeri_00002$PFT = "icmeri"
icmeri_00003$PFT = "icmeri"
icmeri_00004$PFT = "icmeri"
icmeri_00005$PFT = "icmeri"
icmeri_00006$PFT = "icmeri"
icmeri_00007$PFT = "icmeri"
icmeri_00008$PFT = "icmeri"
icmeri_00009$PFT = "icmeri"
leddec_00001$PFT = "leddec"
leddec_00002$PFT = "leddec"
leddec_00003$PFT = "leddec"
leddec_00004$PFT = "leddec"
rubcam_00001$PFT = "rubcam"
rubcam_00002$PFT = "rubcam"
rubcam_00003$PFT = "rubcam"
rubcam_00004$PFT = "rubcam"
rubcha_00001$PFT = "rubcha"
rubcha_00002$PFT = "rubcha"
rubcha_00003$PFT = "rubcha"
rubcha_00004$PFT = "rubcha"
sphfus_00001$PFT = "sphfus"
sphfus_00002$PFT = "sphfus"
sphfus_00003$PFT = "sphfus"
sphfus_00004$PFT = "sphfus"
vacvit_00001$PFT = "vacvit"
vacvit_00002$PFT = "vacvit"
vacvit_00003$PFT = "vacvit"
vacvit_00004$PFT = "vacvit"

bethelLib<-rbind(
arcnig_00001
,arcnig_00002
,arcnig_00003
,arcnig_00004
,arcsta_00001
,arcsta_00002
,arcsta_00003
,arcsta_00004
,betnan_00001
,betnan_00002
,betnan_00003
,betnan_00004
,claste_00001
,claste_00002
,claste_00003
,claste_00004
,clasty_00001
,clasty_00002
,clasty_00003
,clasty_00004
,empnig_00001
,empnig_00002
,empnig_00003
,empnig_00004
,empnig_00005
,empnig_00006
,empnig_00007
,empnig_00008
,icmeri_00002
,icmeri_00003
,icmeri_00004
,icmeri_00005
,icmeri_00006
,icmeri_00007
,icmeri_00008
,icmeri_00009
,leddec_00001
,leddec_00002
,leddec_00003
,leddec_00004
,rubcam_00001
,rubcam_00002
,rubcam_00003
,rubcam_00004
,rubcha_00001
,rubcha_00002
,rubcha_00003
,rubcha_00004
,sphfus_00001
,sphfus_00002
,sphfus_00003
,sphfus_00004
,vacvit_00001
,vacvit_00002
,vacvit_00003
,vacvit_00004
);

##YKDeltLib


alnfru_00001<-read.table("alnfrulclip2_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00002<-read.table("alnfrulclip2_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00003<-read.table("alnfrulclip2_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00004<-read.table("alnfrulclip2_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00005<-read.table("alnfrulclip2_00005.sed", header = FALSE, sep="", skip=27)
alnfru_00006<-read.table("alnfrulclip3_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00007<-read.table("alnfrulclip3_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00008<-read.table("alnfrulclip3_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00009<-read.table("alnfrulclip3_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00010<-read.table("alnfrulclip4_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00011<-read.table("alnfrulclip4_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00012<-read.table("alnfrulclip4_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00013<-read.table("alnfrulclip4_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00014<-read.table("alnfrulclip5_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00015<-read.table("alnfrulclip5_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00016<-read.table("alnfrulclip5_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00017<-read.table("alnfrulclip5_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00018<-read.table("alnfrulclip6_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00019<-read.table("alnfrulclip6_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00020<-read.table("alnfrulclip6_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00021<-read.table("alnfrulclip6_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00022<-read.table("alnfrulclip_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00023<-read.table("alnfrulclip_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00024<-read.table("alnfrulclip_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00025<-read.table("alnfrulclip_00004.sed", header = FALSE, sep="", skip=27)
calcan_00001<-read.table("calcanlclip_00001.sed", header = FALSE, sep="", skip=27)
calcan_00002<-read.table("calcanlclip_00002.sed", header = FALSE, sep="", skip=27)
calcan_00003<-read.table("calcanlclip_00003.sed", header = FALSE, sep="", skip=27)
calcan_00004<-read.table("calcanlclip_00004.sed", header = FALSE, sep="", skip=27)
carlin_00001<-read.table("carlinlclip1_00001.sed", header = FALSE, sep="", skip=27)
carlin_00002<-read.table("carlinlclip1_00002.sed", header = FALSE, sep="", skip=27)
carlin_00003<-read.table("carlinlclip1_00003.sed", header = FALSE, sep="", skip=27)
carlin_00004<-read.table("carlinlclip1_00004.sed", header = FALSE, sep="", skip=27)
carlin_00005<-read.table("carlinlclip2_00001.sed", header = FALSE, sep="", skip=27)
carlin_00006<-read.table("carlinlclip2_00002.sed", header = FALSE, sep="", skip=27)
carlin_00007<-read.table("carlinlclip2_00003.sed", header = FALSE, sep="", skip=27)
carlin_00008<-read.table("carlinlclip2_00004.sed", header = FALSE, sep="", skip=27)
salala_00001<-read.table("salalalclip_00001.sed", header = FALSE, sep="", skip=27)
salala_00002<-read.table("salalalclip_00002.sed", header = FALSE, sep="", skip=27)
salala_00003<-read.table("salalalclip_00003.sed", header = FALSE, sep="", skip=27)
salala_00004<-read.table("salalalclip_00004.sed", header = FALSE, sep="", skip=27)
salala_00005<-read.table("salalalclip_00005.sed", header = FALSE, sep="", skip=27)
salarb_00001<-read.table("salarblclip_00001.sed", header = FALSE, sep="", skip=27)
salarb_00002<-read.table("salarblclip_00002.sed", header = FALSE, sep="", skip=27)
salarb_00003<-read.table("salarblclip_00003.sed", header = FALSE, sep="", skip=27)
salarb_00004<-read.table("salarblclip_00004.sed", header = FALSE, sep="", skip=27)
sallan_00001<-read.table("sallanlclip_00001.sed", header = FALSE, sep="", skip=27)
sallan_00002<-read.table("sallanlclip_00002.sed", header = FALSE, sep="", skip=27)
sallan_00003<-read.table("sallanlclip_00003.sed", header = FALSE, sep="", skip=27)
sallan_00004<-read.table("sallanlclip_00004.sed", header = FALSE, sep="", skip=27)
salpul_00001<-read.table("salpullclip_00001.sed", header = FALSE, sep="", skip=27)
salpul_00002<-read.table("salpullclip_00002.sed", header = FALSE, sep="", skip=27)
salpul_00003<-read.table("salpullclip_00003.sed", header = FALSE, sep="", skip=27)
salpul_00004<-read.table("salpullclip_00004.sed", header = FALSE, sep="", skip=27)
salpul_00005<-read.table("salpullclip_00005.sed", header = FALSE, sep="", skip=27)
alnfru_00026<-read.table("site14alnfrulclip1_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00027<-read.table("site14alnfrulclip1_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00028<-read.table("site14alnfrulclip1_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00029<-read.table("site14alnfrulclip1_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00030<-read.table("site14alnfrulclip2_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00031<-read.table("site14alnfrulclip2_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00032<-read.table("site14alnfrulclip2_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00033<-read.table("site14alnfrulclip2_00004.sed", header = FALSE, sep="", skip=27)
salala_00006<-read.table("site14salalalclip1_00001.sed", header = FALSE, sep="", skip=27)
salala_00007<-read.table("site14salalalclip1_00002.sed", header = FALSE, sep="", skip=27)
salala_00008<-read.table("site14salalalclip1_00003.sed", header = FALSE, sep="", skip=27)
salala_00009<-read.table("site14salalalclip1_00004.sed", header = FALSE, sep="", skip=27)
salala_00010<-read.table("site14salalalclip2_00001.sed", header = FALSE, sep="", skip=27)
salala_00011<-read.table("site14salalalclip2_00002.sed", header = FALSE, sep="", skip=27)
salala_00012<-read.table("site14salalalclip2_00003.sed", header = FALSE, sep="", skip=27)
salala_00013<-read.table("site14salalalclip2_00004.sed", header = FALSE, sep="", skip=27)
salala_00014<-read.table("site14salalalclip3_00001.sed", header = FALSE, sep="", skip=27)
salala_00015<-read.table("site14salalalclip3_00002.sed", header = FALSE, sep="", skip=27)
salala_00016<-read.table("site14salalalclip3_00003.sed", header = FALSE, sep="", skip=27)
salala_00017<-read.table("site14salalalclip3_00004.sed", header = FALSE, sep="", skip=27)
salgla_00001<-read.table("site14salglalclip1_00001.sed", header = FALSE, sep="", skip=27)
salgla_00002<-read.table("site14salglalclip1_00002.sed", header = FALSE, sep="", skip=27)
salgla_00003<-read.table("site14salglalclip1_00003.sed", header = FALSE, sep="", skip=27)
salgla_00004<-read.table("site14salglalclip1_00004.sed", header = FALSE, sep="", skip=27)
salgla_00005<-read.table("site14salglalclip2_00001.sed", header = FALSE, sep="", skip=27)
salgla_00006<-read.table("site14salglalclip2_00002.sed", header = FALSE, sep="", skip=27)
salgla_00007<-read.table("site14salglalclip2_00003.sed", header = FALSE, sep="", skip=27)
salgla_00008<-read.table("site14salglalclip2_00004.sed", header = FALSE, sep="", skip=27)
salgla_00009<-read.table("site14salglalclip3_00001.sed", header = FALSE, sep="", skip=27)
salgla_00010<-read.table("site14salglalclip3_00002.sed", header = FALSE, sep="", skip=27)
salgla_00011<-read.table("site14salglalclip3_00003.sed", header = FALSE, sep="", skip=27)
salgla_00012<-read.table("site14salglalclip3_00004.sed", header = FALSE, sep="", skip=27)
salala_00018<-read.table("site16salalalclip1_00001.sed", header = FALSE, sep="", skip=27)
salala_00019<-read.table("site16salalalclip1_00002.sed", header = FALSE, sep="", skip=27)
salala_00020<-read.table("site16salalalclip1_00003.sed", header = FALSE, sep="", skip=27)
salala_00021<-read.table("site16salalalclip1_00004.sed", header = FALSE, sep="", skip=27)
salala_00022<-read.table("site16salalalclip2_00001.sed", header = FALSE, sep="", skip=27)
salala_00023<-read.table("site16salalalclip2_00002.sed", header = FALSE, sep="", skip=27)
salala_00024<-read.table("site16salalalclip2_00003.sed", header = FALSE, sep="", skip=27)
salala_00025<-read.table("site16salalalclip2_00004.sed", header = FALSE, sep="", skip=27)
salala_00026<-read.table("site16salalalclip3_00001.sed", header = FALSE, sep="", skip=27)
salala_00027<-read.table("site16salalalclip3_00002.sed", header = FALSE, sep="", skip=27)
salala_00028<-read.table("site16salalalclip3_00003.sed", header = FALSE, sep="", skip=27)
salala_00029<-read.table("site16salalalclip3_00004.sed", header = FALSE, sep="", skip=27)
salala_00030<-read.table("site16salalalclip4_00001.sed", header = FALSE, sep="", skip=27)
salala_00031<-read.table("site16salalalclip4_00002.sed", header = FALSE, sep="", skip=27)
salala_00032<-read.table("site16salalalclip4_00003.sed", header = FALSE, sep="", skip=27)
salala_00033<-read.table("site16salalalclip4_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00034<-read.table("site19alnfrulclip2_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00035<-read.table("site19alnfrulclip2_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00036<-read.table("site19alnfrulclip2_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00037<-read.table("site19alnfrulclip2_00004.sed", header = FALSE, sep="", skip=27)
alnfru_00038<-read.table("site19alnfrulclip3_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00039<-read.table("site19alnfrulclip3_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00040<-read.table("site19alnfrulclip3_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00041<-read.table("site19alnfrulclip_00001.sed", header = FALSE, sep="", skip=27)
alnfru_00042<-read.table("site19alnfrulclip_00002.sed", header = FALSE, sep="", skip=27)
alnfru_00043<-read.table("site19alnfrulclip_00003.sed", header = FALSE, sep="", skip=27)
alnfru_00044<-read.table("site19alnfrulclip_00004.sed", header = FALSE, sep="", skip=27)
herlan_00001<-read.table("site19herlanlcli2_00001.sed", header = FALSE, sep="", skip=27)
herlan_00002<-read.table("site19herlanlcli2_00002.sed", header = FALSE, sep="", skip=27)
herlan_00003<-read.table("site19herlanlcli2_00003.sed", header = FALSE, sep="", skip=27)
herlan_00004<-read.table("site19herlanlcli2_00004.sed", header = FALSE, sep="", skip=27)
herlan_00005<-read.table("site19herlanlclip_00001.sed", header = FALSE, sep="", skip=27)
herlan_00006<-read.table("site19herlanlclip_00002.sed", header = FALSE, sep="", skip=27)
herlan_00007<-read.table("site19herlanlclip_00003.sed", header = FALSE, sep="", skip=27)
herlan_00008<-read.table("site19herlanlclip_00004.sed", header = FALSE, sep="", skip=27)
salric_00001<-read.table("site19salriclclip2_00001.sed", header = FALSE, sep="", skip=27)
salric_00002<-read.table("site19salriclclip2_00002.sed", header = FALSE, sep="", skip=27)
salric_00003<-read.table("site19salriclclip2_00003.sed", header = FALSE, sep="", skip=27)
salric_00004<-read.table("site19salriclclip2_00004.sed", header = FALSE, sep="", skip=27)
betnan_00001<-read.table("site21betnanlclip_00001.sed", header = FALSE, sep="", skip=27)
betnan_00002<-read.table("site21betnanlclip_00002.sed", header = FALSE, sep="", skip=27)
betnan_00003<-read.table("site21betnanlclip_00003.sed", header = FALSE, sep="", skip=27)
betnan_00004<-read.table("site21betnanlclip_00004.sed", header = FALSE, sep="", skip=27)
clacuc_00001<-read.table("site21clacuccprobe_00001.sed", header = FALSE, sep="", skip=27)
clacuc_00002<-read.table("site21clacuccprobe_00002.sed", header = FALSE, sep="", skip=27)
clacuc_00003<-read.table("site21clacuccprobe_00003.sed", header = FALSE, sep="", skip=27)
clacuc_00004<-read.table("site21clacuccprobe_00004.sed", header = FALSE, sep="", skip=27)
clacuc_00005<-read.table("site21clacuccprobe_00005.sed", header = FALSE, sep="", skip=27)
##clasti_00001<-read.table("site21clasticprobe_00001.sed", header = FALSE, sep="", skip=27)
##clasti_00002<-read.table("site21clasticprobe_00002.sed", header = FALSE, sep="", skip=27)
##clasti_00003<-read.table("site21clasticprobe_00003.sed", header = FALSE, sep="", skip=27)
##clasti_00004<-read.table("site21clasticprobe_00004.sed", header = FALSE, sep="", skip=27)
##clasti_00005<-read.table("site21clasticprobe_00005.sed", header = FALSE, sep="", skip=27)
##clasti_00006<-read.table("site21clasticprobe_00006.sed", header = FALSE, sep="", skip=27)
equarv_00001<-read.table("site22equarvlclip_00001.sed", header = FALSE, sep="", skip=27)
equarv_00002<-read.table("site22equarvlclip_00002.sed", header = FALSE, sep="", skip=27)
equarv_00003<-read.table("site22equarvlclip_00003.sed", header = FALSE, sep="", skip=27)
equarv_00004<-read.table("site22equarvlclip_00004.sed", header = FALSE, sep="", skip=27)
salala_00034<-read.table("site22salalalclip_00001.sed", header = FALSE, sep="", skip=27)
salala_00035<-read.table("site22salalalclip_00002.sed", header = FALSE, sep="", skip=27)
salala_00036<-read.table("site22salalalclip_00003.sed", header = FALSE, sep="", skip=27)
salala_00036<-read.table("site22salalalclip_00004.sed", header = FALSE, sep="", skip=27)
salala_00037<-read.table("site22salalalclip_00005.sed", header = FALSE, sep="", skip=27)
salala_00038<-read.table("site22salalalclip_00006.sed", header = FALSE, sep="", skip=27)
carlyn_00001<-read.table("site24carlynlclip_00001.sed", header = FALSE, sep="", skip=27)
carlyn_00002<-read.table("site24carlynlclip_00002.sed", header = FALSE, sep="", skip=27)
carlyn_00003<-read.table("site24carlynlclip_00003.sed", header = FALSE, sep="", skip=27)
carlyn_00004<-read.table("site24carlynlclip_00004.sed", header = FALSE, sep="", skip=27)
carram_00001<-read.table("site24carramlclip_00001.sed", header = FALSE, sep="", skip=27)
carram_00002<-read.table("site24carramlclip_00002.sed", header = FALSE, sep="", skip=27)
carram_00003<-read.table("site24carramlclip_00003.sed", header = FALSE, sep="", skip=27)
carram_00004<-read.table("site24carramlclip_00004.sed", header = FALSE, sep="", skip=27)
irisit_00001<-read.table("site24irisitlclip_00001.sed", header = FALSE, sep="", skip=27)
irisit_00002<-read.table("site24irisitlclip_00002.sed", header = FALSE, sep="", skip=27)
irisit_00003<-read.table("site24irisitlclip_00003.sed", header = FALSE, sep="", skip=27)
irisit_00004<-read.table("site24irisitlclip_00004.sed", header = FALSE, sep="", skip=27)
pedsud_00002<-read.table("site24pedsud_00001.sed", header = FALSE, sep="", skip=27)
pedsud_00003<-read.table("site24pedsud_00002.sed", header = FALSE, sep="", skip=27)
pedsud_00004<-read.table("site24pedsud_00003.sed", header = FALSE, sep="", skip=27)
pedsud_00001<-read.table("site24pedsud_00004.sed", header = FALSE, sep="", skip=27)
salala_00039<-read.table("site24salalalclip_00001.sed", header = FALSE, sep="", skip=27)
salala_00040<-read.table("site24salalalclip_00002.sed", header = FALSE, sep="", skip=27)
salala_00041<-read.table("site24salalalclip_00003.sed", header = FALSE, sep="", skip=27)
salala_00042<-read.table("site24salalalclip_00004.sed", header = FALSE, sep="", skip=27)
salova_00001<-read.table("site24Salova_00001.sed", header = FALSE, sep="", skip=27)
salova_00002<-read.table("site24Salova_00002.sed", header = FALSE, sep="", skip=27)
salova_00003<-read.table("site24Salova_00003.sed", header = FALSE, sep="", skip=27)
salova_00004<-read.table("site24Salova_00004.sed", header = FALSE, sep="", skip=27)
salova_00005<-read.table("site24Salova_00005.sed", header = FALSE, sep="", skip=27)
salova_00006<-read.table("site24Salova_00006.sed", header = FALSE, sep="", skip=27)
popbal_00001<-read.table("site27popballclip_00001.sed", header = FALSE, sep="", skip=27)
popbal_00002<-read.table("site27popballclip_00002.sed", header = FALSE, sep="", skip=27)
popbal_00003<-read.table("site27popballclip_00003.sed", header = FALSE, sep="", skip=27)
popbal_00004<-read.table("site27popballclip_00004.sed", header = FALSE, sep="", skip=27)
salala_00043<-read.table("site27salalalclip_00001.sed", header = FALSE, sep="", skip=27)
salala_00044<-read.table("site27salalalclip_00002.sed", header = FALSE, sep="", skip=27)
salala_00045<-read.table("site27salalalclip_00003.sed", header = FALSE, sep="", skip=27)
salala_00046<-read.table("site27salalalclip_00004.sed", header = FALSE, sep="", skip=27)
salgla_00013<-read.table("site27salglalclip_00001.sed", header = FALSE, sep="", skip=27)
salgla_00014<-read.table("site27salglalclip_00002.sed", header = FALSE, sep="", skip=27)
salgla_00015<-read.table("site27salglalclip_00003.sed", header = FALSE, sep="", skip=27)
salgla_00016<-read.table("site27salglalclip_00004.sed", header = FALSE, sep="", skip=27)

alnfru_00001$PFT = "alnfru"
alnfru_00002$PFT = "alnfru"
alnfru_00003$PFT = "alnfru"
alnfru_00004$PFT = "alnfru"
alnfru_00005$PFT = "alnfru"
alnfru_00006$PFT = "alnfru"
alnfru_00007$PFT = "alnfru"
alnfru_00008$PFT = "alnfru"
alnfru_00009$PFT = "alnfru"
alnfru_00010$PFT = "alnfru"
alnfru_00011$PFT = "alnfru"
alnfru_00012$PFT = "alnfru"
alnfru_00013$PFT = "alnfru"
alnfru_00014$PFT = "alnfru"
alnfru_00015$PFT = "alnfru"
alnfru_00016$PFT = "alnfru"
alnfru_00017$PFT = "alnfru"
alnfru_00018$PFT = "alnfru"
alnfru_00019$PFT = "alnfru"
alnfru_00020$PFT = "alnfru"
alnfru_00021$PFT = "alnfru"
alnfru_00022$PFT = "alnfru"
alnfru_00023$PFT = "alnfru"
alnfru_00024$PFT = "alnfru"
alnfru_00025$PFT = "alnfru"
calcan_00001$PFT = "calcan"
calcan_00002$PFT = "calcan"
calcan_00003$PFT = "calcan"
calcan_00004$PFT = "calcan"
carlin_00001$PFT = "carlin"
carlin_00002$PFT = "carlin"
carlin_00003$PFT = "carlin"
carlin_00004$PFT = "carlin"
carlin_00005$PFT = "carlin"
carlin_00006$PFT = "carlin"
carlin_00007$PFT = "carlin"
carlin_00008$PFT = "carlin"
salala_00001$PFT = "salala"
salala_00002$PFT = "salala"
salala_00003$PFT = "salala"
salala_00004$PFT = "salala"
salala_00005$PFT = "salala"
salarb_00001$PFT = "salarb"
salarb_00002$PFT = "salarb"
salarb_00003$PFT = "salarb"
salarb_00004$PFT = "salarb"
sallan_00001$PFT = "sallan"
sallan_00002$PFT = "sallan"
sallan_00003$PFT = "sallan"
sallan_00004$PFT = "sallan"
salpul_00001$PFT = "salpul"
salpul_00002$PFT = "salpul"
salpul_00003$PFT = "salpul"
salpul_00004$PFT = "salpul"
salpul_00005$PFT = "salpul"
alnfru_00026$PFT = "alnfru"
alnfru_00027$PFT = "alnfru"
alnfru_00028$PFT = "alnfru"
alnfru_00029$PFT = "alnfru"
alnfru_00030$PFT = "alnfru"
alnfru_00031$PFT = "alnfru"
alnfru_00032$PFT = "alnfru"
alnfru_00033$PFT = "alnfru"
salala_00006$PFT = "salala"
salala_00007$PFT = "salala"
salala_00008$PFT = "salala"
salala_00009$PFT = "salala"
salala_00010$PFT = "salala"
salala_00011$PFT = "salala"
salala_00012$PFT = "salala"
salala_00013$PFT = "salala"
salala_00014$PFT = "salala"
salala_00015$PFT = "salala"
salala_00016$PFT = "salala"
salala_00017$PFT = "salala"
salgla_00001$PFT = "salgla"
salgla_00002$PFT = "salgla"
salgla_00003$PFT = "salgla"
salgla_00004$PFT = "salgla"
salgla_00005$PFT = "salgla"
salgla_00006$PFT = "salgla"
salgla_00007$PFT = "salgla"
salgla_00008$PFT = "salgla"
salgla_00009$PFT = "salgla"
salgla_00010$PFT = "salgla"
salgla_00011$PFT = "salgla"
salgla_00012$PFT = "salgla"
salala_00018$PFT = "salala"
salala_00019$PFT = "salala"
salala_00020$PFT = "salala"
salala_00021$PFT = "salala"
salala_00022$PFT = "salala"
salala_00023$PFT = "salala"
salala_00024$PFT = "salala"
salala_00025$PFT = "salala"
salala_00026$PFT = "salala"
salala_00027$PFT = "salala"
salala_00028$PFT = "salala"
salala_00029$PFT = "salala"
salala_00030$PFT = "salala"
salala_00031$PFT = "salala"
salala_00032$PFT = "salala"
salala_00033$PFT = "salala"
alnfru_00034$PFT = "alnfru"
alnfru_00035$PFT = "alnfru"
alnfru_00036$PFT = "alnfru"
alnfru_00037$PFT = "alnfru"
alnfru_00038$PFT = "alnfru"
alnfru_00039$PFT = "alnfru"
alnfru_00040$PFT = "alnfru"
alnfru_00041$PFT = "alnfru"
alnfru_00042$PFT = "alnfru"
alnfru_00043$PFT = "alnfru"
alnfru_00044$PFT = "alnfru"
herlan_00001$PFT = "herlan"
herlan_00002$PFT = "herlan"
herlan_00003$PFT = "herlan"
herlan_00004$PFT = "herlan"
herlan_00005$PFT = "herlan"
herlan_00006$PFT = "herlan"
herlan_00007$PFT = "herlan"
herlan_00008$PFT = "herlan"
salric_00001$PFT = "salric"
salric_00002$PFT = "salric"
salric_00003$PFT = "salric"
salric_00004$PFT = "salric"
betnan_00001$PFT = "betnan"
betnan_00002$PFT = "betnan"
betnan_00003$PFT = "betnan"
betnan_00004$PFT = "betnan"
clacuc_00001$PFT = "clacuc"
clacuc_00002$PFT = "clacuc"
clacuc_00003$PFT = "clacuc"
clacuc_00004$PFT = "clacuc"
clacuc_00005$PFT = "clacuc"
equarv_00001$PFT = "equarv"
equarv_00002$PFT = "equarv"
equarv_00003$PFT = "equarv"
equarv_00004$PFT = "equarv"
salala_00034$PFT = "salala"
salala_00035$PFT = "salala"
salala_00036$PFT = "salala"
salala_00036$PFT = "salala"
salala_00037$PFT = "salala"
salala_00038$PFT = "salala"
carlyn_00001$PFT = "carlyn"
carlyn_00002$PFT = "carlyn"
carlyn_00003$PFT = "carlyn"
carlyn_00004$PFT = "carlyn"
carram_00001$PFT = "carram"
carram_00002$PFT = "carram"
carram_00003$PFT = "carram"
carram_00004$PFT = "carram"
irisit_00001$PFT = "irisit"
irisit_00002$PFT = "irisit"
irisit_00003$PFT = "irisit"
irisit_00004$PFT = "irisit"
pedsud_00002$PFT = "pedsud"
pedsud_00003$PFT = "pedsud"
pedsud_00004$PFT = "pedsud"
pedsud_00001$PFT = "pedsud"
salala_00039$PFT = "salala"
salala_00040$PFT = "salala"
salala_00041$PFT = "salala"
salala_00042$PFT = "salala"
salova_00001$PFT = "salova"
salova_00002$PFT = "salova"
salova_00003$PFT = "salova"
salova_00004$PFT = "salova"
salova_00005$PFT = "salova"
salova_00006$PFT = "salova"
popbal_00001$PFT = "popbal"
popbal_00002$PFT = "popbal"
popbal_00003$PFT = "popbal"
popbal_00004$PFT = "popbal"
salala_00043$PFT = "salala"
salala_00044$PFT = "salala"
salala_00045$PFT = "salala"
salala_00046$PFT = "salala"
salgla_00013$PFT = "salgla"
salgla_00014$PFT = "salgla"
salgla_00015$PFT = "salgla"
salgla_00016$PFT = "salgla"

yKDeltLib<-rbind (
alnfru_00001
,alnfru_00002
,alnfru_00003
,alnfru_00004
,alnfru_00005
,alnfru_00006
,alnfru_00007
,alnfru_00008
,alnfru_00009
,alnfru_00010
,alnfru_00011
,alnfru_00012
,alnfru_00013
,alnfru_00014
,alnfru_00015
,alnfru_00016
,alnfru_00017
,alnfru_00018
,alnfru_00019
,alnfru_00020
,alnfru_00021
,alnfru_00022
,alnfru_00023
,alnfru_00024
,alnfru_00025
,calcan_00001
,calcan_00002
,calcan_00003
,calcan_00004
,carlin_00001
,carlin_00002
,carlin_00003
,carlin_00004
,carlin_00005
,carlin_00006
,carlin_00007
,carlin_00008
,salala_00001
,salala_00002
,salala_00003
,salala_00004
,salala_00005
,salarb_00001
,salarb_00002
,salarb_00003
,salarb_00004
,sallan_00001
,sallan_00002
,sallan_00003
,sallan_00004
,salpul_00001
,salpul_00002
,salpul_00003
,salpul_00004
,salpul_00005
,alnfru_00026
,alnfru_00027
,alnfru_00028
,alnfru_00029
,alnfru_00030
,alnfru_00031
,alnfru_00032
,alnfru_00033
,salala_00006
,salala_00007
,salala_00008
,salala_00009
,salala_00010
,salala_00011
,salala_00012
,salala_00013
,salala_00014
,salala_00015
,salala_00016
,salala_00017
,salgla_00001
,salgla_00002
,salgla_00003
,salgla_00004
,salgla_00005
,salgla_00006
,salgla_00007
,salgla_00008
,salgla_00009
,salgla_00010
,salgla_00011
,salgla_00012
,salala_00018
,salala_00019
,salala_00020
,salala_00021
,salala_00022
,salala_00023
,salala_00024
,salala_00025
,salala_00026
,salala_00027
,salala_00028
,salala_00029
,salala_00030
,salala_00031
,salala_00032
,salala_00033
,alnfru_00034
,alnfru_00035
,alnfru_00036
,alnfru_00037
,alnfru_00038
,alnfru_00039
,alnfru_00040
,alnfru_00041
,alnfru_00042
,alnfru_00043
,alnfru_00044
,herlan_00001
,herlan_00002
,herlan_00003
,herlan_00004
,herlan_00005
,herlan_00006
,herlan_00007
,herlan_00008
,salric_00001
,salric_00002
,salric_00003
,salric_00004
,betnan_00001
,betnan_00002
,betnan_00003
,betnan_00004
,clacuc_00001
,clacuc_00002
,clacuc_00003
,clacuc_00004
,clacuc_00005
,equarv_00001
,equarv_00002
,equarv_00003
,equarv_00004
,salala_00034
,salala_00035
,salala_00036
,salala_00036
,salala_00037
,salala_00038
,carlyn_00001
,carlyn_00002
,carlyn_00003
,carlyn_00004
,carram_00001
,carram_00002
,carram_00003
,carram_00004
,irisit_00001
,irisit_00002
,irisit_00003
,irisit_00004
,pedsud_00002
,pedsud_00003
,pedsud_00004
,pedsud_00001
,salala_00039
,salala_00040
,salala_00041
,salala_00042
,salova_00001
,salova_00002
,salova_00003
,salova_00004
,salova_00005
,salova_00006
,popbal_00001
,popbal_00002
,popbal_00003
,popbal_00004
,salala_00043
,salala_00044
,salala_00045
,salala_00046
,salgla_00013
,salgla_00014
,salgla_00015
,salgla_00016
);

##Group by species before you bind with other sites! (YKDELTLIB OR BETHELLIB)
##brooksLib

arccen_00001<-read.table("arccen_00001.sed", header = FALSE, sep="", skip=27)
arccen_00002<-read.table("arccen_00002.sed", header = FALSE, sep="", skip=27)
arccen_00003<-read.table("arccen_00003.sed", header = FALSE, sep="", skip=27)
arccen_00004<-read.table("arccen_00004.sed", header = FALSE, sep="", skip=27)
asachr_00001<-read.table("asachr_00001.sed", header = FALSE, sep="", skip=27)
asachr_00002<-read.table("asachr_00002.sed", header = FALSE, sep="", skip=27)
asachr_00003<-read.table("asachr_00003.sed", header = FALSE, sep="", skip=27)
asachr_00004<-read.table("asachr_00004.sed", header = FALSE, sep="", skip=27)
aultur_00001<-read.table("aultur_00001.sed", header = FALSE, sep="", skip=27)
aultur_00002<-read.table("aultur_00002.sed", header = FALSE, sep="", skip=27)
aultur_00003<-read.table("aultur_00003.sed", header = FALSE, sep="", skip=27)
aultur_00004<-read.table("aultur_00004.sed", header = FALSE, sep="", skip=27)
betnan_00001<-read.table("betann_00001.sed", header = FALSE, sep="", skip=27)
betnan_00002<-read.table("betann_00002.sed", header = FALSE, sep="", skip=27)
betnan_00003<-read.table("betann_00003.sed", header = FALSE, sep="", skip=27)
betnan_00004<-read.table("betann_00004.sed", header = FALSE, sep="", skip=27)
betnan_00005<-read.table("betnan_00001.sed", header = FALSE, sep="", skip=27)
betnan_00006<-read.table("betnan_00002.sed", header = FALSE, sep="", skip=27)
betnan_00007<-read.table("betnan_00003.sed", header = FALSE, sep="", skip=27)
betnan_00008<-read.table("betnan_00004.sed", header = FALSE, sep="", skip=27)
betneo_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
betneo_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
betneo_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
betneo_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
cerpur_00005<-read.table("cerpur_00001.sed", header = FALSE, sep="", skip=27)
cerpur_00001<-read.table("cerpur_00002.sed", header = FALSE, sep="", skip=27)
cerpur_00002<-read.table("cerpur_00003.sed", header = FALSE, sep="", skip=27)
cerpur_00003<-read.table("cerpur_00004.sed", header = FALSE, sep="", skip=27)
cerpur_00004<-read.table("cerpur_00005.sed", header = FALSE, sep="", skip=27)
clamit_00001<-read.table("clamit_00001.sed", header = FALSE, sep="", skip=27)
clamit_00002<-read.table("clamit_00002.sed", header = FALSE, sep="", skip=27)
clamit_00003<-read.table("clamit_00003.sed", header = FALSE, sep="", skip=27)
clamit_00004<-read.table("clamit_00004.sed", header = FALSE, sep="", skip=27)
claste_00001<-read.table("claste2_00001.sed", header = FALSE, sep="", skip=27)
claste_00002<-read.table("claste2_00002.sed", header = FALSE, sep="", skip=27)
claste_00003<-read.table("claste2_00003.sed", header = FALSE, sep="", skip=27)
claste_00004<-read.table("claste2_00004.sed", header = FALSE, sep="", skip=27)
claste_00005<-read.table("claste_00001.sed", header = FALSE, sep="", skip=27)
claste_00006<-read.table("claste_00002.sed", header = FALSE, sep="", skip=27)
claste_00007<-read.table("claste_00003.sed", header = FALSE, sep="", skip=27)
claste_00008<-read.table("claste_00004.sed", header = FALSE, sep="", skip=27)
clasty_00001<-read.table("clasty2_00001.sed", header = FALSE, sep="", skip=27)
clasty_00002<-read.table("clasty2_00002.sed", header = FALSE, sep="", skip=27)
clasty_00003<-read.table("clasty2_00003.sed", header = FALSE, sep="", skip=27)
clasty_00004<-read.table("clasty2_00004.sed", header = FALSE, sep="", skip=27)
clasty_00005<-read.table("clasty_00001.sed", header = FALSE, sep="", skip=27)
clasty_00006<-read.table("clasty_00002.sed", header = FALSE, sep="", skip=27)
clasty_00007<-read.table("clasty_00003.sed", header = FALSE, sep="", skip=27)
clasty_00008<-read.table("clasty_00004.sed", header = FALSE, sep="", skip=27)
claunc_00001<-read.table("claunc_00001.sed", header = FALSE, sep="", skip=27)
claunc_00002<-read.table("claunc_00002.sed", header = FALSE, sep="", skip=27)
claunc_00003<-read.table("claunc_00003.sed", header = FALSE, sep="", skip=27)
claunc_00004<-read.table("claunc_00004.sed", header = FALSE, sep="", skip=27)
empnig_00001<-read.table("empnig_00001.sed", header = FALSE, sep="", skip=27)
empnig_00002<-read.table("empnig_00002.sed", header = FALSE, sep="", skip=27)
empnig_00003<-read.table("empnig_00003.sed", header = FALSE, sep="", skip=27)
empnig_00004<-read.table("empnig_00004.sed", header = FALSE, sep="", skip=27)
erivag_00001<-read.table("erivag_00001.sed", header = FALSE, sep="", skip=27)
erivag_00002<-read.table("erivag_00002.sed", header = FALSE, sep="", skip=27)
erivag_00003<-read.table("erivag_00003.sed", header = FALSE, sep="", skip=27)
erivag_00004<-read.table("erivag_00004.sed", header = FALSE, sep="", skip=27)
erivag_00005<-read.table("erivag_00005.sed", header = FALSE, sep="", skip=27)
erivag_00006<-read.table("erivag_00006.sed", header = FALSE, sep="", skip=27)
erivag_00007<-read.table("erivag_00007.sed", header = FALSE, sep="", skip=27)
erivag_00008<-read.table("erivag_00008.sed", header = FALSE, sep="", skip=27)
erivag_00009<-read.table("erivag_00009.sed", header = FALSE, sep="", skip=27)
erivag_00010<-read.table("erivag_00010.sed", header = FALSE, sep="", skip=27)
flacuc_00001<-read.table("flacuc_00001.sed", header = FALSE, sep="", skip=27)
flacuc_00002<-read.table("flacuc_00002.sed", header = FALSE, sep="", skip=27)
flacuc_00003<-read.table("flacuc_00003.sed", header = FALSE, sep="", skip=27)
flacuc_00004<-read.table("flacuc_00004.sed", header = FALSE, sep="", skip=27)
flaniv_00001<-read.table("flaniv2_00001.sed", header = FALSE, sep="", skip=27)
flaniv_00002<-read.table("flaniv2_00002.sed", header = FALSE, sep="", skip=27)
flaniv_00003<-read.table("flaniv2_00003.sed", header = FALSE, sep="", skip=27)
flaniv_00004<-read.table("flaniv_00001.sed", header = FALSE, sep="", skip=27)
flaniv_00005<-read.table("flaniv_00002.sed", header = FALSE, sep="", skip=27)
flaniv_00006<-read.table("flaniv_00003.sed", header = FALSE, sep="", skip=27)
flaniv_00007<-read.table("flaniv_00004.sed", header = FALSE, sep="", skip=27)
hypspl_00001<-read.table("hypspl_00001.sed", header = FALSE, sep="", skip=27)
hypspl_00002<-read.table("hypspl_00002.sed", header = FALSE, sep="", skip=27)
hypspl_00003<-read.table("hypspl_00003.sed", header = FALSE, sep="", skip=27)
hypspl_00004<-read.table("hypspl_00004.sed", header = FALSE, sep="", skip=27)
leddec_00001<-read.table("leddec_00001.sed", header = FALSE, sep="", skip=27)
leddec_00002<-read.table("leddec_00002.sed", header = FALSE, sep="", skip=27)
leddec_00003<-read.table("leddec_00003.sed", header = FALSE, sep="", skip=27)
leddec_00004<-read.table("leddec_00004.sed", header = FALSE, sep="", skip=27)
neparc_00001<-read.table("neparc_00001.sed", header = FALSE, sep="", skip=27)
neparc_00002<-read.table("neparc_00002.sed", header = FALSE, sep="", skip=27)
neparc_00003<-read.table("neparc_00003.sed", header = FALSE, sep="", skip=27)
neparc_00004<-read.table("neparc_00004.sed", header = FALSE, sep="", skip=27)
paromp_00001<-read.table("paromp_00001.sed", header = FALSE, sep="", skip=27)
paromp_00002<-read.table("paromp_00002.sed", header = FALSE, sep="", skip=27)
paromp_00003<-read.table("paromp_00003.sed", header = FALSE, sep="", skip=27)
paromp_00004<-read.table("paromp_00004.sed", header = FALSE, sep="", skip=27)
pelapt_00001<-read.table("pelapt_00001.sed", header = FALSE, sep="", skip=27)
pelapt_00002<-read.table("pelapt_00002.sed", header = FALSE, sep="", skip=27)
pelapt_00003<-read.table("pelapt_00003.sed", header = FALSE, sep="", skip=27)
pelapt_00004<-read.table("pelapt_00004.sed", header = FALSE, sep="", skip=27)
pelapt_00005<-read.table("pelapt_00005.sed", header = FALSE, sep="", skip=27)
poljen_00001<-read.table("poljen_00001.sed", header = FALSE, sep="", skip=27)
poljen_00002<-read.table("poljen_00002.sed", header = FALSE, sep="", skip=27)
poljen_00003<-read.table("poljen_00003.sed", header = FALSE, sep="", skip=27)
poljen_00004<-read.table("poljen_00004.sed", header = FALSE, sep="", skip=27)
raclan_00001<-read.table("raclan_00001.sed", header = FALSE, sep="", skip=27)
raclan_00002<-read.table("raclan_00002.sed", header = FALSE, sep="", skip=27)
raclan_00003<-read.table("raclan_00003.sed", header = FALSE, sep="", skip=27)
raclan_00004<-read.table("raclan_00004.sed", header = FALSE, sep="", skip=27)
rhigeo_00001<-read.table("rhigeo_00001.sed", header = FALSE, sep="", skip=27)
rhigeo_00002<-read.table("rhigeo_00002.sed", header = FALSE, sep="", skip=27)
rhigeo_00003<-read.table("rhigeo_00003.sed", header = FALSE, sep="", skip=27)
rhigeo_00004<-read.table("rhigeo_00004.sed", header = FALSE, sep="", skip=27)
rubcha_00001<-read.table("rubcha_00001.sed", header = FALSE, sep="", skip=27)
rubcha_00002<-read.table("rubcha_00002.sed", header = FALSE, sep="", skip=27)
rubcha_00003<-read.table("rubcha_00003.sed", header = FALSE, sep="", skip=27)
rubcha_00004<-read.table("rubcha_00004.sed", header = FALSE, sep="", skip=27)
stepas_00001<-read.table("stepas2_00001.sed", header = FALSE, sep="", skip=27)
stepas_00002<-read.table("stepas2_00002.sed", header = FALSE, sep="", skip=27)
stepas_00003<-read.table("stepas2_00003.sed", header = FALSE, sep="", skip=27)
stepas_00004<-read.table("stepas2_00004.sed", header = FALSE, sep="", skip=27)
stetas_00005<-read.table("stetas_00001.sed", header = FALSE, sep="", skip=27)
stetas_00006<-read.table("stetas_00002.sed", header = FALSE, sep="", skip=27)
stetas_00007<-read.table("stetas_00003.sed", header = FALSE, sep="", skip=27)
stetas_00008<-read.table("stetas_00004.sed", header = FALSE, sep="", skip=27)
umbarc_00001<-read.table("umbarc_00001.sed", header = FALSE, sep="", skip=27)
umbarc_00002<-read.table("umbarc_00002.sed", header = FALSE, sep="", skip=27)
umbarc_00003<-read.table("umbarc_00003.sed", header = FALSE, sep="", skip=27)
umbarc_00004<-read.table("umbarc_00004.sed", header = FALSE, sep="", skip=27)
vacvit_00001<-read.table("vacvit_00001.sed", header = FALSE, sep="", skip=27)
vacvit_00002<-read.table("vacvit_00002.sed", header = FALSE, sep="", skip=27)
vacvit_00003<-read.table("vacvit_00003.sed", header = FALSE, sep="", skip=27)
vacvit_00004<-read.table("vacvit_00004.sed", header = FALSE, sep="", skip=27)
vacvit_00005<-read.table("vacvit_00005.sed", header = FALSE, sep="", skip=27)


arccen_00001$PFT = "arccen"
arccen_00002$PFT = "arccen"
arccen_00003$PFT = "arccen"
arccen_00004$PFT = "arccen"
asachr_00001$PFT = "asachr"
asachr_00002$PFT = "asachr"
asachr_00003$PFT = "asachr"
asachr_00004$PFT = "asachr"
aultur_00001$PFT = "aultur"
aultur_00002$PFT = "aultur"
aultur_00003$PFT = "aultur"
aultur_00004$PFT = "aultur"
betnan_00001$PFT = "betnan"
betnan_00002$PFT = "betnan"
betnan_00003$PFT = "betnan"
betnan_00004$PFT = "betnan"
betnan_00005$PFT = "betnan"
betnan_00006$PFT = "betnan"
betnan_00007$PFT = "betnan"
betnan_00008$PFT = "betnan"
betneo_00001$PFT = "betneo"
betneo_00002$PFT = "betneo"
betneo_00003$PFT = "betneo"
betneo_00004$PFT = "betneo"
cerpur_00005$PFT = "cerpur"
cerpur_00001$PFT = "cerpur"
cerpur_00002$PFT = "cerpur"
cerpur_00003$PFT = "cerpur"
cerpur_00004$PFT = "cerpur"
clamit_00001$PFT = "clamit"
clamit_00002$PFT = "clamit"
clamit_00003$PFT = "clamit"
clamit_00004$PFT = "clamit"
claste_00001$PFT = "claste"
claste_00002$PFT = "claste"
claste_00003$PFT = "claste"
claste_00004$PFT = "claste"
claste_00005$PFT = "claste"
claste_00006$PFT = "claste"
claste_00007$PFT = "claste"
claste_00008$PFT = "claste"
clasty_00001$PFT = "clasty"
clasty_00002$PFT = "clasty"
clasty_00003$PFT = "clasty"
clasty_00004$PFT = "clasty"
clasty_00005$PFT = "clasty"
clasty_00006$PFT = "clasty"
clasty_00007$PFT = "clasty"
clasty_00008$PFT = "clasty"
claunc_00001$PFT = "claunc"
claunc_00002$PFT = "claunc"
claunc_00003$PFT = "claunc"
claunc_00004$PFT = "claunc"
empnig_00001$PFT = "empnig"
empnig_00002$PFT = "empnig"
empnig_00003$PFT = "empnig"
empnig_00004$PFT = "empnig"
erivag_00001$PFT = "erivag"
erivag_00002$PFT = "erivag"
erivag_00003$PFT = "erivag"
erivag_00004$PFT = "erivag"
erivag_00005$PFT = "erivag"
erivag_00006$PFT = "erivag"
erivag_00007$PFT = "erivag"
erivag_00008$PFT = "erivag"
erivag_00009$PFT = "erivag"
erivag_00010$PFT = "erivag"
flacuc_00001$PFT = "flacuc"
flacuc_00002$PFT = "flacuc"
flacuc_00003$PFT = "flacuc"
flacuc_00004$PFT = "flacuc"
flaniv_00001$PFT = "flaniv"
flaniv_00002$PFT = "flaniv"
flaniv_00003$PFT = "flaniv"
flaniv_00004$PFT = "flaniv"
flaniv_00005$PFT = "flaniv"
flaniv_00006$PFT = "flaniv"
flaniv_00007$PFT = "flaniv"
hypspl_00001$PFT = "hypspl"
hypspl_00002$PFT = "hypspl"
hypspl_00003$PFT = "hypspl"
hypspl_00004$PFT = "hypspl"
leddec_00001$PFT = "leddec"
leddec_00002$PFT = "leddec"
leddec_00003$PFT = "leddec"
leddec_00004$PFT = "leddec"
neparc_00001$PFT = "neparc"
neparc_00002$PFT = "neparc"
neparc_00003$PFT = "neparc"
neparc_00004$PFT = "neparc"
paromp_00001$PFT = "paromp"
paromp_00002$PFT = "paromp"
paromp_00003$PFT = "paromp"
paromp_00004$PFT = "paromp"
pelapt_00001$PFT = "pelapt"
pelapt_00002$PFT = "pelapt"
pelapt_00003$PFT = "pelapt"
pelapt_00004$PFT = "pelapt"
pelapt_00005$PFT = "pelapt"
poljen_00001$PFT = "poljen"
poljen_00002$PFT = "poljen"
poljen_00003$PFT = "poljen"
poljen_00004$PFT = "poljen"
raclan_00001$PFT = "raclan"
raclan_00002$PFT = "raclan"
raclan_00003$PFT = "raclan"
raclan_00004$PFT = "raclan"
rhigeo_00001$PFT = "rhigeo"
rhigeo_00002$PFT = "rhigeo"
rhigeo_00003$PFT = "rhigeo"
rhigeo_00004$PFT = "rhigeo"
rubcha_00001$PFT = "rubcha"
rubcha_00002$PFT = "rubcha"
rubcha_00003$PFT = "rubcha"
rubcha_00004$PFT = "rubcha"
stepas_00001$PFT = "stepas"
stepas_00002$PFT = "stepas"
stepas_00003$PFT = "stepas"
stepas_00004$PFT = "stepas"
stetas_00005$PFT = "stetas"
stetas_00006$PFT = "stetas"
stetas_00007$PFT = "stetas"
stetas_00008$PFT = "stetas"
umbarc_00001$PFT = "umbarc"
umbarc_00002$PFT = "umbarc"
umbarc_00003$PFT = "umbarc"
umbarc_00004$PFT = "umbarc"
vacvit_00001$PFT = "vacvit"
vacvit_00002$PFT = "vacvit"
vacvit_00003$PFT = "vacvit"
vacvit_00004$PFT = "vacvit"
vacvit_00005$PFT = "vacvit"



brooksLib<-rbind (
 arccen_00001
,arccen_00002
,arccen_00003
,arccen_00004
,asachr_00001
,asachr_00002
,asachr_00003
,asachr_00004
,aultur_00001
,aultur_00002
,aultur_00003
,aultur_00004
,betnan_00001
,betnan_00002
,betnan_00003
,betnan_00004
,betnan_00005
,betnan_00006
,betnan_00007
,betnan_00008
,betneo_00001
,betneo_00002
,betneo_00003
,betneo_00004
,cerpur_00005
,cerpur_00001
,cerpur_00002
,cerpur_00003
,cerpur_00004
,clamit_00001
,clamit_00002
,clamit_00003
,clamit_00004
,claste_00001
,claste_00002
,claste_00003
,claste_00004
,claste_00005
,claste_00006
,claste_00007
,claste_00008
,clasty_00001
,clasty_00002
,clasty_00003
,clasty_00004
,clasty_00005
,clasty_00006
,clasty_00007
,clasty_00008
,claunc_00001
,claunc_00002
,claunc_00003
,claunc_00004
,empnig_00001
,empnig_00002
,empnig_00003
,empnig_00004
,erivag_00001
,erivag_00002
,erivag_00003
,erivag_00004
,erivag_00005
,erivag_00006
,erivag_00007
,erivag_00008
,erivag_00009
,erivag_00010
,flacuc_00001
,flacuc_00002
,flacuc_00003
,flacuc_00004
,flaniv_00001
,flaniv_00002
,flaniv_00003
,flaniv_00004
,flaniv_00005
,flaniv_00006
,flaniv_00007
,hypspl_00001
,hypspl_00002
,hypspl_00003
,hypspl_00004
,leddec_00001
,leddec_00002
,leddec_00003
,leddec_00004
,neparc_00001
,neparc_00002
,neparc_00003
,neparc_00004
,paromp_00001
,paromp_00002
,paromp_00003
,paromp_00004
,pelapt_00001
,pelapt_00002
,pelapt_00003
,pelapt_00004
,pelapt_00005
,poljen_00001
,poljen_00002
,poljen_00003
,poljen_00004
,raclan_00001
,raclan_00002
,raclan_00003
,raclan_00004
,rhigeo_00001
,rhigeo_00002
,rhigeo_00003
,rhigeo_00004
,rubcha_00001
,rubcha_00002
,rubcha_00003
,rubcha_00004
,stepas_00001
,stepas_00002
,stepas_00003
,stepas_00004
,stetas_00005
,stetas_00006
,stetas_00007
,stetas_00008
,umbarc_00001
,umbarc_00002
,umbarc_00003
,umbarc_00004
,vacvit_00001
,vacvit_00002
,vacvit_00003
,vacvit_00004
,vacvit_00005
);

setwd("")
##ssag
aultur_00001<-read.table("aultur_00001.sed", header = FALSE, sep="", skip=27)
aultur_00002<-read.table("aultur_00002.sed", header = FALSE, sep="", skip=27)
aultur_00003<-read.table("aultur_00003.sed", header = FALSE, sep="", skip=27)
aultur_00004<-read.table("aultur_00004.sed", header = FALSE, sep="", skip=27)
betnan_00001<-read.table("betann_00001.sed", header = FALSE, sep="", skip=27)
betnan_00002<-read.table("betann_00002.sed", header = FALSE, sep="", skip=27)
betnan_00003<-read.table("betann_00003.sed", header = FALSE, sep="", skip=27)
betnan_00004<-read.table("betann_00004.sed", header = FALSE, sep="", skip=27)
clasty_00001<-read.table("clasty_00001.sed", header = FALSE, sep="", skip=27)
clasty_00002<-read.table("clasty_00002.sed", header = FALSE, sep="", skip=27)
clasty_00003<-read.table("clasty_00003.sed", header = FALSE, sep="", skip=27)
clasty_00004<-read.table("clasty_00004.sed", header = FALSE, sep="", skip=27)
erivag_00001<-read.table("erivag_00001.sed", header = FALSE, sep="", skip=27)
erivag_00002<-read.table("erivag_00002.sed", header = FALSE, sep="", skip=27)
erivag_00003<-read.table("erivag_00003.sed", header = FALSE, sep="", skip=27)
erivag_00004<-read.table("erivag_00004.sed", header = FALSE, sep="", skip=27)
flacuc_00001<-read.table("flacuc_00001.sed", header = FALSE, sep="", skip=27)
flacuc_00002<-read.table("flacuc_00002.sed", header = FALSE, sep="", skip=27)
flacuc_00003<-read.table("flacuc_00003.sed", header = FALSE, sep="", skip=27)
flacuc_00004<-read.table("flacuc_00004.sed", header = FALSE, sep="", skip=27)
hypspl_00001<-read.table("hypspl_00001.sed", header = FALSE, sep="", skip=27)
hypspl_00002<-read.table("hypspl_00002.sed", header = FALSE, sep="", skip=27)
hypspl_00003<-read.table("hypspl_00003.sed", header = FALSE, sep="", skip=27)
hypspl_00004<-read.table("hypspl_00004.sed", header = FALSE, sep="", skip=27)
leddec_00001<-read.table("leddec_00001.sed", header = FALSE, sep="", skip=27)
leddec_00002<-read.table("leddec_00002.sed", header = FALSE, sep="", skip=27)
leddec_00003<-read.table("leddec_00003.sed", header = FALSE, sep="", skip=27)
leddec_00004<-read.table("leddec_00004.sed", header = FALSE, sep="", skip=27)
rubcha_00001<-read.table("rubcha_00001.sed", header = FALSE, sep="", skip=27)
rubcha_00002<-read.table("rubcha_00002.sed", header = FALSE, sep="", skip=27)
rubcha_00003<-read.table("rubcha_00003.sed", header = FALSE, sep="", skip=27)
rubcha_00004<-read.table("rubcha_00004.sed", header = FALSE, sep="", skip=27)
vacvit_00001<-read.table("vacvit_00001.sed", header = FALSE, sep="", skip=27)
vacvit_00002<-read.table("vacvit_00002.sed", header = FALSE, sep="", skip=27)
vacvit_00003<-read.table("vacvit_00003.sed", header = FALSE, sep="", skip=27)
vacvit_00004<-read.table("vacvit_00004.sed", header = FALSE, sep="", skip=27)
vacvit_00005<-read.table("vacvit_00005.sed", header = FALSE, sep="", skip=27)

aultur_00001$site = "ssag"
aultur_00002$site = "ssag"
aultur_00003$site = "ssag"
aultur_00004$site = "ssag"
betnan_00001$site = "ssag"
betnan_00002$site = "ssag"
betnan_00003$site = "ssag"
betnan_00004$site = "ssag"
clasty_00001$site = "ssag"
clasty_00002$site = "ssag"
clasty_00003$site = "ssag"
clasty_00004$site = "ssag"
erivag_00001$site = "ssag"
erivag_00002$site = "ssag"
erivag_00003$site = "ssag"
erivag_00004$site = "ssag"
flacuc_00001$site = "ssag"
flacuc_00002$site = "ssag"
flacuc_00003$site = "ssag"
flacuc_00004$site = "ssag"
hypspl_00001$site = "ssag"
hypspl_00002$site = "ssag"
hypspl_00003$site = "ssag"
hypspl_00004$site = "ssag"
leddec_00001$site = "ssag"
leddec_00002$site = "ssag"
leddec_00003$site = "ssag"
leddec_00004$site = "ssag"
rubcha_00001$site = "ssag"
rubcha_00002$site = "ssag"
rubcha_00003$site = "ssag"
rubcha_00004$site = "ssag"
vacvit_00001$site = "ssag"
vacvit_00002$site = "ssag"
vacvit_00003$site = "ssag"
vacvit_00004$site = "ssag"
vacvit_00005$site = "ssag"

aultur_00001$PFT = "aultur"
aultur_00002$PFT = "aultur"
aultur_00003$PFT = "aultur"
aultur_00004$PFT = "aultur"
betnan_00001$PFT = "betnan"
betnan_00002$PFT = "betnan"
betnan_00003$PFT = "betnan"
betnan_00004$PFT = "betnan"
clasty_00001$PFT = "clasty"
clasty_00002$PFT = "clasty"
clasty_00003$PFT = "clasty"
clasty_00004$PFT = "clasty"
erivag_00001$PFT = "erivag"
erivag_00002$PFT = "erivag"
erivag_00003$PFT = "erivag"
erivag_00004$PFT = "erivag"
flacuc_00001$PFT = "flacuc"
flacuc_00002$PFT = "flacuc"
flacuc_00003$PFT = "flacuc"
flacuc_00004$PFT = "flacuc"
hypspl_00001$PFT = "hypspl"
hypspl_00002$PFT = "hypspl"
hypspl_00003$PFT = "hypspl"
hypspl_00004$PFT = "hypspl"
leddec_00001$PFT = "leddec"
leddec_00002$PFT = "leddec"
leddec_00003$PFT = "leddec"
leddec_00004$PFT = "leddec"
rubcha_00001$PFT = "rubcha"
rubcha_00002$PFT = "rubcha"
rubcha_00003$PFT = "rubcha"
rubcha_00004$PFT = "rubcha"
vacvit_00001$PFT = "vacvit"
vacvit_00002$PFT = "vacvit"
vacvit_00003$PFT = "vacvit"
vacvit_00004$PFT = "vacvit"
vacvit_00005$PFT = "vacvit"

ssagl<-rbind(
aultur_00001
,aultur_00002
,aultur_00003
,aultur_00004
,betnan_00001
,betnan_00002
,betnan_00003
,betnan_00004
,clasty_00001
,clasty_00002
,clasty_00003
,clasty_00004
,erivag_00001
,erivag_00002
,erivag_00003
,erivag_00004
,flacuc_00001
,flacuc_00002
,flacuc_00003
,flacuc_00004
,hypspl_00001
,hypspl_00002
,hypspl_00003
,hypspl_00004
,leddec_00001
,leddec_00002
,leddec_00003
,leddec_00004
,rubcha_00001
,rubcha_00002
,rubcha_00003
,rubcha_00004
,vacvit_00001
,vacvit_00002
,vacvit_00003
,vacvit_00004
,vacvit_00005
);











##atigun

arccen_00001<-read.table("arccen_00001.sed", header = FALSE, sep="", skip=27)
arccen_00002<-read.table("arccen_00002.sed", header = FALSE, sep="", skip=27)
arccen_00003<-read.table("arccen_00003.sed", header = FALSE, sep="", skip=27)
arccen_00004<-read.table("arccen_00004.sed", header = FALSE, sep="", skip=27)
flaniv_00001<-read.table("flaniv_00001.sed", header = FALSE, sep="", skip=27)
flaniv_00002<-read.table("flaniv_00002.sed", header = FALSE, sep="", skip=27)
flaniv_00003<-read.table("flaniv_00003.sed", header = FALSE, sep="", skip=27)
flaniv_00004<-read.table("flaniv_00004.sed", header = FALSE, sep="", skip=27)
claste_00001<-read.table("claste_00001.sed", header = FALSE, sep="", skip=27)
claste_00002<-read.table("claste_00002.sed", header = FALSE, sep="", skip=27)
claste_00003<-read.table("claste_00003.sed", header = FALSE, sep="", skip=27)
claste_00004<-read.table("claste_00004.sed", header = FALSE, sep="", skip=27)
neparc_00001<-read.table("neparc_00001.sed", header = FALSE, sep="", skip=27)
neparc_00002<-read.table("neparc_00002.sed", header = FALSE, sep="", skip=27)
neparc_00003<-read.table("neparc_00003.sed", header = FALSE, sep="", skip=27)
neparc_00004<-read.table("neparc_00004.sed", header = FALSE, sep="", skip=27)
stetas_00001<-read.table("stetas_00001.sed", header = FALSE, sep="", skip=27)
stetas_00002<-read.table("stetas_00002.sed", header = FALSE, sep="", skip=27)
stetas_00003<-read.table("stetas_00003.sed", header = FALSE, sep="", skip=27)
stetas_00004<-read.table("stetas_00004.sed", header = FALSE, sep="", skip=27)
paromp_00001<-read.table("paromp_00001.sed", header = FALSE, sep="", skip=27)
paromp_00002<-read.table("paromp_00002.sed", header = FALSE, sep="", skip=27)
paromp_00003<-read.table("paromp_00003.sed", header = FALSE, sep="", skip=27)
paromp_00004<-read.table("paromp_00004.sed", header = FALSE, sep="", skip=27)

arccen_00001$site = "atigun"
arccen_00002$site = "atigun"
arccen_00003$site = "atigun"
arccen_00004$site = "atigun"
flaniv_00001$site = "atigun"
flaniv_00002$site = "atigun"
flaniv_00003$site = "atigun"
flaniv_00004$site = "atigun"
claste_00001$site = "atigun"
claste_00002$site = "atigun"
claste_00003$site = "atigun"
claste_00004$site = "atigun"
neparc_00001$site = "atigun"
neparc_00002$site = "atigun"
neparc_00003$site = "atigun"
neparc_00004$site = "atigun"
stetas_00001$site = "atigun"
stetas_00002$site = "atigun"
stetas_00003$site = "atigun"
stetas_00004$site = "atigun"
paromp_00001$site = "atigun"
paromp_00002$site = "atigun"
paromp_00003$site = "atigun"
paromp_00004$site = "atigun"


arccen_00001$PFT = "arccen"
arccen_00002$PFT = "arccen"
arccen_00003$PFT = "arccen"
arccen_00004$PFT = "arccen"
flaniv_00001$PFT = "flaniv"
flaniv_00002$PFT = "flaniv"
flaniv_00003$PFT = "flaniv"
flaniv_00004$PFT = "flaniv"
claste_00001$PFT = "claste"
claste_00002$PFT = "claste"
claste_00003$PFT = "claste"
claste_00004$PFT = "claste"
neparc_00001$PFT = "neparc"
neparc_00002$PFT = "neparc"
neparc_00003$PFT = "neparc"
neparc_00004$PFT = "neparc"
stetas_00001$PFT = "stetas"
stetas_00002$PFT = "stetas"
stetas_00003$PFT = "stetas"
stetas_00004$PFT = "stetas"
paromp_00001$PFT = "paromp"
paromp_00002$PFT = "paromp"
paromp_00003$PFT = "paromp"
paromp_00004$PFT = "paromp"


atigunl<-rbind(
 arccen_00001
,arccen_00002
,arccen_00003
,arccen_00004
,flaniv_00001
,flaniv_00002
,flaniv_00003
,flaniv_00004
,claste_00001
,claste_00002
,claste_00003
,claste_00004
,neparc_00001
,neparc_00002
,neparc_00003
,neparc_00004
,stetas_00001
,stetas_00002
,stetas_00003
,stetas_00004
,paromp_00001
,paromp_00002
,paromp_00003
,paromp_00004
);




p<-ggplot(atigunl_summ,aes(Band, Refl))

p+geom_line(aes(color=PFT)








##gobblersknob

asachr_00001<-read.table("asachr_00001.sed", header = FALSE, sep="", skip=27)
asachr_00002<-read.table("asachr_00002.sed", header = FALSE, sep="", skip=27)
asachr_00003<-read.table("asachr_00003.sed", header = FALSE, sep="", skip=27)
asachr_00004<-read.table("asachr_00004.sed", header = FALSE, sep="", skip=27)
umbarc_00001<-read.table("umbarc_00001.sed", header = FALSE, sep="", skip=27)
umbarc_00002<-read.table("umbarc_00002.sed", header = FALSE, sep="", skip=27)
umbarc_00003<-read.table("umbarc_00003.sed", header = FALSE, sep="", skip=27)
umbarc_00004<-read.table("umbarc_00004.sed", header = FALSE, sep="", skip=27)
empnig_00001<-read.table("empnig_00001.sed", header = FALSE, sep="", skip=27)
empnig_00002<-read.table("empnig_00002.sed", header = FALSE, sep="", skip=27)
empnig_00003<-read.table("empnig_00003.sed", header = FALSE, sep="", skip=27)
empnig_00004<-read.table("empnig_00004.sed", header = FALSE, sep="", skip=27)
raclan_00001<-read.table("raclan_00001.sed", header = FALSE, sep="", skip=27)
raclan_00002<-read.table("raclan_00002.sed", header = FALSE, sep="", skip=27)
raclan_00003<-read.table("raclan_00003.sed", header = FALSE, sep="", skip=27)
raclan_00004<-read.table("raclan_00004.sed", header = FALSE, sep="", skip=27)
poljen_00001<-read.table("poljen_00001.sed", header = FALSE, sep="", skip=27)
poljen_00002<-read.table("poljen_00002.sed", header = FALSE, sep="", skip=27)
poljen_00003<-read.table("poljen_00003.sed", header = FALSE, sep="", skip=27)
poljen_00004<-read.table("poljen_00004.sed", header = FALSE, sep="", skip=27)
rhigeo_00001<-read.table("rhigeo_00001.sed", header = FALSE, sep="", skip=27)
rhigeo_00002<-read.table("rhigeo_00002.sed", header = FALSE, sep="", skip=27)
rhigeo_00003<-read.table("rhigeo_00003.sed", header = FALSE, sep="", skip=27)
rhigeo_00004<-read.table("rhigeo_00004.sed", header = FALSE, sep="", skip=27)
cerpur_00001<-read.table("cerpur_00001.sed", header = FALSE, sep="", skip=27)
cerpur_00002<-read.table("cerpur_00002.sed", header = FALSE, sep="", skip=27)
cerpur_00003<-read.table("cerpur_00003.sed", header = FALSE, sep="", skip=27)
cerpur_00004<-read.table("cerpur_00004.sed", header = FALSE, sep="", skip=27)
betneo_00001<-read.table("betneo_00001.sed", header = FALSE, sep="", skip=27)
betneo_00002<-read.table("betneo_00002.sed", header = FALSE, sep="", skip=27)
betneo_00003<-read.table("betneo_00003.sed", header = FALSE, sep="", skip=27)
betneo_00004<-read.table("betneo_00004.sed", header = FALSE, sep="", skip=27)
betnan_00001<-read.table("betnan_00001.sed", header = FALSE, sep="", skip=27)
betnan_00002<-read.table("betnan_00002.sed", header = FALSE, sep="", skip=27)
betnan_00003<-read.table("betnan_00003.sed", header = FALSE, sep="", skip=27)
betnan_00004<-read.table("betnan_00004.sed", header = FALSE, sep="", skip=27)
clamit_00001<-read.table("clamit_00001.sed", header = FALSE, sep="", skip=27)
clamit_00002<-read.table("clamit_00002.sed", header = FALSE, sep="", skip=27)
clamit_00003<-read.table("clamit_00003.sed", header = FALSE, sep="", skip=27)
clamit_00004<-read.table("clamit_00004.sed", header = FALSE, sep="", skip=27)
pelapt_00001<-read.table("pelapt_00001.sed", header = FALSE, sep="", skip=27)
pelapt_00002<-read.table("pelapt_00002.sed", header = FALSE, sep="", skip=27)
pelapt_00003<-read.table("pelapt_00003.sed", header = FALSE, sep="", skip=27)
pelapt_00004<-read.table("pelapt_00004.sed", header = FALSE, sep="", skip=27)
pelapt_00005<-read.table("pelapt_00005.sed", header = FALSE, sep="", skip=27)
clasty_00001<-read.table("clasty2_00001.sed", header = FALSE, sep="", skip=27)
clasty_00002<-read.table("clasty2_00002.sed", header = FALSE, sep="", skip=27)
clasty_00003<-read.table("clasty2_00003.sed", header = FALSE, sep="", skip=27)
clasty_00004<-read.table("clasty2_00004.sed", header = FALSE, sep="", skip=27)
claunc_00001<-read.table("claunc_00001.sed", header = FALSE, sep="", skip=27)
claunc_00002<-read.table("claunc_00002.sed", header = FALSE, sep="", skip=27)
claunc_00003<-read.table("claunc_00003.sed", header = FALSE, sep="", skip=27)
claunc_00004<-read.table("claunc_00004.sed", header = FALSE, sep="", skip=27)
stepas_00001<-read.table("stepas2_00001.sed", header = FALSE, sep="", skip=27)
stepas_00002<-read.table("stepas2_00002.sed", header = FALSE, sep="", skip=27)
stepas_00003<-read.table("stepas2_00003.sed", header = FALSE, sep="", skip=27)
stepas_00004<-read.table("stepas2_00004.sed", header = FALSE, sep="", skip=27)
flaniv_00001<-read.table("flaniv2_00001.sed", header = FALSE, sep="", skip=27)
flaniv_00002<-read.table("flaniv2_00002.sed", header = FALSE, sep="", skip=27)
flaniv_00003<-read.table("flaniv2_00003.sed", header = FALSE, sep="", skip=27)
flaniv_00004<-read.table("flaniv2_00004.sed", header = FALSE, sep="", skip=27)
claste_00001<-read.table("claste2_00001.sed", header = FALSE, sep="", skip=27)
claste_00002<-read.table("claste2_00002.sed", header = FALSE, sep="", skip=27)
claste_00003<-read.table("claste2_00003.sed", header = FALSE, sep="", skip=27)
claste_00004<-read.table("claste2_00004.sed", header = FALSE, sep="", skip=27)

asachr_00001$site = "gobblersknob"
asachr_00002$site = "gobblersknob"
asachr_00003$site = "gobblersknob"
asachr_00004$site = "gobblersknob"
umbarc_00001$site = "gobblersknob"
umbarc_00002$site = "gobblersknob"
umbarc_00003$site = "gobblersknob"
umbarc_00004$site = "gobblersknob"
empnig_00001$site = "gobblersknob"
empnig_00002$site = "gobblersknob"
empnig_00003$site = "gobblersknob"
empnig_00004$site = "gobblersknob"
raclan_00001$site = "gobblersknob"
raclan_00002$site = "gobblersknob"
raclan_00003$site = "gobblersknob"
raclan_00004$site = "gobblersknob"
poljen_00001$site = "gobblersknob"
poljen_00002$site = "gobblersknob"
poljen_00003$site = "gobblersknob"
poljen_00004$site = "gobblersknob"
rhigeo_00001$site = "gobblersknob"
rhigeo_00002$site = "gobblersknob"
rhigeo_00003$site = "gobblersknob"
rhigeo_00004$site = "gobblersknob"
cerpur_00001$site = "gobblersknob"
cerpur_00002$site = "gobblersknob"
cerpur_00003$site = "gobblersknob"
cerpur_00004$site = "gobblersknob"
betneo_00001$site = "gobblersknob"
betneo_00002$site = "gobblersknob"
betneo_00003$site = "gobblersknob"
betneo_00004$site = "gobblersknob"
betnan_00001$site = "gobblersknob"
betnan_00002$site = "gobblersknob"
betnan_00003$site = "gobblersknob"
betnan_00004$site = "gobblersknob"
clamit_00001$site = "gobblersknob"
clamit_00002$site = "gobblersknob"
clamit_00003$site = "gobblersknob"
clamit_00004$site = "gobblersknob"
pelapt_00001$site = "gobblersknob"
pelapt_00002$site = "gobblersknob"
pelapt_00003$site = "gobblersknob"
pelapt_00004$site = "gobblersknob"
pelapt_00005$site = "gobblersknob"
clasty_00001$site = "gobblersknob"
clasty_00002$site = "gobblersknob"
clasty_00003$site = "gobblersknob"
clasty_00004$site = "gobblersknob"
claunc_00001$site = "gobblersknob"
claunc_00002$site = "gobblersknob"
claunc_00003$site = "gobblersknob"
claunc_00004$site = "gobblersknob"
stepas_00001$site = "gobblersknob"
stepas_00002$site = "gobblersknob"
stepas_00003$site = "gobblersknob"
stepas_00004$site = "gobblersknob"
flaniv_00001$site = "gobblersknob"
flaniv_00002$site = "gobblersknob"
flaniv_00003$site = "gobblersknob"
flaniv_00004$site = "gobblersknob"
claste_00001$site = "gobblersknob"
claste_00002$site = "gobblersknob"
claste_00003$site = "gobblersknob"
claste_00004$site = "gobblersknob"


asachr_00001$PFT = "asachr"
asachr_00002$PFT = "asachr"
asachr_00003$PFT = "asachr"
asachr_00004$PFT = "asachr"
umbarc_00001$PFT = "umbarc"
umbarc_00002$PFT = "umbarc"
umbarc_00003$PFT = "umbarc"
umbarc_00004$PFT = "umbarc"
empnig_00001$PFT = "empnig"
empnig_00002$PFT = "empnig"
empnig_00003$PFT = "empnig"
empnig_00004$PFT = "empnig"
raclan_00001$PFT = "raclan"
raclan_00002$PFT = "raclan"
raclan_00003$PFT = "raclan"
raclan_00004$PFT = "raclan"
poljen_00001$PFT = "poljen"
poljen_00002$PFT = "poljen"
poljen_00003$PFT = "poljen"
poljen_00004$PFT = "poljen"
rhigeo_00001$PFT = "rhigeo"
rhigeo_00002$PFT = "rhigeo"
rhigeo_00003$PFT = "rhigeo"
rhigeo_00004$PFT = "rhigeo"
cerpur_00001$PFT = "cerpur"
cerpur_00002$PFT = "cerpur"
cerpur_00003$PFT = "cerpur"
cerpur_00004$PFT = "cerpur"
betneo_00001$PFT = "betneo"
betneo_00002$PFT = "betneo"
betneo_00003$PFT = "betneo"
betneo_00004$PFT = "betneo"
betnan_00001$PFT = "betnan"
betnan_00002$PFT = "betnan"
betnan_00003$PFT = "betnan"
betnan_00004$PFT = "betnan"
clamit_00001$PFT = "clamit"
clamit_00002$PFT = "clamit"
clamit_00003$PFT = "clamit"
clamit_00004$PFT = "clamit"
pelapt_00001$PFT = "pelapt"
pelapt_00002$PFT = "pelapt"
pelapt_00003$PFT = "pelapt"
pelapt_00004$PFT = "pelapt"
pelapt_00005$PFT = "pelapt"
clasty_00001$PFT = "clasty"
clasty_00002$PFT = "clasty"
clasty_00003$PFT = "clasty"
clasty_00004$PFT = "clasty"
claunc_00001$PFT = "claunc"
claunc_00002$PFT = "claunc"
claunc_00003$PFT = "claunc"
claunc_00004$PFT = "claunc"
stepas_00001$PFT = "stepas"
stepas_00002$PFT = "stepas"
stepas_00003$PFT = "stepas"
stepas_00004$PFT = "stepas"
flaniv_00001$PFT = "flaniv"
flaniv_00002$PFT = "flaniv"
flaniv_00003$PFT = "flaniv"
flaniv_00004$PFT = "flaniv"
claste_00001$PFT = "claste"
claste_00002$PFT = "claste"
claste_00003$PFT = "claste"
claste_00004$PFT = "claste"

gobblersknobl<-rbind(
 asachr_00001
,asachr_00002
,asachr_00003
,asachr_00004
,umbarc_00001
,umbarc_00002
,umbarc_00003
,umbarc_00004
,empnig_00001
,empnig_00002
,empnig_00003
,empnig_00004
,raclan_00001
,raclan_00002
,raclan_00003
,raclan_00004
,poljen_00001
,poljen_00002
,poljen_00003
,poljen_00004
,rhigeo_00001
,rhigeo_00002
,rhigeo_00003
,rhigeo_00004
,cerpur_00001
,cerpur_00002
,cerpur_00003
,cerpur_00004
,betneo_00001
,betneo_00002
,betneo_00003
,betneo_00004
,betnan_00001
,betnan_00002
,betnan_00003
,betnan_00004
,clamit_00001
,clamit_00002
,clamit_00003
,clamit_00004
,pelapt_00001
,pelapt_00002
,pelapt_00003
,pelapt_00004
,pelapt_00005
,clasty_00001
,clasty_00002
,clasty_00003
,clasty_00004
,claunc_00001
,claunc_00002
,claunc_00003
,claunc_00004
,stepas_00001
,stepas_00002
,stepas_00003
,stepas_00004
,flaniv_00001
,flaniv_00002
,flaniv_00003
,flaniv_00004
,claste_00001
,claste_00002
,claste_00003
,claste_00004
);

colnames(brooksLib)<-c("Band","Refl", "PFT")
colnames(yKDeltLib)<-c("Band","Refl", "PFT")
colnames(bethelLib)<-c("Band","Refl", "PFT")

alaskaSpecLib<-rbind (
brooksLib
,yKDeltLib
,bethelLib
);

##group by band and PFT
alaskaSpecLib_summ<-alaskaSpecLib %>% group_by(Band, PFT);

##filter out by species (PFT) and create object
alaskaSpecLib_claste<-filter(alaskaSpecLib_summ, PFT == "claste")
alaskaSpecLib_clasty<-filter(alaskaSpecLib_summ, PFT == "clasty")
alaskaSpecLib_salala<-filter(alaskaSpecLib_summ, PFT == "salala")
alaskaSpecLib_salgla<-filter(alaskaSpecLib_summ, PFT == "salgla")

##Create object of median of species 
alaskaSpecLib_claste_median<-alaskaSpecLib_claste%>% summarise(median(Refl))
alaskaSpecLib_clasty_median<-alaskaSpecLib_clasty%>% summarise(median(Refl))
alaskaSpecLib_salala_median<-alaskaSpecLib_salala%>% summarise(median(Refl))
alaskaSpecLib_salgla_median<-alaskaSpecLib_salgla%>% summarise(median(Refl))

##MinRefl
alaskaSpecLib_claste_min<-alaskaSpecLib_claste%>% summarise(min(Refl))
alaskaSpecLib_clasty_min<-alaskaSpecLib_clasty%>% summarise(min(Refl))
alaskaSpecLib_salala_min<-alaskaSpecLib_salala%>% summarise(min(Refl))
alaskaSpecLib_salgla_min<-alaskaSpecLib_salgla%>% summarise(min(Refl))

##maxRefl
alaskaSpecLib_claste_max<-alaskaSpecLib_claste%>% summarise(max(Refl))
alaskaSpecLib_clasty_max<-alaskaSpecLib_clasty%>% summarise(max(Refl))
alaskaSpecLib_salala_max<-alaskaSpecLib_salala%>% summarise(max(Refl))
alaskaSpecLib_salgla_max<-alaskaSpecLib_salgla%>% summarise(max(Refl))

##Merge?
alaskaSpecLib_claste_final<-merge(alaskaSpecLib_claste_median, alaskaSpecLib_claste_min);
alaskaSpecLib_claste_final<-merge(alaskaSpecLib_claste_final, alaskaSpecLib_claste_max);

alaskaSpecLib_clasty_final<-merge(alaskaSpecLib_clasty_median, alaskaSpecLib_clasty_min);
alaskaSpecLib_clasty_final<-merge(alaskaSpecLib_clasty_final, alaskaSpecLib_clasty_max);

alaskaSpecLib_salala_final<-merge(alaskaSpecLib_salala_median, alaskaSpecLib_salala_min);
alaskaSpecLib_salala_final<-merge(alaskaSpecLib_salala_final, alaskaSpecLib_salala_max);

alaskaSpecLib_salgla_final<-merge(alaskaSpecLib_salgla_median, alaskaSpecLib_salgla_min);
alaskaSpecLib_salgla_final<-merge(alaskaSpecLib_salgla_final, alaskaSpecLib_salgla_max);

##renameColums to proper name
colnames(alaskaSpecLib_claste_final)<-c("Band", "PFT","Median", "Minimum", "Maximum")
colnames(alaskaSpecLib_clasty_final)<-c("Band", "PFT","Median", "Minimum", "Maximum")
colnames(alaskaSpecLib_salala_final)<-c("Band", "PFT","Median", "Minimum", "Maximum")
colnames(alaskaSpecLib_salgla_final)<-c("Band", "PFT","Median", "Minimum", "Maximum")

##bind by column bound all  colums including repeats, fix.
##alaskaSpecLib_claste_final<-cbind(
##alaskaSpecLib_claste_median
##,alaskaSpecLib_claste_min
##,alaskaSpecLib_claste_max
##);
##alaskaSpecLib_clasty_final<-cbind(
##alaskaSpecLib_clasty_median
##,alaskaSpecLib_clasty_min
##,alaskaSpecLib_clasty_max
##);
##alaskaSpecLib_salala_final<-cbind(
##alaskaSpecLib_salala_median
##,alaskaSpecLib_salala_min
##,alaskaSpecLib_salala_max
##);
##alaskaSpecLib_salgla_final<-cbind(
##alaskaSpecLib_salgla_median
##,alaskaSpecLib_salgla_min
##,alaskaSpecLib_salgla_max
##);

library(reshape2)

##melt the df by merging reflectance values
alaskaSpecLib_claste_final.long <- melt(alaskaSpecLib_claste_final, id = "Band", measure = c("Median", "Minimum", "Maximum"))
alaskaSpecLib_clasty_final.long <- melt(alaskaSpecLib_clasty_final, id = "Band", measure = c("Median", "Minimum", "Maximum"))
alaskaSpecLib_salala_final.long <- melt(alaskaSpecLib_salala_final, id = "Band", measure = c("Median", "Minimum", "Maximum"))
alaskaSpecLib_salgla_final.long <- melt(alaskaSpecLib_salgla_final, id = "Band", measure = c("Median", "Minimum", "Maximum"))

##rename Colums
colnames(alaskaSpecLib_claste_final.long) <- c("Band", "Variable", "Reflectance")
colnames(alaskaSpecLib_clasty_final.long) <- c("Band", "Variable", "Reflectance")
colnames(alaskaSpecLib_salala_final.long) <- c("Band", "Variable", "Reflectance")
colnames(alaskaSpecLib_salgla_final.long) <- c("Band", "Variable", "Reflectance")

##Plot Species 
p<-ggplot(alaskaSpecLib_claste_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Claste Spectral Signature")

p<-ggplot(alaskaSpecLib_clasty_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Clasty Spectral Signature")

p<-ggplot(alaskaSpecLib_salala_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Salala Spectral Signature")

p<-ggplot(alaskaSpecLib_salgla_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Salgla Spectral Signature")

##SaveJPEG
jpeg("claste_graph.jpg")
p<-ggplot(alaskaSpecLib_claste_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Claste Spectral Signature")
dev.off();
jpeg("clasty_graph.jpg")
p<-ggplot(alaskaSpecLib_clasty_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Clasty Spectral Signature")
dev.off();
jpeg("salala_graph.jpg")
p<-ggplot(alaskaSpecLib_salala_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Salala Spectral Signature")
dev.off();
jpeg("salgla_graph.jpg")
p<-ggplot(alaskaSpecLib_salgla_final.long, aes(Band, Reflectance, colour = Variable)) + geom_line()
p + labs(title = "Salgla Spectral Signature")
dev.off();

##binding all the sites together with rbind then grouping!!
sites1<-rbind(
atigunl_summ
,ssagl_summ);

sites1_summ<-sites1 %>% group_by(Site, Band, PFT) %>% summarise(mean(Refl))
colnames(sites1_summ)<-c("Site","Band","PFT","Refl")
p<-ggplot(sites1_summ,aes(Band, Refl))

p+geom_line(aes(color=PFT))

p<-ggplot(ssagl_summ,aes(Band, Refl))

p+geom_line(aes(color=PFT))









