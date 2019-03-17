setwd("/Users/peternelson 1/Documents/UMFK/Grants/NASA_ABOVE/Data/AK_Spectral_Library/AK_Spectral_Library/AK Spec Lib/Bethellib")

##bethelLib
arcnig_Beth001<-read.table("arcniglclip_00001.sed", header = FALSE, sep="", skip=27)
arcnig_Beth002<-read.table("arcniglclip_00002.sed", header = FALSE, sep="", skip=27)
arcnig_Beth003<-read.table("arcniglclip_00003.sed", header = FALSE, sep="", skip=27)
arcnig_Beth004<-read.table("arcniglclip_00004.sed", header = FALSE, sep="", skip=27)
arcsta_Beth001<-read.table("arcsta_00001.sed", header = FALSE, sep="", skip=27)
arcsta_Beth002<-read.table("arcsta_00002.sed", header = FALSE, sep="", skip=27)
arcsta_Beth003<-read.table("arcsta_00003.sed", header = FALSE, sep="", skip=27)
arcsta_Beth004<-read.table("arcsta_00004.sed", header = FALSE, sep="", skip=27)
betnan_Beth001<-read.table("betnanlclip_00001.sed", header = FALSE, sep="", skip=27)
betnan_Beth002<-read.table("betnanlclip_00002.sed", header = FALSE, sep="", skip=27)
betnan_Beth003<-read.table("betnanlclip_00003.sed", header = FALSE, sep="", skip=27)
betnan_Beth004<-read.table("betnanlclip_00004.sed", header = FALSE, sep="", skip=27)
claste_Beth001<-read.table("clastecprobe_00001.sed", header = FALSE, sep="", skip=27)
claste_Beth002<-read.table("clastecprobe_00002.sed", header = FALSE, sep="", skip=27)
claste_Beth003<-read.table("clastecprobe_00003.sed", header = FALSE, sep="", skip=27)
claste_Beth004<-read.table("clastecprobe_00004.sed", header = FALSE, sep="", skip=27)
clasty_Beth001<-read.table("clastycprobe_00001.sed", header = FALSE, sep="", skip=27)
clasty_Beth002<-read.table("clastycprobe_00002.sed", header = FALSE, sep="", skip=27)
clasty_Beth003<-read.table("clastycprobe_00003.sed", header = FALSE, sep="", skip=27)
clasty_Beth004<-read.table("clastycprobe_00004.sed", header = FALSE, sep="", skip=27)
empnig_Beth001<-read.table("empnig3_00001.sed", header = FALSE, sep="", skip=27)
empnig_Beth002<-read.table("empnig3_00002.sed", header = FALSE, sep="", skip=27)
empnig_Beth003<-read.table("empnig3_00003.sed", header = FALSE, sep="", skip=27)
empnig_Beth004<-read.table("empnig3_00004.sed", header = FALSE, sep="", skip=27)
empnig_Beth005<-read.table("empniglclip_00001.sed", header = FALSE, sep="", skip=27)
empnig_Beth006<-read.table("empniglclip_00002.sed", header = FALSE, sep="", skip=27)
empnig_Beth007<-read.table("empniglclip_00003.sed", header = FALSE, sep="", skip=27)
empnig_Beth008<-read.table("empniglclip_00004.sed", header = FALSE, sep="", skip=27)
icmeri_Beth002<-read.table("fairypuke_00001.sed", header = FALSE, sep="", skip=27)
icmeri_Beth003<-read.table("fairypuke_00002.sed", header = FALSE, sep="", skip=27)
icmeri_Beth004<-read.table("fairypuke_00003.sed", header = FALSE, sep="", skip=27)
icmeri_Beth005<-read.table("fairypuke_00004.sed", header = FALSE, sep="", skip=27)
icmeri_Beth006<-read.table("iceericprobe_00001.sed", header = FALSE, sep="", skip=27)
icmeri_Beth007<-read.table("iceericprobe_00002.sed", header = FALSE, sep="", skip=27)
icmeri_Beth008<-read.table("iceericprobe_00003.sed", header = FALSE, sep="", skip=27)
icmeri_Beth009<-read.table("iceericprobe_00004.sed", header = FALSE, sep="", skip=27)
leddec_Beth001<-read.table("leddec2lclip_00001.sed", header = FALSE, sep="", skip=27)
leddec_Beth002<-read.table("leddec2lclip_00002.sed", header = FALSE, sep="", skip=27)
leddec_Beth003<-read.table("leddec2lclip_00003.sed", header = FALSE, sep="", skip=27)
leddec_Beth004<-read.table("leddec2lclip_00004.sed", header = FALSE, sep="", skip=27)
rubcam_Beth001<-read.table("rubcam_00001.sed", header = FALSE, sep="", skip=27)
rubcam_Beth002<-read.table("rubcam_00002.sed", header = FALSE, sep="", skip=27)
rubcam_Beth003<-read.table("rubcam_00003.sed", header = FALSE, sep="", skip=27)
rubcam_Beth004<-read.table("rubcam_00004.sed", header = FALSE, sep="", skip=27)
rubcha_Beth001<-read.table("rubcha2lclip_00001.sed", header = FALSE, sep="", skip=27)
rubcha_Beth002<-read.table("rubcha2lclip_00002.sed", header = FALSE, sep="", skip=27)
rubcha_Beth003<-read.table("rubcha2lclip_00003.sed", header = FALSE, sep="", skip=27)
rubcha_Beth004<-read.table("rubcha2lclip_00004.sed", header = FALSE, sep="", skip=27)
sphfus_Beth001<-read.table("sphfuscprobe_00001.sed", header = FALSE, sep="", skip=27)
sphfus_Beth002<-read.table("sphfuscprobe_00002.sed", header = FALSE, sep="", skip=27)
sphfus_Beth003<-read.table("sphfuscprobe_00003.sed", header = FALSE, sep="", skip=27)
sphfus_Beth004<-read.table("sphfuscprobe_00004.sed", header = FALSE, sep="", skip=27)
vacvit_Beth001<-read.table("vacvit2_00001.sed", header = FALSE, sep="", skip=27)
vacvit_Beth002<-read.table("vacvit2_00002.sed", header = FALSE, sep="", skip=27)
vacvit_Beth003<-read.table("vacvit2_00003.sed", header = FALSE, sep="", skip=27)
vacvit_Beth004<-read.table("vacvit2_00004.sed", header = FALSE, sep="", skip=27)

arcnig_Beth001$PFT = "arcnig"
arcnig_Beth002$PFT = "arcnig"
arcnig_Beth003$PFT = "arcnig"
arcnig_Beth004$PFT = "arcnig"
arcsta_Beth001$PFT = "arcsta"
arcsta_Beth002$PFT = "arcsta"
arcsta_Beth003$PFT = "arcsta"
arcsta_Beth004$PFT = "arcsta"
betnan_Beth001$PFT = "betnan"
betnan_Beth002$PFT = "betnan"
betnan_Beth003$PFT = "betnan"
betnan_Beth004$PFT = "betnan"
claste_Beth001$PFT = "claste"
claste_Beth002$PFT = "claste"
claste_Beth003$PFT = "claste"
claste_Beth004$PFT = "claste"
clasty_Beth001$PFT = "clasty"
clasty_Beth002$PFT = "clasty"
clasty_Beth003$PFT = "clasty"
clasty_Beth004$PFT = "clasty"
empnig_Beth001$PFT = "empnig"
empnig_Beth002$PFT = "empnig"
empnig_Beth003$PFT = "empnig"
empnig_Beth004$PFT = "empnig"
empnig_Beth005$PFT = "empnig"
empnig_Beth006$PFT = "empnig"
empnig_Beth007$PFT = "empnig"
empnig_Beth008$PFT = "empnig"
icmeri_Beth002$PFT = "icmeri"
icmeri_Beth003$PFT = "icmeri"
icmeri_Beth004$PFT = "icmeri"
icmeri_Beth005$PFT = "icmeri"
icmeri_Beth006$PFT = "icmeri"
icmeri_Beth007$PFT = "icmeri"
icmeri_Beth008$PFT = "icmeri"
icmeri_Beth009$PFT = "icmeri"
leddec_Beth001$PFT = "leddec"
leddec_Beth002$PFT = "leddec"
leddec_Beth003$PFT = "leddec"
leddec_Beth004$PFT = "leddec"
rubcam_Beth001$PFT = "rubcam"
rubcam_Beth002$PFT = "rubcam"
rubcam_Beth003$PFT = "rubcam"
rubcam_Beth004$PFT = "rubcam"
rubcha_Beth001$PFT = "rubcha"
rubcha_Beth002$PFT = "rubcha"
rubcha_Beth003$PFT = "rubcha"
rubcha_Beth004$PFT = "rubcha"
sphfus_Beth001$PFT = "sphfus"
sphfus_Beth002$PFT = "sphfus"
sphfus_Beth003$PFT = "sphfus"
sphfus_Beth004$PFT = "sphfus"
vacvit_Beth001$PFT = "vacvit"
vacvit_Beth002$PFT = "vacvit"
vacvit_Beth003$PFT = "vacvit"
vacvit_Beth004$PFT = "vacvit"

arcnig_Beth001$ScanID = "arcnig_00001"
arcnig_Beth002$ScanID = "arcnig_00002"
arcnig_Beth003$ScanID = "arcnig_00003"
arcnig_Beth004$ScanID = "arcnig_00004"
arcsta_Beth001$ScanID = "arcsta_00001"
arcsta_Beth002$ScanID = "arcsta_00002"
arcsta_Beth003$ScanID = "arcsta_00003"
arcsta_Beth004$ScanID = "arcsta_00004"
betnan_Beth001$ScanID = "betnan_00001"
betnan_Beth002$ScanID = "betnan_00002"
betnan_Beth003$ScanID = "betnan_00003"
betnan_Beth004$ScanID = "betnan_00004"
claste_Beth001$ScanID = "claste_00001"
claste_Beth002$ScanID = "claste_00002"
claste_Beth003$ScanID = "claste_00003"
claste_Beth004$ScanID = "claste_00004"
clasty_Beth001$ScanID = "clasty_00001"
clasty_Beth002$ScanID = "clasty_00002"
clasty_Beth003$ScanID = "clasty_00003"
clasty_Beth004$ScanID = "clasty_00004"
empnig_Beth001$ScanID = "empnig_00001"
empnig_Beth002$ScanID = "empnig_00002"
empnig_Beth003$ScanID = "empnig_00003"
empnig_Beth004$ScanID = "empnig_00004"
empnig_Beth005$ScanID = "empnig_00005"
empnig_Beth006$ScanID = "empnig_00006"
empnig_Beth007$ScanID = "empnig_00007"
empnig_Beth008$ScanID = "empnig_00008"
icmeri_Beth002$ScanID = "icmeri_00002"
icmeri_Beth003$ScanID = "icmeri_00003"
icmeri_Beth004$ScanID = "icmeri_00004"
icmeri_Beth005$ScanID = "icmeri_00005"
icmeri_Beth006$ScanID = "icmeri_00006"
icmeri_Beth007$ScanID = "icmeri_00007"
icmeri_Beth008$ScanID = "icmeri_00008"
icmeri_Beth009$ScanID = "icmeri_00009"
leddec_Beth001$ScanID = "leddec_00001"
leddec_Beth002$ScanID = "leddec_00002"
leddec_Beth003$ScanID = "leddec_00003"
leddec_Beth004$ScanID = "leddec_00004"
rubcam_Beth001$ScanID = "rubcam_00001"
rubcam_Beth002$ScanID = "rubcam_00002"
rubcam_Beth003$ScanID = "rubcam_00003"
rubcam_Beth004$ScanID = "rubcam_00004"
rubcha_Beth001$ScanID = "rubcha_00001"
rubcha_Beth002$ScanID = "rubcha_00002"
rubcha_Beth003$ScanID = "rubcha_00003"
rubcha_Beth004$ScanID = "rubcha_00004"
sphfus_Beth001$ScanID = "sphfus_00001"
sphfus_Beth002$ScanID = "sphfus_00002"
sphfus_Beth003$ScanID = "sphfus_00003"
sphfus_Beth004$ScanID = "sphfus_00004"
vacvit_Beth001$ScanID = "vacvit_00001"
vacvit_Beth002$ScanID = "vacvit_00002"
vacvit_Beth003$ScanID = "vacvit_00003"
vacvit_Beth004$ScanID = "vacvit_00004"

bethelLib<-rbind(
 arcnig_Beth001
,arcnig_Beth002
,arcnig_Beth003
,arcnig_Beth004
,arcsta_Beth001
,arcsta_Beth002
,arcsta_Beth003
,arcsta_Beth004
,betnan_Beth001
,betnan_Beth002
,betnan_Beth003
,betnan_Beth004
,claste_Beth001
,claste_Beth002
,claste_Beth003
,claste_Beth004
,clasty_Beth001
,clasty_Beth002
,clasty_Beth003
,clasty_Beth004
,empnig_Beth001
,empnig_Beth002
,empnig_Beth003
,empnig_Beth004
,empnig_Beth005
,empnig_Beth006
,empnig_Beth007
,empnig_Beth008
,icmeri_Beth002
,icmeri_Beth003
,icmeri_Beth004
,icmeri_Beth005
,icmeri_Beth006
,icmeri_Beth007
,icmeri_Beth008
,icmeri_Beth009
,leddec_Beth001
,leddec_Beth002
,leddec_Beth003
,leddec_Beth004
,rubcam_Beth001
,rubcam_Beth002
,rubcam_Beth003
,rubcam_Beth004
,rubcha_Beth001
,rubcha_Beth002
,rubcha_Beth003
,rubcha_Beth004
,sphfus_Beth001
,sphfus_Beth002
,sphfus_Beth003
,sphfus_Beth004
,vacvit_Beth001
,vacvit_Beth002
,vacvit_Beth003
,vacvit_Beth004
);


bethelLib$area<- "Bethel"

bethelLib<-rename(bethelLib, Band = V1, Refl = V2)

test<-bethelLib %>% group_by(PFT, Band) %>% ggplot(aes(Band,Refl))+geom_line(aes(color=PFT))+geom_dl(aes(label=ScanID), method="last.points")
test+facet_wrap(facets= vars(PFT), nrow=10, ncol=5)

