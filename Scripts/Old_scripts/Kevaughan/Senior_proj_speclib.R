library(spectrolab)
library(tidyverse)
library(hsdar)
#setwd("/Alaska_Spectral_Library")

##Reads in spectra for each area sampled in Alaksa (from the year 2018-2019)
AK2018_spectra          <-readRDS("Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra.rds")
bethelLib_spectra       <-readRDS("Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra.rds")
brooksLib_spectra       <-readRDS("Processed_spec/Alaska_Summer_2018/brooksLib/brooksLib_spectra.rds")
Murph2_spectra          <-readRDS("Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra.rds")
Murph_lib_spectra       <-readRDS("Processed_spec/Alaska_Summer_2018/Murph_lib/Murph_lib_spectra.rds")
yKDeltLib_spectra       <-readRDS("Processed_spec/Alaska_Summer_2018/yKDeltLib/yKDeltLib_spectra.rds")
TwelveMile_spectra      <-readRDS("Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra.rds")
Big_Trail_Lake_spectra  <-readRDS("Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra.rds")
Eagle_summit_spectra    <-readRDS("Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra.rds")
Murphy_domeA_spectra    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeA/Murphy_domeA_spectra.rds")
Murphy_domeB_spectra    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra.rds")
Wickersham_domeA_spectra<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeA/Wickersham_domeA_spectra.rds")
Wickersham_domeB_spectra<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra.rds")
Howland_spectra         <-readRDS("Processed_spec/Maine_Summer_2019/Howland_spectra.rds")
Pef_spectra             <-readRDS("Processed_spec/Maine_Summer_2019/Pef_spectra.rds")

##Combines Spectral Datasets for all areas sampled....spectral object with all the scans collected, 1985 samples, 2151 bands 
alaskaSpecLib_rds<-Reduce(spectrolab::combine,list(AK2018_spectra          
                                                   ,bethelLib_spectra       
                                                   ,brooksLib_spectra       
                                                   ,Murph2_spectra          
                                                   ,Murph_lib_spectra       
                                                   ,yKDeltLib_spectra       
                                                   ,TwelveMile_spectra      
                                                   ,Big_Trail_Lake_spectra  
                                                   ,Eagle_summit_spectra    
                                                   ,Murphy_domeA_spectra    
                                                   ,Murphy_domeB_spectra    
                                                   ,Wickersham_domeA_spectra
                                                   ,Wickersham_domeB_spectra
                                                   ,Howland_spectra
                                                   ,Pef_spectra))

#Convert spectral library to dataframe....dataframe with 2468 rows and 2154 cols. 
alaskaSpecLib<-as.data.frame(alaskaSpecLib_rds)

##Removes sample_ame column because it is redundant 
alaskaSpecLib$sample_name<-NULL

##Now we want to add columns that represent the species and funcional groups, PFT_2 and PFT_3 respectively. 
##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="abibal"]<-"Abies balsamea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="acerub"]<-"Acer rubrum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="acepen"]<-"Acer pensylvanicum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="alninc"]<-"Alnus Incana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arctop"]<-"Unknown"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betall"]<-"Betula alleghaniensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betpap"]<-"Betula papyrifera"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betpop"]<-"Betula populifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carram"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="faggra"]<-"Fagus grandifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="fraame"]<-"Fraxinus americana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="gravel"]<-"Gravel"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="larlar"]<-"Larix Larcina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="picrub"]<-"Picea rubens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="pinstr"]<-"Pinus strobus"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="popgra"]<-"Populus grandidentata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="prupen"]<-"Prunus pensylvanica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="quartz"]<-"Quartz"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="querub"]<-"Quercus Rubra"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhutyp"]<-"Rhus typhina"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="thuocc"]<-"Thuja occidentalis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tsucan"]<-"Tsuga canadensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="abibal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="acerub"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="acepen"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alninc"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arccen"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="arctop"]<-"Unknown"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="asachr"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aulpal"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aultur"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betall"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betnan"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betneo"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betpap"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betpop"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carram"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cerpur"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claama"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clacor"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clagra"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clamit"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claran"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claste"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clasty"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="clasul"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="claunc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dicranum"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equarv"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equsyl"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="evemes"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="faggra"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="fraame"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="herlan"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hylspl"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypspl"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="irisit"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="larlar"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="leddec"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="luparc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="masric"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melhep"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="neparc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="naparc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="paramb"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="paromp"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="parsul"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pedrac"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pedsud"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="petfri"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="picmar"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="picrub"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="pinstr"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popgra"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="prupen"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plesch"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljen"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljun"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polstr"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="querub"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="raclan"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhutyp"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosaci"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosasc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salala"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salarb"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salgla"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sallan"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salova"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="Salova"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salpul"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salric"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphagn"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphfus"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stepas"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stetas"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="thuocc"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tomnit"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tragra"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tsucan"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vaculi"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="wooly_salix"]<-"Shrub"

###Now we want to add a frequency column, this frequency value represents the number of scans per species
##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_freqTab<-as.data.frame(table(alaskaSpecLib$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib$Freq<-alaskaSpecLib_freqTab$Freq[match(alaskaSpecLib$PFT_2,alaskaSpecLib_freqTab$Var1)]

##Reorder columns
alaskaSpecLib<-alaskaSpecLib%>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Just plants....1959 rows and 2157 columns
alaskaSpecLib_plants     <-subset(alaskaSpecLib,PFT_3!="Abiotic")
alaskaSpecLib_plants     <-subset(alaskaSpecLib_plants,PFT_3!="Unknown")


##Now we want to remove all the bad scans...1954 ROWS AND 2157 COLUMNS 
##This means the rannge of the columns that reflect reflectance values should be <2
##First we need to check the range of these columns
##alaskaSpecLib_plants[-1:-6]%>%range()%>%View()
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1891'<2)    
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1892'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1893'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1894'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1895'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1896'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1897'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1898'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1899'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1900'<2) 
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1902'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1908'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1909'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1916'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1917'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1930'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1931'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1932'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1938'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1939'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1940'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1947'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1948'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1980'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1981'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1982'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1990'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1991'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1992'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1993'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1997'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1998'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'1999'<2)
alaskaSpecLib_plants<-subset(alaskaSpecLib_plants, alaskaSpecLib_plants$'2500'<2)

##lets check the range of these columns again
##alaskaSpecLib_plants[7:2157]%>%range()%>%View()
##once range is below 2 then you can proceed

##We want to create dataframes that have all the scans of each functional groups, this can be used to make graphs of all the species within each functional group
##Creates a subset of each of the functional groups 

alaskaSpecLib_Lichen    <-subset(alaskaSpecLib_plants,PFT_3=="Lichen")
alaskaSpecLib_Tree      <-subset(alaskaSpecLib_plants,PFT_3=="Tree")
alaskaSpecLib_Dwarfshrub<-subset(alaskaSpecLib_plants,PFT_3=="Dwarf Shrub")
alaskaSpecLib_shrub     <-subset(alaskaSpecLib_plants,PFT_3=="Shrub")
alaskaSpecLib_Moss      <-subset(alaskaSpecLib_plants,PFT_3=="Moss")
alaskaSpecLib_Graminoid <-subset(alaskaSpecLib_plants,PFT_3=="Graminoid")
alaskaSpecLib_Forb      <-subset(alaskaSpecLib_plants,PFT_3=="Forb")

##Now we want to convert our new spectral library back to a spectral object to be saved later
#First Remove metadata from spectral library
alaskaSpecLib_meta<-alaskaSpecLib_plants[,c(1:6)]

###Create alskaspeclib without meta
alaskaSpecLib_plants_spec<-alaskaSpecLib_plants[,c(-1:-6)]

##convert to a .rds file
alaskaSpecLib_rds<-spectrolab::as.spectra(alaskaSpecLib_plants_spec)
str(alaskaSpecLib_rds)

##bind metadata...final producct is a spectral object with 2400 sample and 2151 bands with 6 ,metadata
meta(alaskaSpecLib_rds)<-data.frame(alaskaSpecLib_meta, stringsAsFactors = FALSE)


###Now we want to make new variables, i.e....smooth data, resampled every 5nm,smooth data resampled every 5nm, PCA, Vegitation Indices
##Resample every 5nm
alaskaSpecLib_rds_005nm = spectrolab::resample(alaskaSpecLib_rds, seq(350, 2500, 5))

##Smoothdata
alaskaSpecLib_rds_smooth     <-smooth(alaskaSpecLib_rds)
alaskaSpecLib_rds_smooth_05nm<-smooth(alaskaSpecLib_rds_005nm)

##convert to a dataframe to be saved for models later
alaskaSpecLib_plants_smooth     <-as.data.frame(alaskaSpecLib_rds_smooth     )
alaskaSpecLib_plants_smooth_05nm<-as.data.frame(alaskaSpecLib_rds_smooth_05nm)

#First Remove metadata from spectral library for smooth
alaskaSpecLib_meta_smooth     <-alaskaSpecLib_plants_smooth     [,c(1:7)]
alaskaSpecLib_meta_smooth_05nm<-alaskaSpecLib_plants_smooth_05nm[,c(1:7)]

###Create alskaspeclib without meta for smooth dataset
alaskaSpecLib_plants_smooth_spec     <-alaskaSpecLib_plants_smooth     [,c(-1:-7)]
alaskaSpecLib_plants_smooth_05nm_spec<-alaskaSpecLib_plants_smooth_05nm[,c(-1:-7)]

##convert to a .rds file for smooth dataset
alaskaSpecLib_smooth_rds     <-spectrolab::as.spectra(alaskaSpecLib_plants_smooth_spec)
alaskaSpecLib_smooth_05nm_rds<-spectrolab::as.spectra(alaskaSpecLib_plants_smooth_05nm_spec)

##bind metadata
meta(alaskaSpecLib_smooth_rds     )<-data.frame(alaskaSpecLib_meta_smooth     , stringsAsFactors = FALSE)
meta(alaskaSpecLib_smooth_05nm_rds)<-data.frame(alaskaSpecLib_meta_smooth_05nm, stringsAsFactors = FALSE)


##PCA
##Now we want to convert our spectral library back to a adataframe so we can do PCA Calculations
alaskaSpecLib_plants_pca<-as.data.frame(alaskaSpecLib_rds)

## Removes unwanted metadata from dataframes to be used for PCA Calculation later
drop<-c("ScanID", "PFT","PFT_2", "Freq", "area","sample_name")
alaskaSpecLib_plants_pca            <-alaskaSpecLib_plants_pca        [,!(names(alaskaSpecLib_plants_pca        ) %in% drop)]
alaskaSpecLib_plants_pca_smooth     <-alaskaSpecLib_plants_smooth     [,!(names(alaskaSpecLib_plants_smooth     ) %in% drop)]
alaskaSpecLib_plants_pca_smooth_05nm<-alaskaSpecLib_plants_smooth_05nm[,!(names(alaskaSpecLib_plants_smooth_05nm) %in% drop)]

#Converts species column to a factor
alaskaSpecLib_plants_pca             $PFT_3<-as.factor(alaskaSpecLib_plants_pca             $PFT_3)
alaskaSpecLib_plants_pca_smooth      $PFT_3<-as.factor(alaskaSpecLib_plants_pca_smooth      $PFT_3)
alaskaSpecLib_plants_pca_smooth_05nm $PFT_3<-as.factor(alaskaSpecLib_plants_pca_smooth_05nm $PFT_3)

##Creates PCA (all bands)
PCA_preds <- prcomp(alaskaSpecLib_plants_pca [,-1], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants <- round(PCA_preds$sdev^2/sum(PCA_preds$sdev^2)*100)

PCA_preds_smooth <- prcomp(alaskaSpecLib_plants_pca_smooth [,-1], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants_smooth <- round(PCA_preds_smooth$sdev^2/sum(PCA_preds_smooth$sdev^2)*100)

PCA_preds_smooth_05nm <- prcomp(alaskaSpecLib_plants_pca_smooth_05nm [,-1], retx=TRUE, center=TRUE, scale=TRUE)
expl.var_plants_smooth_05nm <- round(PCA_preds_smooth_05nm$sdev^2/sum(PCA_preds_smooth_05nm$sdev^2)*100)

####fist 6 PC's
pc6<-c(1:6)

#creates dataframe with first 6 PCA vales for each species
names_species     <-as.data.frame(as.character(alaskaSpecLib_plants_pca $PFT_3))
PCA_preds <-as.data.frame(cbind(names_species ,PCA_preds $x[,pc6]))
names(PCA_preds )[1]<-paste("PFT_3")

names_species_smooth     <-as.data.frame(as.character(alaskaSpecLib_plants_pca_smooth $PFT_3))
PCA_preds_smooth <-as.data.frame(cbind(names_species_smooth ,PCA_preds_smooth $x[,pc6]))
names(PCA_preds_smooth )[1]<-paste("PFT_3")

names_species_smooth_05nm     <-as.data.frame(as.character(alaskaSpecLib_plants_pca_smooth_05nm $PFT_3))
PCA_preds_smooth_05nm <-as.data.frame(cbind(names_species_smooth_05nm ,PCA_preds_smooth_05nm $x[,pc6]))
names(PCA_preds_smooth_05nm)[1]<-paste("PFT_3")


##Vegitation Indices
##creates a vector of wavelengths ... a list of numbers 2151 entries long
alaskaSpecLib_wavelength           <-alaskaSpecLib_rds$wavelengths
alaskaSpecLib_wavelength_smooth    <-alaskaSpecLib_rds_smooth$wavelengths     
alaskaSpecLib_wavelength_smooth_5nm<-alaskaSpecLib_rds_smooth_05nm$wavelengths

##convert alaskaSpeclib_spec to matrix...it has to be a matrix to be converted to a speclib object
alaskaSpecLib_matrix            <-as.matrix(alaskaSpecLib_plants_spec            )
alaskaSpecLib_matrix_smooth     <-as.matrix(alaskaSpecLib_plants_smooth_spec     )
alaskaSpecLib_matrix_smooth_05nm<-as.matrix(alaskaSpecLib_plants_smooth_05nm_spec)

##Creates a spectralib object ... 13 slots
alaskaSpecLib_speclib            <-speclib(alaskaSpecLib_matrix            ,alaskaSpecLib_wavelength           )
alaskaSpecLib_speclib_smooth     <-speclib(alaskaSpecLib_matrix_smooth     ,alaskaSpecLib_wavelength_smooth    )
alaskaSpecLib_speclib_smooth_05nm<-speclib(alaskaSpecLib_matrix_smooth_05nm,alaskaSpecLib_wavelength_smooth_5nm)

##creates a vectror of names of all the vegitation indices...there are 115 of these
VIs<-vegindex()

##alaskaSpecLib_mREIP<-vegindex(alaskaSpecLib_lib,index = "mREIP"        )%>%as.data.frame()%>%`colnames<-`(c("mREIP"        ))
##Vegitation indices mREIP won't work so remove it from list
VIs<-VIs[-58]

##Creates dataframe with Vegitation indices as variables ..dataframe of 1901 rows by 114 variables, each of which is a veg index
alaskaSpecLib_VIs            <-vegindex(alaskaSpecLib_speclib            , index = VIs)
alaskaSpecLib_VIs_smooth     <-vegindex(alaskaSpecLib_speclib_smooth     , index = VIs)
alaskaSpecLib_VIs_smooth_05nm<-vegindex(alaskaSpecLib_speclib_smooth_05nm, index = VIs)

##You'll need to duplicate object above to be used later
alaskaSpecLib_VIs_OLD            <-vegindex(alaskaSpecLib_speclib            , index = VIs)
alaskaSpecLib_VIs_OLD_smooth     <-vegindex(alaskaSpecLib_speclib_smooth     , index = VIs)
alaskaSpecLib_VIs_OLD_smooth_05nm<-vegindex(alaskaSpecLib_speclib_smooth_05nm, index = VIs)

## check to see if NaNs/Inf exist ... NaNs produced in vegindex stuff ... check to see if input to vegindex has weird values anyways
lapply(alaskaSpecLib_VIs            , range)
lapply(alaskaSpecLib_VIs_smooth     , range)
lapply(alaskaSpecLib_VIs_smooth_05nm, range)

##REMOVE THOSE NaNs/Inf 
  ## These steps would be place to look at which scans are being removed and why which awould be indications 
  ##of possible errors in preceeding steps
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(NDVI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(TCARI2)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(TCARI2/OSAVI2)==F)
names(alaskaSpecLib_VIs)[101]<-"SWIRFI"##had to remove space within this column name
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(SWIRFI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(PRI_norm)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(`PRI*CI2`)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(PRI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(NDNI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(NDLI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(Datt4)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.na(CI)==F)

alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(SRPI)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(Vogelmann)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(SR7)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(SR5)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(SR2)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(PARS)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(GDVI_4)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(DWSI1)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(Datt6)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(Datt2)==F)
alaskaSpecLib_VIs<-subset(alaskaSpecLib_VIs, is.infinite(D2)==F)

##REMOVE THOSE NaNs/Inf 
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(NDVI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(TCARI2)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(TCARI2/OSAVI2)==F)
names(alaskaSpecLib_VIs_smooth)[101]<-"SWIRFI"##had to remove space withi
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(SWIRFI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(PRI_norm)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(`PRI*CI2`)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(PRI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(NDNI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(NDLI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(Datt4)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(CI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(`MCARI/OSAVI`)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(Datt7)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(`TCARI/OSAVI`)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.na(`MCARI/OSAVI`)==F)

alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(SRPI)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(Vogelmann)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(SR7)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(SR5)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(SR2)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(PARS)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(GDVI_4)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(DWSI1)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(Datt6)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(Datt2)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(D2)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(Datt8)==F)
alaskaSpecLib_VIs_smooth<-subset(alaskaSpecLib_VIs_smooth, is.infinite(PRI_norm)==F)
                          
##REMOVE THOSE NaNs/Inf 
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(NDVI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(TCARI2)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(TCARI2/OSAVI2)==F)
names(alaskaSpecLib_VIs_smooth_05nm)[101]<-"SWIRFI"##had to remove space withi
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(SWIRFI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(PRI_norm)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(`PRI*CI2`)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(PRI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(NDNI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(NDLI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(Datt4)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(CI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(`MCARI/OSAVI`)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(Datt7)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(`TCARI/OSAVI`)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.na(`MCARI/OSAVI`)==F)


alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(SRPI)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(Vogelmann)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(SR7)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(SR5)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(SR2)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(PARS)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(GDVI_4)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(DWSI1)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(Datt6)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(Datt2)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(D2)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(Datt8)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(PRI_norm)==F)
alaskaSpecLib_VIs_smooth_05nm<-subset(alaskaSpecLib_VIs_smooth_05nm, is.infinite(Datt)==F)

## check to see if NaNs/Inf exist ... NaNs produced in vegindex stuff ... check to see if input to vegindex has weird values anyways
lapply(alaskaSpecLib_VIs            , range)
lapply(alaskaSpecLib_VIs_smooth     , range)
lapply(alaskaSpecLib_VIs_smooth_05nm, range)

##Combines metadta with new variables to be used in models...use duplicated dataframe, this way we could do a join later
alaskaSpecLib_VIs_OLD            <-cbind(alaskaSpecLib_meta,alaskaSpecLib_VIs_OLD)
alaskaSpecLib_VIs_OLD_smooth     <-cbind(alaskaSpecLib_meta,alaskaSpecLib_VIs_OLD_smooth)
alaskaSpecLib_VIs_OLD_smooth_05nm<-cbind(alaskaSpecLib_meta,alaskaSpecLib_VIs_OLD_smooth_05nm)

##Join old VIs data frame with new VIs dataframe...this way all the rows will line up with the corrent species
  ## This joins the VIs object with 2395  114 dimensions with VIs old with 2400 120 dim
  ## resulting in an object with 2395 121 dim, SWIR FI is being repeated, with one with a space in it

alaskaSpecLib_VIs_NEW            <-left_join(alaskaSpecLib_VIs,alaskaSpecLib_VIs_OLD)
alaskaSpecLib_VIs_smooth_NEW     <-left_join(alaskaSpecLib_VIs_smooth,alaskaSpecLib_VIs_OLD_smooth)
alaskaSpecLib_VIs_smooth_05nm_NEW<-left_join(alaskaSpecLib_VIs_smooth_05nm,alaskaSpecLib_VIs_OLD_smooth_05nm)


##Reorder columns
alaskaSpecLib_VIs_NEW_PFT2            <-alaskaSpecLib_VIs_NEW                %>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,area,Freq,everything())
alaskaSpecLib_VIs_smooth_NEW_PFT2     <-alaskaSpecLib_VIs_smooth_NEW         %>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,area,Freq,everything())
alaskaSpecLib_VIs_smooth_05nm_NEW_PFT2<-alaskaSpecLib_VIs_smooth_05nm_NEW    %>%dplyr::select(ScanID,PFT,PFT_2,PFT_3,area,Freq,everything())

##Change column names so they have no spaces or arithmetic operators
colnames(alaskaSpecLib_VIs_NEW_PFT2)[8:121]<-c("Boochs"     ,   "Boochs2"   ,   "CAI"          , "CARI"   ,       "Carter"        ,"Carter2"  ,     "Carter3"   ,   
                                               "Carter4"    ,   "Carter5"   ,   "Carter6"      , "CI"     ,       "CI2"           ,"ClAInt"   ,     "CRI1"      ,   
                                               "CRI2"       ,   "CRI3"      ,   "CRI4"         , "D1"     ,       "D2"            ,"Datt"     ,     "Datt2"     ,   
                                               "Datt3"      ,   "Datt4"     ,   "Datt5"        , "Datt6"  ,       "Datt7"         ,"Datt8"    ,     "DD"        ,   
                                               "DDn"        ,   "DPI"       ,   "DWSI1"        , "DWSI2"  ,       "DWSI3"         ,"DWSI4"    ,     "DWSI5"     ,   
                                               "EGFN"       ,   "EGFR"      ,   "EVI"          , "GDVI_2" ,       "GDVI_3"        ,"GDVI_4"   ,     "GI"        ,   
                                               "Gitelson"   ,   "Gitelson2" ,   "GMI1"         , "GMI2"   ,       "GreenNDVI"     ,"LWVI1"    ,     "LWVI2"     ,   
                                               "Maccioni"   ,   "MCARI"     ,   "MCARIOSAVI"   , "MCARI2" ,       "MCARI2OSAVI2"  ,"mND705"   ,     "mNDVI"     ,   
                                               "MPRI"       ,   "MSAVI"     ,   "MSI"          , "mSR"    ,       "mSR2"          ,"mSR705"   ,     "MTCI"      ,   
                                               "MTVI"       ,   "NDLI"      ,   "NDNI"         , "NDVI"   ,       "NDVI2"         ,"NDVI3"    ,     "NDWI"      ,   
                                               "NPCI"       ,   "OSAVI"     ,   "OSAVI2"       , "PARS"   ,       "PRI"           ,"PRICI2"   ,     "PRI_norm"  ,   
                                               "PSND"       ,   "PSRI"      ,   "PSSR"         , "PWI"    ,       "RDVI"          ,"REP_LE"   ,     "REP_Li"    ,   
                                               "SAVI"       ,   "SIPI"      ,   "SPVI"         , "SR"     ,       "SR1"           ,"SR2"      ,     "SR3"       ,   
                                               "SR4"        ,   "SR5"       ,   "SR6"          , "SR7"    ,       "SR8"           ,"SRPI"     ,     "SRWI"      ,   
                                               "Sum_Dr1"    ,   "Sum_Dr2"   ,   "SWIRFI"       , "SWIRLI" ,       "SWIRSI"        ,"SWIRVI"   ,     "TCARI"     ,   
                                               "TCARIOSAVI" ,   "TCARI2"    ,   "TCARI2OSAVI2" , "TGI"    ,       "TVI"           ,"Vogelmann",     "Vogelmann2",   
                                               "Vogelmann3" ,   "Vogelmann4")

colnames(alaskaSpecLib_VIs_smooth_NEW_PFT2)[8:121]<-c("Boochs"     ,   "Boochs2"   ,   "CAI"          , "CARI"   ,       "Carter"        ,"Carter2"  ,     "Carter3"   ,   
                                                      "Carter4"    ,   "Carter5"   ,   "Carter6"      , "CI"     ,       "CI2"           ,"ClAInt"   ,     "CRI1"      ,   
                                                      "CRI2"       ,   "CRI3"      ,   "CRI4"         , "D1"     ,       "D2"            ,"Datt"     ,     "Datt2"     ,   
                                                      "Datt3"      ,   "Datt4"     ,   "Datt5"        , "Datt6"  ,       "Datt7"         ,"Datt8"    ,     "DD"        ,   
                                                      "DDn"        ,   "DPI"       ,   "DWSI1"        , "DWSI2"  ,       "DWSI3"         ,"DWSI4"    ,     "DWSI5"     ,   
                                                      "EGFN"       ,   "EGFR"      ,   "EVI"          , "GDVI_2" ,       "GDVI_3"        ,"GDVI_4"   ,     "GI"        ,   
                                                      "Gitelson"   ,   "Gitelson2" ,   "GMI1"         , "GMI2"   ,       "GreenNDVI"     ,"LWVI1"    ,     "LWVI2"     ,   
                                                      "Maccioni"   ,   "MCARI"     ,   "MCARIOSAVI"   , "MCARI2" ,       "MCARI2OSAVI2"  ,"mND705"   ,     "mNDVI"     ,   
                                                      "MPRI"       ,   "MSAVI"     ,   "MSI"          , "mSR"    ,       "mSR2"          ,"mSR705"   ,     "MTCI"      ,   
                                                      "MTVI"       ,   "NDLI"      ,   "NDNI"         , "NDVI"   ,       "NDVI2"         ,"NDVI3"    ,     "NDWI"      ,   
                                                      "NPCI"       ,   "OSAVI"     ,   "OSAVI2"       , "PARS"   ,       "PRI"           ,"PRICI2"   ,     "PRI_norm"  ,   
                                                      "PSND"       ,   "PSRI"      ,   "PSSR"         , "PWI"    ,       "RDVI"          ,"REP_LE"   ,     "REP_Li"    ,   
                                                      "SAVI"       ,   "SIPI"      ,   "SPVI"         , "SR"     ,       "SR1"           ,"SR2"      ,     "SR3"       ,   
                                                      "SR4"        ,   "SR5"       ,   "SR6"          , "SR7"    ,       "SR8"           ,"SRPI"     ,     "SRWI"      ,   
                                                      "Sum_Dr1"    ,   "Sum_Dr2"   ,   "SWIRFI"       , "SWIRLI" ,       "SWIRSI"        ,"SWIRVI"   ,     "TCARI"     ,   
                                                      "TCARIOSAVI" ,   "TCARI2"    ,   "TCARI2OSAVI2" , "TGI"    ,       "TVI"           ,"Vogelmann",     "Vogelmann2",   
                                                      "Vogelmann3" ,   "Vogelmann4")

colnames(alaskaSpecLib_VIs_smooth_05nm_NEW_PFT2)[8:121]<-c("Boochs"     ,   "Boochs2"   ,   "CAI"          , "CARI"   ,       "Carter"        ,"Carter2"  ,     "Carter3"   ,   
                                                           "Carter4"    ,   "Carter5"   ,   "Carter6"      , "CI"     ,       "CI2"           ,"ClAInt"   ,     "CRI1"      ,   
                                                           "CRI2"       ,   "CRI3"      ,   "CRI4"         , "D1"     ,       "D2"            ,"Datt"     ,     "Datt2"     ,   
                                                           "Datt3"      ,   "Datt4"     ,   "Datt5"        , "Datt6"  ,       "Datt7"         ,"Datt8"    ,     "DD"        ,   
                                                           "DDn"        ,   "DPI"       ,   "DWSI1"        , "DWSI2"  ,       "DWSI3"         ,"DWSI4"    ,     "DWSI5"     ,   
                                                           "EGFN"       ,   "EGFR"      ,   "EVI"          , "GDVI_2" ,       "GDVI_3"        ,"GDVI_4"   ,     "GI"        ,   
                                                           "Gitelson"   ,   "Gitelson2" ,   "GMI1"         , "GMI2"   ,       "GreenNDVI"     ,"LWVI1"    ,     "LWVI2"     ,   
                                                           "Maccioni"   ,   "MCARI"     ,   "MCARIOSAVI"   , "MCARI2" ,       "MCARI2OSAVI2"  ,"mND705"   ,     "mNDVI"     ,   
                                                           "MPRI"       ,   "MSAVI"     ,   "MSI"          , "mSR"    ,       "mSR2"          ,"mSR705"   ,     "MTCI"      ,   
                                                           "MTVI"       ,   "NDLI"      ,   "NDNI"         , "NDVI"   ,       "NDVI2"         ,"NDVI3"    ,     "NDWI"      ,   
                                                           "NPCI"       ,   "OSAVI"     ,   "OSAVI2"       , "PARS"   ,       "PRI"           ,"PRICI2"   ,     "PRI_norm"  ,   
                                                           "PSND"       ,   "PSRI"      ,   "PSSR"         , "PWI"    ,       "RDVI"          ,"REP_LE"   ,     "REP_Li"    ,   
                                                           "SAVI"       ,   "SIPI"      ,   "SPVI"         , "SR"     ,       "SR1"           ,"SR2"      ,     "SR3"       ,   
                                                           "SR4"        ,   "SR5"       ,   "SR6"          , "SR7"    ,       "SR8"           ,"SRPI"     ,     "SRWI"      ,   
                                                           "Sum_Dr1"    ,   "Sum_Dr2"   ,   "SWIRFI"       , "SWIRLI" ,       "SWIRSI"        ,"SWIRVI"   ,     "TCARI"     ,   
                                                           "TCARIOSAVI" ,   "TCARI2"    ,   "TCARI2OSAVI2" , "TGI"    ,       "TVI"           ,"Vogelmann",     "Vogelmann2",   
                                                           "Vogelmann3" ,   "Vogelmann4")

##Remove last column since its repeated
  ## This may not be needed ... object appears to only have 121 columns
alaskaSpecLib_VIs_NEW_PFT2[122]<-NULL
alaskaSpecLib_VIs_smooth_NEW_PFT2[122]<-NULL
alaskaSpecLib_VIs_smooth_05nm_NEW_PFT2[122]<-NULL

##Now we want to create dataframes with equal amounts of scans per species, these are to be used in models later 
##do this for each pridictors created
##creates datafranme with equal scans for each species (10,15,20 scans per species )
##Smooth
alaskaSpecLib_plants_equal10_smooth<-alaskaSpecLib_plants_smooth %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_plants_equal15_smooth<-alaskaSpecLib_plants_smooth %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_plants_equal20_smooth<-alaskaSpecLib_plants_smooth %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Smooth 5nm
alaskaSpecLib_plants_equal10_5nm_smooth<-alaskaSpecLib_plants_smooth_05nm %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_plants_equal15_5nm_smooth<-alaskaSpecLib_plants_smooth_05nm %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_plants_equal20_5nm_smooth<-alaskaSpecLib_plants_smooth_05nm %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##PCA
PCA_preds_equal10<-PCA_preds %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
PCA_preds_equal15<-PCA_preds %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
PCA_preds_equal20<-PCA_preds %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

PCA_preds_equal10_smooth<-PCA_preds_smooth %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
PCA_preds_equal15_smooth<-PCA_preds_smooth %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
PCA_preds_equal20_smooth<-PCA_preds_smooth %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

PCA_preds_equal10_smooth_05nm<-PCA_preds_smooth_05nm %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
PCA_preds_equal15_smooth_05nm<-PCA_preds_smooth_05nm %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
PCA_preds_equal20_smooth_05nm<-PCA_preds_smooth_05nm %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)


##Vegitation indices
VEG_Index_equal10<-alaskaSpecLib_VIs_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
VEG_Index_equal15<-alaskaSpecLib_VIs_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
VEG_Index_equal20<-alaskaSpecLib_VIs_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

VEG_Index_equal10_smooth<-alaskaSpecLib_VIs_smooth_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
VEG_Index_equal15_smooth<-alaskaSpecLib_VIs_smooth_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
VEG_Index_equal20_smooth<-alaskaSpecLib_VIs_smooth_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

VEG_Index_equal10_smooth_05nm<-alaskaSpecLib_VIs_smooth_05nm_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
VEG_Index_equal15_smooth_05nm<-alaskaSpecLib_VIs_smooth_05nm_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
VEG_Index_equal20_smooth_05nm<-alaskaSpecLib_VIs_smooth_05nm_NEW_PFT2 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)





##Save outputs....these are the list of predictors that can be used in our spectral library
##Original Spectral lIbrary
saveRDS(alaskaSpecLib_rds            ,"Seniorproj_outcomes/alaskaSpecLib_rds.rds"            )
saveRDS(alaskaSpecLib_rds_005nm      ,"Seniorproj_outcomes/alaskaSpecLib_rds_005nm.rds"      )

##Smooth spectral library
saveRDS(alaskaSpecLib_rds_smooth     ,"Seniorproj_outcomes/alaskaSpecLib_rds_smooth.rds"     )
saveRDS(alaskaSpecLib_rds_smooth_05nm,"Seniorproj_outcomes/alaskaSpecLib_rds_smooth_05nm.rds")

##Smooth Predictors
write.csv(alaskaSpecLib_plants_equal10_smooth,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal10_smooth.csv"    , row.names = F)
write.csv(alaskaSpecLib_plants_equal15_smooth,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal15_smooth.csv"    , row.names = F)
write.csv(alaskaSpecLib_plants_equal20_smooth,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal20_smooth.csv"    , row.names = F)

write.csv(alaskaSpecLib_plants_equal10_5nm_smooth,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal10_5nm_smooth.csv"    , row.names = F)
write.csv(alaskaSpecLib_plants_equal15_5nm_smooth,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal15_5nm_smooth.csv"    , row.names = F)
write.csv(alaskaSpecLib_plants_equal20_5nm_smooth,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_plants_equal20_5nm_smooth.csv"    , row.names = F)


##PCA predictors
write.csv(PCA_preds_equal10,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_equal10.csv"    , row.names = F)
write.csv(PCA_preds_equal15,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_equal15.csv"    , row.names = F)
write.csv(PCA_preds_equal20,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_equal20.csv"    , row.names = F)

write.csv(PCA_preds_equal10_smooth,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_smooth_equal10.csv"    , row.names = F)
write.csv(PCA_preds_equal15_smooth,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_smooth_equal15.csv"    , row.names = F)
write.csv(PCA_preds_equal20_smooth,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_smooth_equal20.csv"    , row.names = F)

write.csv(PCA_preds_equal10_smooth_05nm,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_equal10_smooth_05nm.csv"    , row.names = F)
write.csv(PCA_preds_equal15_smooth_05nm,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_equal15_smooth_05nm.csv"    , row.names = F)
write.csv(PCA_preds_equal20_smooth_05nm,"Seniorproj_outcomes/Rawdata/PFT3_PCA_preds_equal20_smooth_05nm.csv"    , row.names = F)

##Vegitation Indices 
write.csv(alaskaSpecLib_VIs_NEW_PFT2        ,"Seniorproj_outcomes/Rawdata/VEG_Index.csv"                     , row.names = F)
write.csv(VEG_Index_equal10                 ,"Seniorproj_outcomes/Rawdata/VEG_Index_equal10.csv"             , row.names = F)
write.csv(VEG_Index_equal15                 ,"Seniorproj_outcomes/Rawdata/VEG_Index_equal15.csv"             , row.names = F)
write.csv(VEG_Index_equal20                 ,"Seniorproj_outcomes/Rawdata/VEG_Index_equal20.csv"             , row.names = F)

write.csv(VEG_Index_equal10_smooth,"Seniorproj_outcomes/Rawdata/VEG_Index_equal10_smooth.csv"    , row.names = F)
write.csv(VEG_Index_equal15_smooth,"Seniorproj_outcomes/Rawdata/VEG_Index_equal15_smooth.csv"    , row.names = F)
write.csv(VEG_Index_equal20_smooth,"Seniorproj_outcomes/Rawdata/VEG_Index_equal20_smooth.csv"    , row.names = F)


write.csv(VEG_Index_equal10_smooth_05nm,"Seniorproj_outcomes/Rawdata/VEG_Index_equal10_smooth_05nm.csv"    , row.names = F)
write.csv(VEG_Index_equal15_smooth_05nm,"Seniorproj_outcomes/Rawdata/VEG_Index_equal15_smooth_05nm.csv"    , row.names = F)
write.csv(VEG_Index_equal20_smooth_05nm,"Seniorproj_outcomes/Rawdata/VEG_Index_equal20_smooth_05nm.csv"    , row.names = F)

##Finctional groups to be used for Graphs
write.csv(alaskaSpecLib_Lichen    ,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_Lichen.csv"    , row.names = F)
write.csv(alaskaSpecLib_Tree      ,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_Tree.csv"      , row.names = F)
write.csv(alaskaSpecLib_Dwarfshrub,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_Dwarfshrub.csv", row.names = F)
write.csv(alaskaSpecLib_shrub     ,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_shrub.csv"     , row.names = F)
write.csv(alaskaSpecLib_Moss      ,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_Moss.csv"      , row.names = F)
write.csv(alaskaSpecLib_Graminoid ,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_Graminoid.csv" , row.names = F)
write.csv(alaskaSpecLib_Forb      ,"Seniorproj_outcomes/Rawdata/alaskaSpecLib_Forb.csv"      , row.names = F)

