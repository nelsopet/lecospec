library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

##Reads in spectra for each area sampled in Alaksa 
AK2018_spectra_smooth          <-readRDS("Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_smooth.rds")
bethelLib_spectra_smooth       <-readRDS("Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_smooth.rds")
brooksLib_spectra_smooth       <-readRDS("Processed_spec/Alaska_Summer_2018/brooksLib/brooksLib_spectra_smooth.rds")
Murph2_spectra_smooth          <-readRDS("Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_smooth.rds")
Murph_lib_spectra_smooth       <-readRDS("Processed_spec/Alaska_Summer_2018/Murph_lib/Murph_lib_spectra_smooth.rds")
yKDeltLib_spectra_smooth       <-readRDS("Processed_spec/Alaska_Summer_2018/yKDeltLib/yKDeltLib_spectra_smooth.rds")
TwelveMile_spectra_smooth      <-readRDS("Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_smooth.rds")
Big_Trail_Lake_spectra_smooth  <-readRDS("Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_smooth.rds")
Eagle_summit_spectra_smooth    <-readRDS("Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_smooth.rds")
Murphy_domeA_spectra_smooth    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeA/Murphy_domeA_spectra_smooth.rds")
Murphy_domeB_spectra_smooth    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_smooth.rds")
Wickersham_domeA_spectra_smooth<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeA/Wickersham_domeA_spectra_smooth.rds")
Wickersham_domeB_spectra_smooth<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_smooth.rds")

##Combines Spectral Datasets for all areas sampled
alaskaSpecLib_smooth<-Reduce(spectrolab::combine,list(AK2018_spectra_smooth           
                                              ,bethelLib_spectra_smooth        
                                              ,brooksLib_spectra_smooth        
                                              ,Murph2_spectra_smooth          
                                              ,Murph_lib_spectra_smooth       
                                              ,yKDeltLib_spectra_smooth       
                                              ,TwelveMile_spectra_smooth      
                                              ,Big_Trail_Lake_spectra_smooth  
                                              ,Eagle_summit_spectra_smooth    
                                              ,Murphy_domeA_spectra_smooth    
                                              ,Murphy_domeB_spectra_smooth    
                                              ,Wickersham_domeA_spectra_smooth
                                              ,Wickersham_domeB_spectra_smooth))

##convert spectral library to dataframe
alaskaSpecLib_smooth<-as.data.frame(alaskaSpecLib_smooth)

##Removes sample_ame column because it is redundant 
alaskaSpecLib_smooth$sample_name<-NULL

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_smooth$PFT_2[alaskaSpecLib_smooth$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="aultur"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="betneo"]<-"Tree"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="claama"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="claran"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="claste"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="equarv"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="herlan"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="irisit"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="luparc"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="masric"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="petfri"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="picmar"]<-"Tree"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="plesch"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="poljen"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="poljun"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="polstr"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="popbal"]<-"Tree"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="raclan"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="salala"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="salova"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="salric"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_smooth$PFT_3[alaskaSpecLib_smooth$PFT=="wooly_salix"]<-"Shrub"

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_smooth_freqTab<-as.data.frame(table(alaskaSpecLib_smooth$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_smooth$Freq<-alaskaSpecLib_smooth_freqTab$Freq[match(alaskaSpecLib_smooth$PFT_2,alaskaSpecLib_smooth_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_smooth<-alaskaSpecLib_smooth%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Plants only
alaskaSpecLib_smooth_plants<-subset(alaskaSpecLib_smooth,PFT_3!="Abiotic")

##Creates dataframes that has all the species with scans greater than specified values (5,10,15,20)
alaskaSpecLib_smooth_plants_more05<-subset(alaskaSpecLib_smooth_plants,Freq>=5)
alaskaSpecLib_smooth_plants_more10<-subset(alaskaSpecLib_smooth_plants,Freq>=10)
alaskaSpecLib_smooth_plants_more15<-subset(alaskaSpecLib_smooth_plants,Freq>=15)
alaskaSpecLib_smooth_plants_more20<-subset(alaskaSpecLib_smooth_plants,Freq>=20)

##creates datafranme with equal amount of scans for each species (10,15,20 scans per species )
alaskaSpecLib_smooth_plants_equal05<-alaskaSpecLib_smooth_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_smooth_plants_equal10<-alaskaSpecLib_smooth_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_smooth_plants_equal15<-alaskaSpecLib_smooth_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_smooth_plants_equal20<-alaskaSpecLib_smooth_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Extracts scans for each life form (lichen, bryophyte, vascular plant) for spectral library
#alaskaSpecLib_smooth_lichen     <-subset(alaskaSpecLib_smooth_plants,PFT_3=="Lichen")
#alaskaSpecLib_smooth_bryo       <-subset(alaskaSpecLib_smooth_plants,PFT_3=="Moss")
#alaskaSpecLib_smooth_lichen_bryo<-subset(alaskaSpecLib_smooth_plants,PFT_3=="Lichen"|PFT_3=="Moss")
#alaskaSpecLib_smooth_vascular   <-subset(alaskaSpecLib_smooth_plants,PFT_3!="Lichen"&PFT_3!="Moss")

###save spectral library 
#write.csv(alaskaSpecLib_smooth            ,"Processed_spec/alaskaSpecLib_smooth/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_2019_all.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_plants     ,"Processed_spec/alaskaSpecLib_smooth/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_2019_plants.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_lichen     ,"Processed_spec/alaskaSpecLib_smooth/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_2019_lichen.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_bryo       ,"Processed_spec/alaskaSpecLib_smooth/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_2019_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_lichen_bryo,"Processed_spec/alaskaSpecLib_smooth/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_2019_lichen_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_smooth_vascular   ,"Processed_spec/alaskaSpecLib_smooth/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_2019_vascular.csv",row.names = FALSE)

write.csv(alaskaSpecLib_smooth_plants_more05 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_more05.csv")
write.csv(alaskaSpecLib_smooth_plants_more10 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_more10.csv")
write.csv(alaskaSpecLib_smooth_plants_more15 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_more15.csv")
write.csv(alaskaSpecLib_smooth_plants_more20 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_more20.csv")
write.csv(alaskaSpecLib_smooth_plants_equal05,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_equal05.csv")
write.csv(alaskaSpecLib_smooth_plants_equal10,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_equal10.csv")
write.csv(alaskaSpecLib_smooth_plants_equal15,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_equal15.csv")
write.csv(alaskaSpecLib_smooth_plants_equal20,"Processed_spec/AlaskaSpecLib/Spectral_Library/Smooth/2019/alaskaSpecLib_smooth_plants_equal20.csv")
