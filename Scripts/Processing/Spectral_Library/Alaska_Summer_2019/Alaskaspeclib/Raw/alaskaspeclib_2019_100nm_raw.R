library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

##Reads in spectra for each area sampled in Alaksa 
AK2018_spectra_100nm          <-readRDS("Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_100nm.rds")
bethelLib_spectra_100nm       <-readRDS("Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_100nm.rds")
brooksLib_spectra_100nm       <-readRDS("Processed_spec/Alaska_Summer_2018/brooksLib/brooksLib_spectra_100nm.rds")
Murph2_spectra_100nm          <-readRDS("Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_100nm.rds")
Murph_lib_spectra_100nm       <-readRDS("Processed_spec/Alaska_Summer_2018/Murph_lib/Murph_lib_spectra_100nm.rds")
yKDeltLib_spectra_100nm       <-readRDS("Processed_spec/Alaska_Summer_2018/yKDeltLib/yKDeltLib_spectra_100nm.rds")
TwelveMile_spectra_100nm      <-readRDS("Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_100nm.rds")
Big_Trail_Lake_spectra_100nm  <-readRDS("Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_100nm.rds")
Eagle_summit_spectra_100nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_100nm.rds")
Murphy_domeA_spectra_100nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeA/Murphy_domeA_spectra_100nm.rds")
Murphy_domeB_spectra_100nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_100nm.rds")
Wickersham_domeA_spectra_100nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeA/Wickersham_domeA_spectra_100nm.rds")
Wickersham_domeB_spectra_100nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_100nm.rds")

##Combines Spectral Datasets for all areas sampled
alaskaSpecLib_100nm<-Reduce(spectrolab::combine,list(AK2018_spectra_100nm           
                                              ,bethelLib_spectra_100nm        
                                              ,brooksLib_spectra_100nm        
                                              ,Murph2_spectra_100nm          
                                              ,Murph_lib_spectra_100nm       
                                              ,yKDeltLib_spectra_100nm       
                                              ,TwelveMile_spectra_100nm      
                                              ,Big_Trail_Lake_spectra_100nm  
                                              ,Eagle_summit_spectra_100nm    
                                              ,Murphy_domeA_spectra_100nm    
                                              ,Murphy_domeB_spectra_100nm    
                                              ,Wickersham_domeA_spectra_100nm
                                              ,Wickersham_domeB_spectra_100nm))

##convert spectral library to dataframe
alaskaSpecLib_100nm<-as.data.frame(alaskaSpecLib_100nm)

##Removes sample_ame column because it is redundant 
alaskaSpecLib_100nm$sample_name<-NULL

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_100nm$PFT_2[alaskaSpecLib_100nm$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="aultur"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="betneo"]<-"Tree"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="claama"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="claran"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="claste"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="equarv"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="herlan"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="irisit"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="luparc"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="masric"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="petfri"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="picmar"]<-"Tree"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="plesch"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="poljen"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="poljun"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="polstr"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="popbal"]<-"Tree"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="raclan"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="salala"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="salova"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="salric"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_100nm$PFT_3[alaskaSpecLib_100nm$PFT=="wooly_salix"]<-"Shrub"

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_100nm_freqTab<-as.data.frame(table(alaskaSpecLib_100nm$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_100nm$Freq<-alaskaSpecLib_100nm_freqTab$Freq[match(alaskaSpecLib_100nm$PFT_2,alaskaSpecLib_100nm_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_100nm<-alaskaSpecLib_100nm%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Plants only
alaskaSpecLib_100nm_plants<-subset(alaskaSpecLib_100nm,PFT_3!="Abiotic")

##Creates dataframes that has all the species with scans greater than specified values (5,10,15,20)
alaskaSpecLib_100nm_plants_more05<-subset(alaskaSpecLib_100nm_plants,Freq>=5)
alaskaSpecLib_100nm_plants_more10<-subset(alaskaSpecLib_100nm_plants,Freq>=10)
alaskaSpecLib_100nm_plants_more15<-subset(alaskaSpecLib_100nm_plants,Freq>=15)
alaskaSpecLib_100nm_plants_more20<-subset(alaskaSpecLib_100nm_plants,Freq>=20)

##creates datafranme with equal amount of scans for each species (10,15,20 scans per species )
alaskaSpecLib_100nm_plants_equal05<-alaskaSpecLib_100nm_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_100nm_plants_equal10<-alaskaSpecLib_100nm_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_100nm_plants_equal15<-alaskaSpecLib_100nm_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_100nm_plants_equal20<-alaskaSpecLib_100nm_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Extracts scans for each life form (lichen, bryophyte, vascular plant) for spectral library
#alaskaSpecLib_100nm_lichen     <-subset(alaskaSpecLib_100nm_plants,PFT_3=="Lichen")
#alaskaSpecLib_100nm_bryo       <-subset(alaskaSpecLib_100nm_plants,PFT_3=="Moss")
#alaskaSpecLib_100nm_lichen_bryo<-subset(alaskaSpecLib_100nm_plants,PFT_3=="Lichen"|PFT_3=="Moss")
#alaskaSpecLib_100nm_vascular   <-subset(alaskaSpecLib_100nm_plants,PFT_3!="Lichen"&PFT_3!="Moss")

###save spectral library 
#write.csv(alaskaSpecLib_100nm            ,"Processed_spec/alaskaSpecLib_100nm/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_2019_all.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_100nm_plants     ,"Processed_spec/alaskaSpecLib_100nm/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_2019_plants.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_100nm_lichen     ,"Processed_spec/alaskaSpecLib_100nm/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_2019_lichen.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_100nm_bryo       ,"Processed_spec/alaskaSpecLib_100nm/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_2019_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_100nm_lichen_bryo,"Processed_spec/alaskaSpecLib_100nm/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_2019_lichen_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_100nm_vascular   ,"Processed_spec/alaskaSpecLib_100nm/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_2019_vascular.csv",row.names = FALSE)

write.csv(alaskaSpecLib_100nm_plants_more05 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_more05.csv")
write.csv(alaskaSpecLib_100nm_plants_more10 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_more10.csv")
write.csv(alaskaSpecLib_100nm_plants_more15 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_more15.csv")
write.csv(alaskaSpecLib_100nm_plants_more20 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_more20.csv")
write.csv(alaskaSpecLib_100nm_plants_equal05,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_equal05.csv")
write.csv(alaskaSpecLib_100nm_plants_equal10,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_equal10.csv")
write.csv(alaskaSpecLib_100nm_plants_equal15,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_equal15.csv")
write.csv(alaskaSpecLib_100nm_plants_equal20,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_100nm_plants_equal20.csv")
