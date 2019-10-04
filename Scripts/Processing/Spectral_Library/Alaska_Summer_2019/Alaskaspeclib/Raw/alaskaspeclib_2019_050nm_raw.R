library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

##Reads in spectra for each area sampled in Alaksa 
AK2018_spectra_050nm          <-readRDS("Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_050nm.rds")
bethelLib_spectra_050nm       <-readRDS("Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_050nm.rds")
brooksLib_spectra_050nm       <-readRDS("Processed_spec/Alaska_Summer_2018/brooksLib/brooksLib_spectra_050nm.rds")
Murph2_spectra_050nm          <-readRDS("Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_050nm.rds")
Murph_lib_spectra_050nm       <-readRDS("Processed_spec/Alaska_Summer_2018/Murph_lib/Murph_lib_spectra_050nm.rds")
yKDeltLib_spectra_050nm       <-readRDS("Processed_spec/Alaska_Summer_2018/yKDeltLib/yKDeltLib_spectra_050nm.rds")
TwelveMile_spectra_050nm      <-readRDS("Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_050nm.rds")
Big_Trail_Lake_spectra_050nm  <-readRDS("Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_050nm.rds")
Eagle_summit_spectra_050nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_050nm.rds")
Murphy_domeA_spectra_050nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeA/Murphy_domeA_spectra_050nm.rds")
Murphy_domeB_spectra_050nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_050nm.rds")
Wickersham_domeA_spectra_050nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeA/Wickersham_domeA_spectra_050nm.rds")
Wickersham_domeB_spectra_050nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_050nm.rds")

##Combines Spectral Datasets for all areas sampled
alaskaSpecLib_050nm<-Reduce(spectrolab::combine,list(AK2018_spectra_050nm           
                                              ,bethelLib_spectra_050nm        
                                              ,brooksLib_spectra_050nm        
                                              ,Murph2_spectra_050nm          
                                              ,Murph_lib_spectra_050nm       
                                              ,yKDeltLib_spectra_050nm       
                                              ,TwelveMile_spectra_050nm      
                                              ,Big_Trail_Lake_spectra_050nm  
                                              ,Eagle_summit_spectra_050nm    
                                              ,Murphy_domeA_spectra_050nm    
                                              ,Murphy_domeB_spectra_050nm    
                                              ,Wickersham_domeA_spectra_050nm
                                              ,Wickersham_domeB_spectra_050nm))

##convert spectral library to dataframe
alaskaSpecLib_050nm<-as.data.frame(alaskaSpecLib_050nm)

##Removes sample_ame column because it is redundant 
alaskaSpecLib_050nm$sample_name<-NULL

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_050nm$PFT_2[alaskaSpecLib_050nm$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="aultur"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="betneo"]<-"Tree"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="claama"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="claran"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="claste"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="equarv"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="herlan"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="irisit"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="luparc"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="masric"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="petfri"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="picmar"]<-"Tree"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="plesch"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="poljen"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="poljun"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="polstr"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="popbal"]<-"Tree"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="raclan"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="salala"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="salova"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="salric"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_050nm$PFT_3[alaskaSpecLib_050nm$PFT=="wooly_salix"]<-"Shrub"

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_050nm_freqTab<-as.data.frame(table(alaskaSpecLib_050nm$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_050nm$Freq<-alaskaSpecLib_050nm_freqTab$Freq[match(alaskaSpecLib_050nm$PFT_2,alaskaSpecLib_050nm_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_050nm<-alaskaSpecLib_050nm%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Plants only
alaskaSpecLib_050nm_plants<-subset(alaskaSpecLib_050nm,PFT_3!="Abiotic")

##Creates dataframes that has all the species with scans greater than specified values (5,10,15,20)
alaskaSpecLib_050nm_plants_more05<-subset(alaskaSpecLib_050nm_plants,Freq>=5)
alaskaSpecLib_050nm_plants_more10<-subset(alaskaSpecLib_050nm_plants,Freq>=10)
alaskaSpecLib_050nm_plants_more15<-subset(alaskaSpecLib_050nm_plants,Freq>=15)
alaskaSpecLib_050nm_plants_more20<-subset(alaskaSpecLib_050nm_plants,Freq>=20)

##creates datafranme with equal amount of scans for each species (10,15,20 scans per species )
alaskaSpecLib_050nm_plants_equal05<-alaskaSpecLib_050nm_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_050nm_plants_equal10<-alaskaSpecLib_050nm_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_050nm_plants_equal15<-alaskaSpecLib_050nm_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_050nm_plants_equal20<-alaskaSpecLib_050nm_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Extracts scans for each life form (lichen, bryophyte, vascular plant) for spectral library
#alaskaSpecLib_050nm_lichen     <-subset(alaskaSpecLib_050nm_plants,PFT_3=="Lichen")
#alaskaSpecLib_050nm_bryo       <-subset(alaskaSpecLib_050nm_plants,PFT_3=="Moss")
#alaskaSpecLib_050nm_lichen_bryo<-subset(alaskaSpecLib_050nm_plants,PFT_3=="Lichen"|PFT_3=="Moss")
#alaskaSpecLib_050nm_vascular   <-subset(alaskaSpecLib_050nm_plants,PFT_3!="Lichen"&PFT_3!="Moss")

###save spectral library 
#write.csv(alaskaSpecLib_050nm            ,"Processed_spec/alaskaSpecLib_050nm/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_2019_all.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_050nm_plants     ,"Processed_spec/alaskaSpecLib_050nm/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_2019_plants.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_050nm_lichen     ,"Processed_spec/alaskaSpecLib_050nm/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_2019_lichen.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_050nm_bryo       ,"Processed_spec/alaskaSpecLib_050nm/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_2019_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_050nm_lichen_bryo,"Processed_spec/alaskaSpecLib_050nm/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_2019_lichen_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_050nm_vascular   ,"Processed_spec/alaskaSpecLib_050nm/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_2019_vascular.csv",row.names = FALSE)

write.csv(alaskaSpecLib_050nm_plants_more05 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more05.csv")
write.csv(alaskaSpecLib_050nm_plants_more10 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more10.csv")
write.csv(alaskaSpecLib_050nm_plants_more15 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more15.csv")
write.csv(alaskaSpecLib_050nm_plants_more20 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_more20.csv")
write.csv(alaskaSpecLib_050nm_plants_equal05,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal05.csv")
write.csv(alaskaSpecLib_050nm_plants_equal10,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal10.csv")
write.csv(alaskaSpecLib_050nm_plants_equal15,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal15.csv")
write.csv(alaskaSpecLib_050nm_plants_equal20,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_050nm_plants_equal20.csv")
