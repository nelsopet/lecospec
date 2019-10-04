library(spectrolab)
library(tidyverse)
setwd("/Alaska_Spectral_Library")

##Reads in spectra for each area sampled in Alaksa 
AK2018_spectra_010nm          <-readRDS("Processed_spec/Alaska_Summer_2018/AK2018/AK2018_spectra_010nm.rds")
bethelLib_spectra_010nm       <-readRDS("Processed_spec/Alaska_Summer_2018/bethelLib/bethelLib_spectra_010nm.rds")
brooksLib_spectra_010nm       <-readRDS("Processed_spec/Alaska_Summer_2018/brooksLib/brooksLib_spectra_010nm.rds")
Murph2_spectra_010nm          <-readRDS("Processed_spec/Alaska_Summer_2018/Murph2_Lib/Murph2_spectra_010nm.rds")
Murph_lib_spectra_010nm       <-readRDS("Processed_spec/Alaska_Summer_2018/Murph_lib/Murph_lib_spectra_010nm.rds")
yKDeltLib_spectra_010nm       <-readRDS("Processed_spec/Alaska_Summer_2018/yKDeltLib/yKDeltLib_spectra_010nm.rds")
TwelveMile_spectra_010nm      <-readRDS("Processed_spec/Alaska_Summer_2019/TwelveMile/TwelveMile_spectra_010nm.rds")
Big_Trail_Lake_spectra_010nm  <-readRDS("Processed_spec/Alaska_Summer_2019/Big_Trail_Lake/Big_Trail_Lake_spectra_010nm.rds")
Eagle_summit_spectra_010nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Eagle_summit/Eagle_summit_spectra_010nm.rds")
Murphy_domeA_spectra_010nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeA/Murphy_domeA_spectra_010nm.rds")
Murphy_domeB_spectra_010nm    <-readRDS("Processed_spec/Alaska_Summer_2019/Murphy_domeB/Murphy_domeB_spectra_010nm.rds")
Wickersham_domeA_spectra_010nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeA/Wickersham_domeA_spectra_010nm.rds")
Wickersham_domeB_spectra_010nm<-readRDS("Processed_spec/Alaska_Summer_2019/Wickersham_domeB/Wickersham_domeB_spectra_010nm.rds")

##Combines Spectral Datasets for all areas sampled
alaskaSpecLib_010nm<-Reduce(spectrolab::combine,list(AK2018_spectra_010nm           
                                              ,bethelLib_spectra_010nm        
                                              ,brooksLib_spectra_010nm        
                                              ,Murph2_spectra_010nm          
                                              ,Murph_lib_spectra_010nm       
                                              ,yKDeltLib_spectra_010nm       
                                              ,TwelveMile_spectra_010nm      
                                              ,Big_Trail_Lake_spectra_010nm  
                                              ,Eagle_summit_spectra_010nm    
                                              ,Murphy_domeA_spectra_010nm    
                                              ,Murphy_domeB_spectra_010nm    
                                              ,Wickersham_domeA_spectra_010nm
                                              ,Wickersham_domeB_spectra_010nm))

##convert spectral library to dataframe
alaskaSpecLib_010nm<-as.data.frame(alaskaSpecLib_010nm)

##Removes sample_ame column because it is redundant 
alaskaSpecLib_010nm$sample_name<-NULL

##Add column PFT_2 (SPECIES) to spectral library
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="arcsta"]<-"Arctostaphyllos"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="carram"]<-"Carex sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="castet"]<-"Cassiope tetragona"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="cerpur"]<-"Ceratadon purpureus"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="cetisl"]<-"Cetraria islandica"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="cetlae"]<-"Cetraria laevigata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="claama"]<-"Cladonia amaurocraea"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="clacor"]<-"Cladonia cornuta"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="clacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="clagra"]<-"Cladonia gracilis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="clamit"]<-"Cladonia mitis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="claran"]<-"Cladonia rangiferina"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="claste"]<-"Cladonia steallaris"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="clasty"]<-"Cladonia stygia"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="clasul"]<-"Cladonia sulphurina"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="claunc"]<-"Cladonia uncialis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="dacarc"]<-"Dactylina arctica"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="dryala"]<-"Dryas alleghenies"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="dryhyb"]<-"Dryas sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="dryoct"]<-"Dryas octopetala"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="gravel"]<-"Gravel"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="neparc"]<-"Nephroma arcticum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="naparc"]<-"Nephroma arcticum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="orange_Porpidia"]<-"Porpidia sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="paramb"]<-"Parmeliopsis ambigua"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="paromp"]<-"Parmelia omphalodes"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="parsul"]<-"Parmelis sulcata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pedrac"]<-"Pedicularis racemosa"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pedsud"]<-"Pedicularis sudetica"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pelapt"]<-"Peltigera apthosa"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pelleu"]<-"Peltigers leucophlebia"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pelmal"]<-"Peltigera malacea"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pelsca"]<-"Peltigera scabrata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="petfri"]<-"Petasites frigida"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="picmar"]<-"Picea mariana"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="pilaci"]<-"Pilophorus acicularis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="quartz"]<-"Quartz"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="rosaci"]<-"Rosa acicularis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="Salova"]<-"Salix ovalifolia"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="vaculi"]<-"Vaccinium uliginosum"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib_010nm$PFT_2[alaskaSpecLib_010nm$PFT=="wooly_salix"]<-"Salix (wooly)"

###Add column PFT_3 (Couser response variables)
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="alnfru"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="arccen"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="arcnig"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="arcrub"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="arcsta"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="arctop"]<-"Unknown"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="asachr"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="aulpal"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="aultur"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="bare rock"]<-"Abiotic"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="bare_soil"]<-"Abiotic"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="betnan"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="betneo"]<-"Tree"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="carram"]<-"Graminoid"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="castet"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="cerpur"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="cetisl"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="cetlae"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="claama"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="clacor"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="clacuc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="clagra"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="clamit"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="claran"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="claste"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="clasty"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="clasul"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="claunc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="dacarc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="dicranum"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="dryala"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="dryhyb"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="dryoct"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="equarv"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="equsyl"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="evemes"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="herlan"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="hylspl"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="hypspl"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="irisit"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="leddec"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="luparc"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="masric"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="melhep"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="neparc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="naparc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="orange_Porpidia"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="paramb"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="paromp"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="parsul"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pedrac"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pedsud"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pelapt"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pelleu"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pelmal"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pelsca"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="petfri"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="picmar"]<-"Tree"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="pilaci"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="plesch"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="poljen"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="poljun"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="polstr"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="popbal"]<-"Tree"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="raclan"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="rosasc"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="salala"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="salarb"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="salgla"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="sallan"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="salova"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="Salova"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="salpul"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="salric"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="sphagn"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="sphfus"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="stepas"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="stetas"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="tomnit"]<-"Moss"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="tragra"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib_010nm$PFT_3[alaskaSpecLib_010nm$PFT=="wooly_salix"]<-"Shrub"

##Creates new dataframe with a frequency column (shows the amount of scans per species within the library) 
alaskaSpecLib_010nm_freqTab<-as.data.frame(table(alaskaSpecLib_010nm$PFT_2))

##Combines the frequency dataframe with spectral library
alaskaSpecLib_010nm$Freq<-alaskaSpecLib_010nm_freqTab$Freq[match(alaskaSpecLib_010nm$PFT_2,alaskaSpecLib_010nm_freqTab$Var1)]

##Reorder columns
alaskaSpecLib_010nm<-alaskaSpecLib_010nm%>%select(ScanID,PFT,PFT_2,PFT_3,Freq,everything())

##Plants only
alaskaSpecLib_010nm_plants<-subset(alaskaSpecLib_010nm,PFT_3!="Abiotic")

##Creates dataframes that has all the species with scans greater than specified values (5,10,15,20)
alaskaSpecLib_010nm_plants_more05<-subset(alaskaSpecLib_010nm_plants,Freq>=5)
alaskaSpecLib_010nm_plants_more10<-subset(alaskaSpecLib_010nm_plants,Freq>=10)
alaskaSpecLib_010nm_plants_more15<-subset(alaskaSpecLib_010nm_plants,Freq>=15)
alaskaSpecLib_010nm_plants_more20<-subset(alaskaSpecLib_010nm_plants,Freq>=20)

##creates datafranme with equal amount of scans for each species (10,15,20 scans per species )
alaskaSpecLib_010nm_plants_equal05<-alaskaSpecLib_010nm_plants_more05 %>% group_by(PFT_2) %>% sample_n(5,replace = TRUE)
alaskaSpecLib_010nm_plants_equal10<-alaskaSpecLib_010nm_plants_more10 %>% group_by(PFT_2) %>% sample_n(10,replace = TRUE)
alaskaSpecLib_010nm_plants_equal15<-alaskaSpecLib_010nm_plants_more15 %>% group_by(PFT_2) %>% sample_n(15,replace = TRUE)
alaskaSpecLib_010nm_plants_equal20<-alaskaSpecLib_010nm_plants_more20 %>% group_by(PFT_2) %>% sample_n(20,replace = TRUE)

##Extracts scans for each life form (lichen, bryophyte, vascular plant) for spectral library
#alaskaSpecLib_010nm_lichen     <-subset(alaskaSpecLib_010nm_plants,PFT_3=="Lichen")
#alaskaSpecLib_010nm_bryo       <-subset(alaskaSpecLib_010nm_plants,PFT_3=="Moss")
#alaskaSpecLib_010nm_lichen_bryo<-subset(alaskaSpecLib_010nm_plants,PFT_3=="Lichen"|PFT_3=="Moss")
#alaskaSpecLib_010nm_vascular   <-subset(alaskaSpecLib_010nm_plants,PFT_3!="Lichen"&PFT_3!="Moss")

###save spectral library 
#write.csv(alaskaSpecLib_010nm            ,"Processed_spec/alaskaSpecLib_010nm/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_2019_all.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_010nm_plants     ,"Processed_spec/alaskaSpecLib_010nm/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_2019_plants.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_010nm_lichen     ,"Processed_spec/alaskaSpecLib_010nm/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_2019_lichen.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_010nm_bryo       ,"Processed_spec/alaskaSpecLib_010nm/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_2019_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_010nm_lichen_bryo,"Processed_spec/alaskaSpecLib_010nm/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_2019_lichen_bryo.csv",row.names = FALSE)
#write.csv(alaskaSpecLib_010nm_vascular   ,"Processed_spec/alaskaSpecLib_010nm/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_2019_vascular.csv",row.names = FALSE)

write.csv(alaskaSpecLib_010nm_plants_more05 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_more05.csv")
write.csv(alaskaSpecLib_010nm_plants_more10 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_more10.csv")
write.csv(alaskaSpecLib_010nm_plants_more15 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_more15.csv")
write.csv(alaskaSpecLib_010nm_plants_more20 ,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_more20.csv")
write.csv(alaskaSpecLib_010nm_plants_equal05,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_equal05.csv")
write.csv(alaskaSpecLib_010nm_plants_equal10,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_equal10.csv")
write.csv(alaskaSpecLib_010nm_plants_equal15,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_equal15.csv")
write.csv(alaskaSpecLib_010nm_plants_equal20,"Processed_spec/AlaskaSpecLib/Spectral_Library/Raw/2019/alaskaSpecLib_010nm_plants_equal20.csv")
