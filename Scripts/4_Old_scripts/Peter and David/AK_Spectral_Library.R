require(tidyverse)
require(RStoolbox)


setwd("/Users/peternelson 1/Documents/UMFK/Grants/NASA_ABOVE/Data/AK_Spectral_Library/AK_Spectral_Library/AK Spec Lib/")
alaskaSpecLib<-rbind (
brooksLib
,yKDeltLib
,bethelLib
,AK2018_lib
,Murph_lib
,Murph2);


pdf("Median_Refl_Pft.pdf")
test<-alaskaSpecLib%>% group_by(PFT_2, Band) %>% mutate(Median_reflectance = median(Refl)) %>% ggplot(aes(Band,Median_reflectance))+geom_line(aes(color=PFT_2))
test+facet_wrap(facets= vars(PFT),  nrow=10, ncol=11)+theme(legend.position="none")
dev.off()

pdf("Raw_Refl_Pft.pdf")
test<-alaskaSpecLib %>% group_by(PFT, Band) %>% ggplot(aes(Band,Refl))+geom_line(aes(color=PFT))
test+facet_wrap(facets= vars(PFT), nrow=10, ncol=11)+theme(legend.position="none")
dev.off()

alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aleoch"]<-"Alectoria ochroleuca"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="alnfru"]<-"Alnus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arccen"]<-"Arctocetraria centrifuga"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcnig"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcrub"]<-"Arctostaphyllos"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arcsta"]<-"Arctostaphyllos "
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="arctop"]<-"Unknown"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="asachr"]<-"Asahinea chrysantha"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aulpal"]<-"Aulacomnium palustre"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="aultur"]<-"Aulacomnium turgidum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bare rock"]<-"Bare Rock"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bare_soil"]<-"Bare Soil"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betnan"]<-"Betula nana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="betneo"]<-"Betula neoalaskana"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="bryoria"]<-"Bryoria sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="calcan"]<-"Calamogrostis sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carlin"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carlyn"]<-"Carex sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="carram"]<-"Carex sp."
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
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dead salix"]<-"Dead Salix"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="dicranum"]<-"Dicranum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="empnig"]<-"Empetrum nigrum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="equarv"]<-"Equisetum arvense"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="equsyl"]<-"Equisetum sylvaticum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="erivag"]<-"Eriophorum vaginatum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="evemes"]<-"Evernia mesomorpha"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="flacuc"]<-"Flavocetraria cucculata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="flaniv"]<-"Flavocetraria nivalis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="gravel"]<-"Gravel"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Rhizocarpon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="herlan"]<-"Heracleum lanatum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hylspl"]<-"Hylocomium splendens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hypaus"]<-"Hypogymnia austerodes"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="hypspl"]<-"Hylocomium splendens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="icmeri"]<-"Icmadophila ericetorum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="irisit"]<-"Iris sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="leddec"]<-"Ledum decumbens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="loipro"]<-"Loisleuria procumbens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="luparc"]<-"Lupinus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="masric"]<-"Masonhalea richardsonii"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="melanelia"]<-"Melanelia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="melhep"]<-"Melanelia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="neparc"]<-"Nephroma arcticum"
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
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="plagiomnium"]<-"Plagiomnium sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="plesch"]<-"Pleurozium schreberi"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="poljen"]<-"Polytrichum juniperinum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="poljun"]<-"Polytrichum juniperinum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="polstr"]<-"Polytrichum strictum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="polytrichum"]<-"Polytrichum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="popbal"]<-"Populus balsamifera"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="quartz"]<-"Quartz"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="raclan"]<-"Racomitrium lanoiginosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhigeo"]<-"Rhizocarpon geographicum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rhyrug"]<-"Rhytidum rugosum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rosasc"]<-"Rosa acicularis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rubcam"]<-"Rubus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="rubcha"]<-"Rubus sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salala"]<-"Salix alaxensis"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salarb"]<-"Salix arbusculoides"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salgla"]<-"Salix glauca"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sallan"]<-"Salix lanata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salova"]<-"Salix ovalifolia"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salpul"]<-"Salix pulchra"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="salric"]<-"Salix richardsonii"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sphagn"]<-"Sphagnum sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="sphfus"]<-"Sphagnum fuscum"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="spruce bark"]<-"Pices (bark)"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="stepas"]<-"Stereocaulon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="stetas"]<-"Stereocaulon sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="toefeldia"]<-"Toefeldia sp."
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tomnit"]<-"Tomenthypnum nitens"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="tragra"]<-"Trapelopsis granulosa"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="umbarc"]<-"Umbilicaria arctica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="umbhyp"]<-"Umbilicaria hyperborea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="usnlap"]<-"Usnea lapponica"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="usnsca"]<-"Usnea scabrata"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vacvit"]<-"Vaccinium vitis-idea"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="vulpin"]<-"Vulpicida pinastri"
alaskaSpecLib$PFT_2[alaskaSpecLib$PFT=="wooly_salix"]<-"Salix (wooly)"


alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="aleoch"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="alnfru"]<-"Shrub"
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
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betnan"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="betneo"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="bryoria"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="calcan"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlin"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carlyn"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="carram"]<-"Graminoid"
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
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dead salix"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="dicranum"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="empnig"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equarv"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="equsyl"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="erivag"]<-"Graminoid"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="evemes"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flacuc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="flaniv"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="gravel"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="grey_rhizocarpon"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="herlan"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hylspl"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypaus"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="hypspl"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="icmeri"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="irisit"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="leddec"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="loipro"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="luparc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="masric"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melanelia"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="melhep"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="neparc"]<-"Lichen"
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
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plagiomnium"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="plesch"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljen"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="poljun"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polstr"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="polytrichum"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="popbal"]<-"Tree"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="quartz"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="raclan"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhigeo"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rhyrug"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rosasc"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcam"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="rubcha"]<-"Dwarf Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salala"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salarb"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salgla"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sallan"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salova"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salpul"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="salric"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphagn"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="sphfus"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="spruce bark"]<-"Abiotic"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stepas"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="stetas"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="toefeldia"]<-"Forb"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tomnit"]<-"Moss"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="tragra"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbarc"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="umbhyp"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnlap"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="usnsca"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vacvit"]<-"Shrub"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="vulpin"]<-"Lichen"
alaskaSpecLib$PFT_3[alaskaSpecLib$PFT=="wooly_salix"]<-"Shrub"



jpeg("Moss.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Moss") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Moss spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Cladonia_Lichen.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT %in% Cladonia) %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Cladonia lichen spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()


jpeg("Dark_Lichen.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT %in% Dark_lichen) %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Dark lichen spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Grey_Lichen.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT %in% Grey_Lichen) %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Grey lichen spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Yellow_Lichen.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT %in% Yellow_Lichen) %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Yellow lichen spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()



Cladonia<-c(
"claama"
,"clacor"
,"clacuc"
,"clagra"
,"clamit"
,"claran"
,"claste"
,"clasty"
,"clasul"
,"claunc"
)

Dark_lichen<-c(
"bryoria"
,"cetisl"
,"cetlae"
,"masric"
,"melanelia"
,"melhep"
,"tragra"
,"umbarc"
,"umbhyp"
,"grey_rhizocarpon"
,"orange_Porpidia"
)

Grey_Lichen<-c(
"hypaus"
,"icmeri"
,"paromp"
,"parsul"
,"stepas"
,"stetas"
,"pelapt"
,"pelleu"
,"pelmal"
,"pelsca"
)

Yellow_Lichen<-c(
"paramb"
,"usnlap"
,"usnsca"
,"neparc"
,"evemes"
,"flacuc"
,"flaniv"
,"vulpin"
,"aleoch"
,"arccen"
,"asachr")


jpeg("Shrub.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Shrub") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Shrub spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Dwarf Shrub.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Dwarf Shrub") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Dwarf shrub spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Tree.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Tree") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Tree spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Forb.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Forb") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Forb spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Graminoid.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Graminoid") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Graminoid spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()

jpeg("Abiotic.jpg", units="px", height = 1400, width=2400, res=350)
##test<-
alaskaSpecLib%>% subset(PFT_3=="Abiotic") %>% group_by(PFT_2, Band) %>% mutate(Median_Reflectance = median(Refl)) %>% ggplot(aes(Band,Median_Reflectance))+geom_line(aes(color=PFT_2))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+labs(title="Abiotic spectral signatures")
##test+facet_wrap(facets= vars(PFT_2), ncol=4)+theme(legend.position="none")
dev.off()





## write out the spectral library in ENVI format
alaskaSpecLib%>% group_by(PFT, Band) %>% mutate(Refl_Median = median(Refl)) %>% select(PFT, Band, Refl_Median) %>% unique() %>% spread(PFT, Refl_Median) %>% writeSLI(, path="AK_Spec_Lib_ENVI_4.sli", wavl.units = "Nanometers")

alaskaSpecLib%>% group_by(PFT, Band) %>% mutate(Refl_Median = median(Refl)*0.01) %>% select(PFT, Band, Refl_Median) %>% unique() %>% spread(PFT, Refl_Median) %>% writeSLI(path="AK_Spec_Lib_ENVI_4_rescale.sli", wavl.units = "Nanometers")

## see https://stackoverflow.com/questions/43373568/dplyr-to-iterate-over-all-columns-for-quantile
alaskaSpecLib %>% group_by(PFT,Band) %>% summarise(list(quantile(Refl, probs = c(0.25, 0.5, 0.75)))) %>% unnest() #%>% transpose()

## Problem is that this remove individual observations ... eg. one reflectance measurement for a single band and not the whole scan
## I need to flag the whole scan and remove it if it is an outlier
## I think I need to add a scan number to each PFT scan to tell them apart
## probably would be good to have an informative scan number like lat/long concatenated or timestamp
remove_outliers <- function(x, na.rm = TRUE, ...) {
qnt <- quantile(x, probs=c(.1, .9), na.rm = na.rm, ...)
H <- 1.5 * IQR(x, na.rm = na.rm)
y <- x
y[x < (qnt[1] - H)] <- NA
y[x > (qnt[2] + H)] <- NA
y
}



This works

for figure

forfig<-c("salala",
"salpul",
"alnfru",
"betnan",
"popbal")

                       
+scale_color_manual(name="PFTs",labels=c("Alder", "Dwarf birch", "Paper birch","Balsam poplar","Grey-leaved willow", "Green-leaved willow",),values=c("red","green","blue","orange","brown","grey"))



alaskaSpecLib %>% 
subset(PFT %in% forfig) %>% 
group_by(PFT,Band) %>% 
mutate(PFT_median = median(Refl)) %>% 
mutate(PFT_max = max(Refl)) %>%
mutate(PFT_min = min(Refl)) %>%
ggplot(aes(Band,PFT_median))+geom_line(aes(color=PFT))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+scale_color_manual(name="Plant",labels=c("Alder", "Dwarf birch", "Balsam poplar","Felt-leaf willow", "Diamond-leaf willow"),values=c("red","green","blue","orange","brown"))+labs(x="Wavelength(nm)",y="Median reflectance")+ xlim(400,1000)

ggplot(aes(Band,PFT_min))+geom_line(aes(color=PFT))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+scale_color_manual(name="Plant",labels=c("Alder", "Dwarf birch", "Balsam poplar","Felt-leaved willow", "Diamond-leaved willow"),values=c("red","green","blue","orange","brown"))+labs(x="Wavelength(nm)",y="Min reflectance")
ggplot(aes(Band,PFT_max))+geom_line(aes(color=PFT))+theme(panel.background = element_rect(fill = "white", colour = "grey50"))+scale_color_manual(name="Plant",labels=c("Alder", "Dwarf birch", "Balsam poplar","Felt-leaved willow", "Diamond-leaved willow"),values=c("red","green","blue","orange","brown"))+labs(x="Wavelength(nm)",y="Max reflectance")


## change font size in axes
alaskaSpecLib %>% 
subset(PFT %in% forfig) %>% 
group_by(PFT,Band) %>% 
mutate(PFT_median = median(Refl)) %>% 
mutate(PFT_max = max(Refl)) %>%
mutate(PFT_min = min(Refl)) %>%
ggplot(aes(Band,PFT_median))+
geom_line(aes(color=PFT))+
theme(panel.background = element_rect(fill = "white", colour = "grey50"),text = element_text(size=20))+
scale_color_manual(name="Plant",labels=c("Alder", "Dwarf birch", "Balsam poplar","Felt-leaf willow", "Diamond-leaf willow"),values=c("red","green","blue","orange","brown"))+
labs(x="Wavelength(nm)",y="Median reflectance")


alaskaSpecLib %>% 
subset(PFT %in% forfig) %>% 
group_by(PFT,Band) %>% 
mutate(PFT_median = median(Refl)) %>% 
mutate(PFT_max = max(Refl)) %>%
mutate(PFT_min = min(Refl)) %>%
ggplot(aes(Band,PFT_median))+
geom_line(aes(color=PFT))+
theme(panel.background = element_rect(fill = "white", colour = "grey50"),text = element_text(size=20),legend.position="none")+
scale_color_manual(name="Plant",labels=c("Alder", "Dwarf birch", "Balsam poplar","Felt-leaf willow", "Diamond-leaf willow"),values=c("red","green","blue","orange","brown"))+
labs(x="Wavelength(nm)",y="Median reflectance")

