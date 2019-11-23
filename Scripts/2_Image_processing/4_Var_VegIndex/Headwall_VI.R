####################Calculates the Vegitation indices for the spectral library developed from headwall's bandpases####
library(spectrolab)
library(tidyverse)
library(hsdar)

##Reads in Imagery as a dataframe...
Clayton_test_HDW<-brick("Original_data/Test_imagery/Clayton_test_HDW")%>%rasterToPoints()%>%as.data.frame()

##Reads in spectral library....dim 1975  333
alaskaSpeclib_HDW<-readRDS("Outputs/2_HDW_Imagery/1_processing/alaskaSpeclib_HDW.rds")%>%as.data.frame()%>%dplyr::select(-sample_name)

##Reads in bandpasses for imagery to be used later
HDW_ng_wv<-scan("Outputs/2_HDW_Imagery/1_Processing/Headwall_wv", numeric())

##Now lets change the column names of the df created from thge image, we want these column names to match the bandpasses
colnames(Clayton_test_HDW)[-1:-2]<-HDW_ng_wv

##Now lets check the range of the values in the image and spectral library
test=lapply(Clayton_test_HDW[,-1:-2],range)%>%as.data.frame%>%t()%>%as.data.frame
test%>%View()
test%>%lapply(range) ### All values fall between 0 and 1.2 and there are no NA values

test2<-alaskaSpeclib_HDW[-1:-7]%>%lapply(range)%>%as.data.frame()%>%t()%>%as.data.frame
test2%>%lapply(range)
test2<-test2%>%subset(V1<0)####ewhy am i getting negative values here?


##now we have all the columns that have values greater than two, lets save those column names in an object
badscans<-rownames(test2)
badscans<-c( "416.11" , "417.962", "419.814", "421.666", "442.035", "443.887","445.739", "447.59" ,"449.442", "506.847" 
            ,"508.699", "510.551", "512.403", "514.254", "516.106", "529.069","530.92" , "532.772","534.624", "536.476"
            ,"549.438", "551.29" , "553.142", "554.994", "556.845", "567.956","569.808", "571.659","573.511", "575.363"
            ,"586.474", "588.325", "590.177", "592.029", "604.991", "606.843","608.695", "610.547","612.399", "623.509"
            ,"625.361", "627.213", "629.065", "630.916", "632.768", "643.879","645.731", "647.582","649.434", "651.286"
            ,"653.138", "688.321", "690.173", "692.025", "693.877", "695.729","697.58" , "710.543","712.395", "714.246"
            ,"716.098", "717.95" , "729.061", "730.912", "732.764", "734.616","736.468", "738.319","749.43" , "751.282"
            ,"753.134", "754.985", "756.837", "758.689", "769.8"  , "771.651","773.503", "775.355","777.207", "779.058"
            ,"790.169", "792.021", "793.873", "795.724", "797.576", "799.428","814.242", "816.094","817.946", "819.798"
            ,"821.649", "823.501", "834.612", "836.464", "838.315", "840.167","842.019", "843.871","854.981", "856.833"
            ,"858.685", "860.537", "862.388", "875.351", "877.203", "879.054","880.906", "882.758","895.72" , "897.572"
            ,"899.424", "901.276", "903.127", "914.238", "916.09 ", "917.942","919.793", "943.867","945.718", "947.57"
            ,"949.422", "951.274", "953.125", "967.94" , "969.791", "971.643","973.495", "975.347")

##Column names are saved, lets create a function that will will remove all those rows that have values less than 0
####Need to come up with a function
alaskaSpeclib_HDW<-alaskaSpeclib_HDW[apply(alaskaSpeclib_HDW[,badscans]>0, 1, all),]

###you'll need to convert your dfs to a matrix before VIS can be applied
##lets fo this for df created from the image and our spectral library of scans
alaskaSpeclib_HDW_matrix<-as.matrix(alaskaSpeclib_HDW[-1:-7])
Clayton_test_HDW_matrix <-as.matrix(Clayton_test_HDW [-1:-2])

##lets check the column attributes to see if any weird values were introduced
alaskaSpeclib_HDW_matrix%>%max()
Clayton_test_HDW_matrix %>%max()




