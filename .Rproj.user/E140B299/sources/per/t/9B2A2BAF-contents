library(spectrolab)
library(tidyverse)
library(pls)

###reads in alaskasspeclib
alaskaSpecLib<-read.csv("/Alaska_Spectral_Library/processed spec/alaskaSpecLib.csv",check.names = FALSE)

## Remove unwanted metadata
alaskaSpecLib[c("ScanID","PFT_2","area")] = NULL

###extracts the data for species
vacvit<-alaskaSpecLib%>%subset(PFT%in%"vacvit")
bryoria<-alaskaSpecLib%>%subset(PFT%in%"bryoria")

###Fit
fit_vacvit = plsr(PFT ~ ., data = vacvit, validation = "CV", ncomp = 7)
plot(fit_vacvit)

fit_bryoria = plsr(PFT ~ ., data = bryoria, validation = "CV", ncomp = 8, jacknife=TRUE)
plot(bryoria)
