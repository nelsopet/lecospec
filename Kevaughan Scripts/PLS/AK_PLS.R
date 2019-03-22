library(spectrolab)
library(tidyverse)
library(pls)

###reads in alaskasspeclib
alaskaSpecLib<-read.csv("/Alaska_Spectral_Library/processed spec/alaskaSpecLib.csv",check.names = FALSE)

## Remove unwanted metadata
alaskaSpecLib[c("ScanID","PFT_2","area")] = NULL
alaskaSpecLib$PFT<-as.factor(alaskaSpecLib$PFT)
alaskaSpecLib$PFT<-as.numeric(alaskaSpecLib$PFT)

###Make test and validation data set
training_dat<-alaskaSpecLib[1:600,   ]
test_dat = alaskaSpecLib[601:871, -1]

###Fit
fit_alaskaSpecLib = plsr(PFT~ ., data = training_dat, ncomp = 30)
plot(fit_alaskaSpecLib)
summary(fit_alaskaSpecLib)

## Predict chlorophyll from spectra!
prediction = predict(object = fit_alaskaSpecLib, newdata = test_dat, ncomp = 30)
plot(prediction)


plot(val,prediction, cex = 1, pch = 16)
