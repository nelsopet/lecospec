source("Functions/lecospectR.R")
library(testthat)

speclib_filepath <- "./Data/D_002_SpecLib_Derivs.csv"

data <- read.csv(speclib_filepath, header = TRUE)

# Plot Spectra goes here

# print the basic data
print(colnames(data))
print(summary(data))

# check just the raw spectra
automated_speclib_results <- automated_check(
    data, 
    ignore_cols = colnames(data)[1:140])

# Examine the count by PFT
print(data$Functional_group1 %>% as.factor() %>% table())
