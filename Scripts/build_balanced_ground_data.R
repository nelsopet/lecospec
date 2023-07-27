source("Functions/lecospectR.R")

base_path <- "Data/D_002_SpecLib_Derivs.csv"
full_data <- read.csv(base_path)
colnames(full_data)



grd_train <- subset(
    full_data, 
    select=-c(
        ScanID,
        Area,
        Code_name,
        Species_name,
        Functional_group1,
        Functional_group2,
        Species_name_Freq,
        Functional_group1_Freq,
        Functional_group2_Freq,
        Genus,
        Version,
        File.Name,
        Instrument,
        Detectors,
        Measurement,
        Date,
        Time,
        Battery.Voltage,
        Averages,
        Integration1,
        Integration2,
        Integration3,
        Dark.Mode,
        Foreoptic,
        Radiometric.Calibration,
        Units,
        Latitude,
        Longitude,
        Altitude,
        GPS.Time,
        Satellites,
        Calibrated.Reference.Correction.File,
        Channels,
        ScanNum
    )
)

labels <- full_data$Functional_group1

train_data <- 

