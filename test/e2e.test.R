library(testthat)

source("Functions/lecospectR.R")
source("Scripts/validation_defs.R")
source("test/test_defs.R")

testthat::test_that("Validation runs", {
    ml_model <- load_model("mle/models/scale_noise_img_w.rda")
    test_validation <- validate_model(
        ml_model, "test/e2e", 
        scale_input = TRUE
    )
})

