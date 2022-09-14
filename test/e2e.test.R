library(testthat)

source("Functions/lecospectR.R")
source("Scripts/validation_defs.R")
source("test/test_defs.R")

testthat::test_that("Validation runs", {
    ml_model <- load_model(model_path)
    test_validation <- validate_model(
        ml_model, "test/e2e"
    )
})
