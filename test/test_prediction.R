source("Functions/lecospectR.R")

library(testthat)

names(raster::brick("test/inputs/one_row_tile.envi"))

testthat::test_that("Predictions run on one-row input", {
    input_path <- "test/inputs/one_row_tile.envi"
    model_path <- "results/f02a4a5d-beff-418b-8c13-20df941457c7/model.rda"

    prediction <- process_tile(
        input_path,
        load_model(model_path),
        0,
        band_names = read.csv("assets/bands.csv")$x
    )

    expect_s4_class(prediction, raster::RasterLayer)
})

testthat::test_that("Runs predictions on normal data", {
    
})