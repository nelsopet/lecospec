source("./Functions/lecospectR.R")


# Get some results to work with

test_path <- "./Data/Ground_Validation/BisonGulchQuads.envi"
model_path <- "C:/Users/kenne/Documents/GitHub/lecospec/Output/E_003_Pred_Model_RandomForest_FncGrp1_1000trees.rda"

ml_model <- load_model(model_path)
bandnames <- read.csv("./bands.csv")$x %>% as.vector()

tile_results <- process_tile(
    test_path, 
    ml_model, 
    1,
    cluster = NULL, 
    return_raster = TRUE, 
    names = bandnames,
    save_path = "./test_raster_save.grd", 
    suppress_output = FALSE)

plot(tile_results)
print(tile_results)


# load the validation data