source("Functions/lecospectR.R")
source("Scripts/validation_defs.R")

c_model <- load_model("mle/models/gs/3012f5ed-7d17-4e94-a454-24d8a65f5b4f.rda")

print(c_model$forest$independent.variable.names)

str(c_model)
training_variable_names <- c_model$forest$independent.variable.names %>% as.list()
split_vars <- c_model$forest$split.varIDs[[1]] %>% as.list()
split_vars[[1]][[1]][[1]]
var_names <- list()

for(x in seq_along(split_vars)){
    
    if( split_vars[[x]][[1]][[1]] > 0 ){
        var_names <- append(var_names, training_variable_names[[split_vars[[x]][[1]][[1]]]])
    }
}

print(unique(var_names))
print(estimate_land_cover)

datacube_path <- "Data/raw_0_rd_rf_56pctWhiteRef_or"
test_path <- "Data/tile_34M0bC2ZrwSZkgXo.envi"
#test_path_1 <- "Data/Ground_Validation/Imagery/BisonGulchQuads.envi"


results <- estimate_land_cover(
    test_path_3,
    model = c_model
)

raster::NAvalue(results)

plot(results)
X11();plot(results_4)

X11();plot(results_4 - results_3)

X11();plot(results)
X11();plot(results_2)
X11();plot(results - results_2, bpy.colors(10))
