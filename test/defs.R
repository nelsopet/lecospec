source("Functions/lecospectR.R")

# a dummy model that always predicts zero
dummy_model_fg1 <- function(data, output_dim = 1){
    return(rep(c("Abiotic"),nrow(data)))
}

dummy_model_genus <- function(data){
    return(rep(c("Soil"), nrow(data)))
}

apply_dummy <- function(model, df){
    return(model(df))
}

dummy_model <- LSModel(dummy_model_genus, apply_dummy)

print(read.csv("assets/vegIndicesUsed.csv", header = TRUE)$x)

save(dummy_model, file = "mle/dummy.rda")

get_required_veg_indices(dummy_model)

print(class(dummy_model))

