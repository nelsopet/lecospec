source("Functions/lecospectR.R")

c_model <- load_model("mle/models/gs/cd565a46-8d6d-42b8-aa54-29abd0e9affc.rda")

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
