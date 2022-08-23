# a dummy model that always predicts zero
dummy_model <- function(data, output_dim = 0){
    return(matrix(nrow = nrow(data), ncol = output_dim))
}

