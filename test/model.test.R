source("Functions/lecospectR.R")

model_dir <- "mle/models/"
model_paths <- list.files(model_dir, full.names = TRUE)
data <- read.csv()

for(model_file in model_paths){
    model <- load_model(model_file)
    results <- apply_model(data, model) %>% as.factor()
    print(results %>% table())
    break()
}