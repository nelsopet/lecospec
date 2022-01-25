source("./Functions/LecospectR.R")

last_test_files <- read.csv(
    "./last_test_files.txt"
) %>% as.list()


# look at each file.  Press enter in the terminal to move to the next file
for(prediction in last_test_files){
    visualize_prediction(prediction)
    readline(prompt="Press [enter] to continue")
}



colors <- create_color_map(training_data_key, "Functional_group1")
labels <- create_labels(training_data_key, "Functional_group1")
colors(labels)
map <- visualize_predictions(
    "./prediction_XAUipWpkI6eHsEcK.grd",
    training_data_key,
    "Functional_group1")
map
print(map)