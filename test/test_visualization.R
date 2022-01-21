source("./Functions/LecospectR.R")

last_test_files <- read.csv(
    "./last_test_files.txt"
) %>% as.list()


# look at each file.  Press enter in the terminal to move to the next file
for(prediction in last_test_files){
    visualize_prediction(prediction)
    readline(prompt="Press [enter] to continue")
}
