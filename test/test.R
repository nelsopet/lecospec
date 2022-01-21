source("./Functions/lecospectR.R")

test_log <- "./test/test.log"
sink(test_log, append = TRUE)
print("test start")
print(date())

test_data <- "./test/test_data/"
config_folder <- "./test/test_configs/"
test_files <- list.files(
    test_data, 
    pattern = "*.envi",
    full.names = TRUE)
print("Testing on files:")
print(test_files)

test_configs <- list.files(
    config_folder,
    full.names = TRUE)
print("Configuration files:")
print(test_configs)

output_files <- list()



for(raster_file in test_files){
    for(config in test_configs){
        tryCatch(
            {
                output_file <- paste0(
                        "./test/test_outputs/pred_",
                        raster_file)
                output <- estimate_land_cover(
                    raster_file, 
                    config,
                    output_filepath = output_file,
                    use_external_bands = TRUE)

                append(output_files, output_file)
                print(paste0(
                    date(),
                    "\nTest passed for config: \n",
                    config,
                    "and file: \n",
                    file
                ))
            }, 

            warning = function(w) {
                print(paste0(
                    date(),
                    "\nTest completed with warnings for config: \n",
                    config,
                    "\nand file: \n",
                    raster_file,
                    "\nwarnings: \n",
                    w
                ))
            },
            error = function(e){
                print(paste0(
                    date(),
                    "\nTest failed for file:\n",
                    raster_file,
                    " using config: \n",
                    config,
                    "\nError: \n",
                    e
                ))
            }
        )
    }
}

write.csv(
    output_files,
    "./test/last_test_files.txt"
)