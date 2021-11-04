library(raster)
library(future)

install.packages("arrow")
library(arrow)

future::plan(multicore)

output_file <- "/home/kbundy/lsData/converted/scan_1511.csv"

process_f <- future::future({
    # define filepaths
    input_file <- "/home/kbundy/lsData/alaska/raw_1511_rd_rf_or"
    
    # Load and convert the data
    input_ras <- raster::brick(input_file)
    input_df <- raster::rasterToPoints(input_ras)
})

arrow::write_csv_arrow(
    future::value(process_f),
    output_file
)