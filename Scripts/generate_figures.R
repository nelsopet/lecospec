source("./Functions/lecospectR.R")
require(sf)
require(stringr)

aggregation_path <- "figures/aggregator.csv"

# run the 
aggregated_df <- read.csv(aggregation_path)
ordered_df <- aggregated_df[order(aggregated_df$validation_prop),]

bar_graph <- plot_quadrat_proportions(aggregated_df)

windows();bar_graph



validation_paths <- c(
    "./figures/BisonGulch/bg_validation_1.csv",
    "./figures/BisonGulch/bg_validation_2.csv",
    "./figures/BisonGulch/bg_validation_3.csv",
    "./figures/BisonGulch/bg_validation_4.csv",
    "./figures/BisonGulch/bg_validation_5.csv",
    "./figures/BisonGulch/bg_validation_6.csv",
    "./figures/BisonGulch/bg_validation_7.csv",
    "./figures/BisonGulch/bg_validation_8.csv",
    "./figures/BisonGulch/bg_validation_9.csv",
    "./figures/Chatanika/ca_validation_1.csv",
    "./figures/Chatanika/ca_validation_2.csv",
    "./figures/Chatanika/ca_validation_3.csv",
    "./figures/Chatanika/ca_validation_4.csv",
    "./figures/Chatanika/ca_validation_5.csv",
    "./figures/Chatanika/ca_validation_6.csv",
    "./figures/Chatanika/ca_validation_7.csv",
    "./figures/Chatanika/ca_validation_8.csv",
    "./figures/Chatanika/ca_validation_9.csv",
    "./figures/Chatanika/ca_validation_10.csv",
    "./figures/Chatanika/ca_validation_11.csv",
    "./figures/EightMile/8mile_validation_1.csv",
    "./figures/EightMile/8mile_validation_2.csv",
    "./figures/EightMile/8mile_validation_3.csv",
    "./figures/EightMile/8mile_validation_4.csv",
    "./figures/EightMile/8mile_validation_5.csv",
    "./figures/EightMile/8mile_validation_6.csv",
    "./figures/EightMile/8mile_validation_7.csv",
    "./figures/EightMile/8mile_validation_8.csv",
    "./figures/EightMile/8mile_validation_9.csv",
    "./figures/EightMile/8mile_validation_10.csv",
    "./figures/EightMile/8mile_validation_11.csv",
    "./figures/MurphyDome/md_b_validation_1.csv",
    "./figures/MurphyDome/md_b_validation_2.csv",
    "./figures/MurphyDome/md_b_validation_3.csv",
    "./figures/MurphyDome/md_b_validation_4.csv",
    "./figures/MurphyDome/md_b_validation_5.csv",
    "./figures/MurphyDome/md_c_validation_1.csv",
    "./figures/MurphyDome/md_c_validation_2.csv",
    "./figures/MurphyDome/md_c_validation_3.csv",
    "./figures/MurphyDome/md_c_validation_4.csv",
    "./figures/MurphyDome/md_c_validation_5.csv",
    "./figures/twelveMile1/tm1_validation_1.csv",
    "./figures/twelveMile1/tm1_validation_2.csv",
    "./figures/twelveMile1/tm1_validation_3.csv",
    "./figures/twelveMile1/tm1_validation_4.csv",
    "./figures/twelveMile1/tm1_validation_5.csv",
    "./figures/twelveMile2/tm2_validation_1.csv",
    "./figures/twelveMile2/tm2_validation_2.csv",
    "./figures/twelveMile2/tm2_validation_3.csv",
    "./figures/twelveMile2/tm2_validation_4.csv"
)


parse_path <- function(path){
    no_prefix <- stringr::str_replace(path, "./figures/", "")
    no_suffix <- stringr::str_replace(no_prefix, "/*_validation_*.csv")
    return(no_suffix)
}

load_and_label_data <- function(path){
    label <- parse_path(path)
    df <- read.csv(path)
    df$site <- label
    return(df)
}

loaded_validation <- vapply(validation_paths, load_and_label_data)

