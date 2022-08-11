source("./Functions/lecospectR.R")
require(sf)
require(stringr)

aggregation_path <- "figures/aggregator.csv"

# run the 
aggregated_df <- read.csv(aggregation_path)
ordered_df <- aggregated_df[order(aggregated_df$validation_prop),]

bar_graph <- plot_quadrat_proportions(aggregated_df)

windows();bar_graph



validation_paths <- list(
    "figures/BisonGulch/bg_validation_1.csv",
    "figures/BisonGulch/bg_validation_2.csv",
    "figures/BisonGulch/bg_validation_3.csv",
    "figures/BisonGulch/bg_validation_4.csv",
    "figures/BisonGulch/bg_validation_5.csv",
    "figures/BisonGulch/bg_validation_6.csv",
    "figures/BisonGulch/bg_validation_7.csv",
    "figures/BisonGulch/bg_validation_8.csv",
    "figures/BisonGulch/bg_validation_9.csv",
    "figures/Chatanika/ca_validation_1.csv",
    "figures/Chatanika/ca_validation_2.csv",
    "figures/Chatanika/ca_validation_3.csv",
    "figures/Chatanika/ca_validation_4.csv",
    "figures/Chatanika/ca_validation_5.csv",
    "figures/Chatanika/ca_validation_6.csv",
    "figures/Chatanika/ca_validation_7.csv",
    "figures/Chatanika/ca_validation_8.csv",
    "figures/Chatanika/ca_validation_9.csv",
    "figures/Chatanika/ca_validation_10.csv",
    "figures/Chatanika/ca_validation_11.csv",
    "figures/EightMile/8mile_validation_1.csv",
    "figures/EightMile/8mile_validation_2.csv",
    "figures/EightMile/8mile_validation_3.csv",
    "figures/EightMile/8mile_validation_4.csv",
    "figures/EightMile/8mile_validation_5.csv",
    "figures/EightMile/8mile_validation_6.csv",
    "figures/EightMile/8mile_validation_7.csv",
    "figures/EightMile/8mile_validation_8.csv",
    "figures/EightMile/8mile_validation_9.csv",
    "figures/EightMile/8mile_validation_10.csv",
    "figures/EightMile/8mile_validation_11.csv",
    "figures/MurphyDome/md_a_validation_1.csv",
    "figures/MurphyDome/md_a_validation_2.csv",
    "figures/MurphyDome/md_b_validation_1.csv",
    "figures/MurphyDome/md_b_validation_2.csv",
    "figures/MurphyDome/md_b_validation_3.csv",
    "figures/MurphyDome/md_b_validation_4.csv",
    "figures/MurphyDome/md_b_validation_5.csv",
    "figures/MurphyDome/md_c_validation_1.csv",
    "figures/MurphyDome/md_c_validation_2.csv",
    "figures/MurphyDome/md_c_validation_3.csv",
    "figures/MurphyDome/md_c_validation_4.csv",
    "figures/MurphyDome/md_c_validation_5.csv",
    "figures/twelveMile1/tm1_validation_1.csv",
    "figures/twelveMile1/tm1_validation_2.csv",
    "figures/twelveMile1/tm1_validation_3.csv",
    "figures/twelveMile1/tm1_validation_4.csv",
    "figures/twelveMile1/tm1_validation_5.csv",
    "figures/twelveMile2/tm2_validation_1.csv",
    "figures/twelveMile2/tm2_validation_2.csv",
    "figures/twelveMile2/tm2_validation_3.csv",
    "figures/twelveMile2/tm2_validation_4.csv"
)


parse_path <- function(path){
    split_path <- strsplit(path, split="/", fixed = TRUE)
    return(split_path[[1]][[2]])
}

print(parse_path("figures/twelveMile2/tm2_validation_4.csv"))

load_and_label_data <- function(path){
    label <- parse_path(path)
    df <- read.csv(path, header = TRUE) %>% as.data.frame()
    df$site <- label
    return(df %>% as.data.frame())
}

loaded_validation <- purrr::map(validation_paths, load_and_label_data)
print(is.data.frame(loaded_validation[[1]]))

merged_validation <- Reduce(rbind, loaded_validation)
print(merged_validation)

summary(merged_validation)

head(merged_validation)


write.csv(merged_validation, "figures/merged_validation.csv")

big_plot <- ggplot2::ggplot(data = merged_validation) +
    geom_point(
        aes(
            x=validation_prop,
            y=prediction_prop,
            color=key
        )
    ) + 
    ggplot2::theme_classic() +
    ggplot2::labs(
        title = "Proportions of Human and Machine Label data by Plant Functional type",
    )

windows();big_plot
