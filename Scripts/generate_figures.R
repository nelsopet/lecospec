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
    "figures/BisonGulch/validation_redux_1.csv",
    "figures/BisonGulch/validation_redux_2.csv",
    "figures/BisonGulch/validation_redux_3.csv",
    "figures/BisonGulch/validation_redux_4.csv",
    "figures/BisonGulch/validation_redux_5.csv",
    "figures/BisonGulch/validation_redux_6.csv",
    "figures/BisonGulch/validation_redux_7.csv",
    "figures/BisonGulch/validation_redux_8.csv",
    "figures/BisonGulch/validation_redux_9.csv",
    "figures/Chatanika/validation_redux_1.csv",
    "figures/Chatanika/validation_redux_2.csv",
    "figures/Chatanika/validation_redux_3.csv",
    "figures/Chatanika/validation_redux_4.csv",
    "figures/Chatanika/validation_redux_5.csv",
    "figures/Chatanika/validation_redux_6.csv",
    "figures/Chatanika/validation_redux_7.csv",
    "figures/Chatanika/validation_redux_8.csv",
    "figures/Chatanika/validation_redux_9.csv",
    "figures/Chatanika/validation_redux_10.csv",
    "figures/Chatanika/validation_redux_11.csv",
    "figures/EightMile/validation_redux_1.csv",
    "figures/EightMile/validation_redux_2.csv",
    "figures/EightMile/validation_redux_3.csv",
    "figures/EightMile/validation_redux_4.csv",
    "figures/EightMile/validation_redux_5.csv",
    "figures/EightMile/validation_redux_6.csv",
    "figures/EightMile/validation_redux_7.csv",
    "figures/EightMile/validation_redux_8.csv",
    "figures/EightMile/validation_redux_9.csv",
    "figures/EightMile/validation_redux_10.csv",
    "figures/EightMile/validation_redux_11.csv",
    "figures/MurphyDome/Part1/validation_redux_1.csv",
    "figures/MurphyDome/Part1/validation_redux_2.csv",
    "figures/MurphyDome/Part2/validation_redux_1.csv",
    "figures/MurphyDome/Part2/validation_redux_2.csv",
    "figures/MurphyDome/Part2/validation_redux_3.csv",
    "figures/MurphyDome/Part2/validation_redux_4.csv",
    "figures/MurphyDome/Part3/validation_redux_1.csv",
    "figures/MurphyDome/Part3/validation_redux_2.csv",
    "figures/MurphyDome/Part3/validation_redux_3.csv",
    "figures/MurphyDome/Part3/validation_redux_4.csv",
    "figures/MurphyDome/Part3/validation_redux_5.csv",
    "figures/twelveMile1/validation_redux_1.csv",
    "figures/twelveMile1/validation_redux_2.csv",
    "figures/twelveMile1/validation_redux_3.csv",
    "figures/twelveMile1/validation_redux_4.csv",
    "figures/twelveMile1/validation_redux_5.csv",
    "figures/twelveMile2/validation_redux_1.csv",
    "figures/twelveMile2/validation_redux_2.csv",
    "figures/twelveMile2/validation_redux_3.csv",
    "figures/twelveMile2/validation_redux_4.csv"
)


parse_path <- function(path){
    split_path <- strsplit(path, split="/", fixed = TRUE)
    return(split_path[[1]][[2]])
}

#print(parse_path("figures/twelveMile2/tm2_validation_4.csv"))

load_and_label_data <- function(path){
    label <- parse_path(path)
    df <- read.csv(path, header = TRUE) %>% as.data.frame()
    df$site <- label
    return(df %>% as.data.frame())
}

loaded_validation <- purrr::map(validation_paths, load_and_label_data)

print(head(load_and_label_data("figures/BisonGulch/validation_redux_1.csv"), 10))
print(head(load_and_label_data("figures/BisonGulch/validation_redux_2.csv"), 10))

merged_validation <- Reduce(rbind, loaded_validation)
print(merged_validation)

summary(merged_validation)

head(merged_validation, 20)


write.csv(merged_validation, "figures/merged_validation_r.csv")

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
