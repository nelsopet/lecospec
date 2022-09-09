source("./Functions/lecospectR.R")
require(sf)
require(stringr)
library(plotly)
library(dplyr)

aggregation_path <- "figures/aggregator.csv"

# run the
aggregated_df <- read.csv(aggregation_path)
ordered_df <- aggregated_df[order(aggregated_df$validation_prop), ]

bar_graph <- plot_quadrat_proportions(aggregated_df)

windows()
bar_graph



validation_paths <- list(
    "figures/BisonGulch/actually_normed_1.csv",
    "figures/BisonGulch/actually_normed_2.csv",
    "figures/BisonGulch/actually_normed_3.csv",
    "figures/BisonGulch/actually_normed_4.csv",
    "figures/BisonGulch/actually_normed_5.csv",
    "figures/BisonGulch/actually_normed_6.csv",
    "figures/BisonGulch/actually_normed_7.csv",
    "figures/BisonGulch/actually_normed_8.csv",
    "figures/BisonGulch/actually_normed_9.csv",
    "figures/Chatanika/actually_normed_1.csv",
    "figures/Chatanika/actually_normed_2.csv",
    "figures/Chatanika/actually_normed_3.csv",
    "figures/Chatanika/actually_normed_4.csv",
    "figures/Chatanika/actually_normed_5.csv",
    "figures/Chatanika/actually_normed_6.csv",
    "figures/Chatanika/actually_normed_7.csv",
    "figures/Chatanika/actually_normed_8.csv",
    "figures/Chatanika/actually_normed_9.csv",
    "figures/Chatanika/actually_normed_10.csv",
    "figures/Chatanika/actually_normed_11.csv",
    "figures/EightMile/actually_normed_1.csv",
    "figures/EightMile/actually_normed_2.csv",
    "figures/EightMile/actually_normed_3.csv",
    "figures/EightMile/actually_normed_4.csv",
    "figures/EightMile/actually_normed_5.csv",
    "figures/EightMile/actually_normed_6.csv",
    "figures/EightMile/actually_normed_7.csv",
    "figures/EightMile/actually_normed_8.csv",
    "figures/EightMile/actually_normed_9.csv",
    "figures/EightMile/actually_normed_10.csv",
    "figures/EightMile/actually_normed_11.csv",
    "figures/MurphyDome/Part1/actually_normed_1.csv",
    "figures/MurphyDome/Part1/actually_normed_2.csv",
    "figures/MurphyDome/Part2/actually_normed_1.csv",
    "figures/MurphyDome/Part2/actually_normed_2.csv",
    "figures/MurphyDome/Part2/actually_normed_3.csv",
    "figures/MurphyDome/Part2/actually_normed_4.csv",
    "figures/MurphyDome/Part3/actually_normed_1.csv",
    "figures/MurphyDome/Part3/actually_normed_2.csv",
    "figures/MurphyDome/Part3/actually_normed_3.csv",
    "figures/MurphyDome/Part3/actually_normed_4.csv",
    "figures/MurphyDome/Part3/actually_normed_5.csv",
    "figures/twelveMile1/actually_normed_1.csv",
    "figures/twelveMile1/actually_normed_2.csv",
    "figures/twelveMile1/actually_normed_3.csv",
    "figures/twelveMile1/actually_normed_4.csv",
    "figures/twelveMile1/actually_normed_5.csv",
    "figures/twelveMile2/actually_normed_1.csv",
    "figures/twelveMile2/actually_normed_2.csv",
    "figures/twelveMile2/actually_normed_3.csv",
    "figures/twelveMile2/actually_normed_4.csv"
)


parse_path <- function(path) {
    split_path <- strsplit(path, split = "/", fixed = TRUE)
    return(split_path[[1]][[2]])
}

# print(parse_path("figures/twelveMile2/tm2_validation_4.csv"))

load_and_label_data <- function(path) {
    label <- parse_path(path)
    df <- read.csv(path, header = TRUE) %>% as.data.frame()
    df$site <- label
    return(df %>% as.data.frame())
}

loaded_validation <- purrr::map(validation_paths, load_and_label_data)

print(head(load_and_label_data("figures/BisonGulch/actually_normed_1.csv"), 10))
print(head(load_and_label_data("figures/BisonGulch/actually_normed_2.csv"), 10))

merged_validation <- Reduce(rbind, loaded_validation)
print(merged_validation)

summary(merged_validation)

head(merged_validation, 20)


write.csv(merged_validation, "figures/merged_validation_an.csv")

big_plot <- ggplot2::ggplot(data = merged_validation) +
    geom_point(
        aes(
            x = validation_prop,
            y = prediction_prop,
            color = key
        )
    ) +
    ggplot2::theme_classic() +
    ggplot2::labs(
        title = "Proportions of Human and Machine Label data by Plant Functional type",
    )

windows()
big_plot



build_table <- function(df, path = "./figures/validation_table_new.csv"){
    csv_text <- ""
    site_names <- unique(df$site)
    pfts <- unique(df$key)
    header <- ""
    for(col_name in site_names){
        header <- paste(header, col_name, sep = ",")
    }
    csv_text <- paste0(header, "\n")

    for(pft in pfts){
        row_text <- pft
        filtered_df <- df[df$key == pft,]
        for(site in site_names){
            df_site_pft <- filtered_df[filtered_df$site == site, ]
            regression_model <- lm(prediction_prop ~ validation_prop, df_site_pft)
            r_squared <- summary(regression_model)$r.squared
            row_text <- paste(row_text, r_squared, sep = ",")
        }
        csv_text <- paste(csv_text, row_text, sep = "\n")
        
    }
    print(csv_text)
    fileC <- file(path)
    writeLines(csv_text, fileC)
    close(fileC)
}

print(colnames(merged_validation))

build_table(merged_validation)

table_df <- read.csv("./figures/validation_table_new.csv", header = TRUE)
print(table_df)
summary(table_df)

data_path <- "figures/merged_validation_t.csv"

df <- read.csv(data_path, header = TRUE)

summary(df)
print(colnames(df))

create_plot <- function(df, pft, legend = FALSE){
    filtered_df <- df[df$key == pft,]
    return(
        plot_ly(filtered_df) %>%
            add_markers(
                x = ~validation_prop,
                y = ~prediction_prop,
                color = ~site,
                data = filtered_df,
                showlegend = legend
            ) %>% plotly::layout(
                title = pft,
                xaxis = list(
                    title = "Ground Truth Proportion"
                ),
                yaxis = list(
                    title = "Predicted Proportion"
                ),
                annotations = list(x = 0.0 , y = 1.075, text = pft, showarrow = F, 
                    xref='paper', yref='paper')

            )
    )
}

df <- df %>% group_by(site)
plot_abiotic <- create_plot(df, "Abiotic", legend = TRUE)
plot_forb <- create_plot(df, "Forb")
plot_graminoid <- create_plot(df, "Graminoid")
plot_lichen <- create_plot(df, "Lichen")
plot_moss <- create_plot(df, "Moss")
plot_shrub_bro <- create_plot(df, "ShrubDecid")
plot_shrub_eve <- create_plot(df, "ShrubEvergreen")
plot_tree_bro <- create_plot(df, "TreeBroadleaf")
plot_tree_eve <- create_plot(df, "TreeConifer")

fig <- plotly::subplot(
    plot_abiotic,
    plot_forb,
    plot_graminoid,
    plot_lichen,
    plot_moss,
    plot_shrub_bro,
    plot_shrub_eve,
    plot_tree_bro,
    plot_tree_eve,
    nrows = 3,
    margin = 0.05)
fig <- fig %>% plotly::layout(
    title = "Prediction and Ground Truth Labels",
    hovermode = FALSE
)

fig_save_loc <- paste(tempfile('plotly_fig'), 'html', sep = '.')
htmlwidgets::saveWidget(fig, fig_save_loc, selfcontained = FALSE)
browseURL(fig_save_loc)

