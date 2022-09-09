library(plotly)
library(dplyr)

data_path <- "figures/merged_validation_an.csv"

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
                annotations = list(x = 0.0 , y = 1.1, text = pft, showarrow = F, 
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
    hovermode = TRUE
)

fig_save_loc <- paste(tempfile('plotly_fig'), 'html', sep = '.')
htmlwidgets::saveWidget(fig, fig_save_loc, selfcontained = FALSE)
browseURL(fig_save_loc)


build_table <- function(df){
    csv_text <- ""
    header <- ""
    filtered_df <- df[df$key == pft,]
    for(col_name in colnames(df)){
        header <- paste(header, col_name, sep = ",")
    }
    csv_text <- paste0(header, "\n")

    for(pft in df %>% unique()){
        
    }
}


