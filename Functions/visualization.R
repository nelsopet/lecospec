
# talk to NASA spectral imaging working group r/e gaps

visualize_prediction <- function(filepath, key_file, column){
    require(leaflet)
    color_map <- create_color_map(key_file, column)
    labels <- create_labels(key_file, column)
    layer <- raster::raster(filepath)
    epsg_code <- 3857
    layer_projected <- project_to_epsg(layer, epsg_code, categorical_raster = TRUE)
    map <- leaflet::leaflet() %>%
        leaflet::addTiles("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
             options = providerTileOptions(minZoom = 8, maxZoom = 100)) %>%
        leaflet::addRasterImage(layer, layerId = "layer", colors = color_map) %>%
        leaflet::addLegend("bottomleft", colors = color_map(labels), labels = labels, opacity = 1) %>%
        leaflet.opacity::addOpacitySlider(layerId = "layer")
    return(map)
}

create_color_map <- function(filepath, column){
    levels <- unlist(read.csv(filepath, header = TRUE)[column])
    num_levels <- length(unique(levels))
    palette <- leaflet::colorFactor(grDevices::topo.colors(num_levels), sort(levels))
    return(palette)
}

create_labels <- function(filepath, column){
    return( sort(unique(unlist(read.csv(filepath, header = TRUE)[column]))))
}



plot_quadrat_counts <- function(quadrat_aggregate, filter_missing = TRUE){
    data <- data.frame(quadrat_aggregate)
    if(filter_missing){
        data <- filter_aggregate(quadrat_aggregate)
    }

    plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(
        x=data$key,
    ))

    return ( plot )
}

plot_quadrat_proportions <- function(quadrat_aggregate, filter_missing = TRUE, plot_options = list(
    title = "Prediction and Validation",
    xLabel = "Plant Functional Type",
    yLabel = "Proportion",
    legend = c("Prediction", "Validation"),
    legendTitle = ""
)){
    data <- data.frame(quadrat_aggregate)
    if(filter_missing){
        data <- filter_aggregate(data)
    }

    #print("Checking Predicted Proportions (should sum to 1)")
    #print(sum(data$prediction_prop))
    #print("Checking Validation Proportions (should sum to 1)")
    #print(sum(data$validation_prop))

    data_tall <- tidyr::pivot_longer(
        data = data,
        cols = ends_with("_prop"),
        names_to = "Legend",
        values_to = "Proportion"
    )

    plot <- ggplot2::ggplot(data = data_tall, mapping = ggplot2::aes(
        key,
        Proportion,
        fill = Legend
    )) + 
    theme_minimal() + 
    #scale_fill_manual(values=c("#8aedff", "#a3000b")) + 
    labs(title = plot_options$title, x = plot_options$xLabel, y = plot_options$yLabel) + 
    scale_fill_discrete(name = plot_options$legendTitle, labels = plot_options$legend) +

    geom_bar(stat="identity", position = position_dodge()) + 
    theme(axis.text.x = element_text(angle=90,hjust=1,vjust=1))

    return( plot )
}


define_plot_options <- function(
    title = "",
    xLabel = "Plant Functional Type",
    yLabel = "Proportion",
    legend = c("Prediction", "Validation"),
    legendTitle = "Legend",
    save_location = NULL
){
    return( list(
        title = title,
        xLabel = yLabel,
        yLabel = xLabel,
        legend = legend,
        legendTitle = legendTitle,
        saveLocation = save_location
    ))
}

# plots the raster object wioth ggplot using theme_void


#Aboitic: #ffffff
#Forb: #db2a53
#Graminoid: #c9ae69
#Lichen: #faf87d
#Moss: #7dfaf8
#ShrubDecid: #69876b
#EvergreenShrub: #db2ad2
#TreeBroadleaf: #03fc2c
#TreeConifer: #2ac4db
#Unknown: #000000


fg1_palette <- c(
        "#ffffff",
        "#db2a53",
        "#c9ae69",
        "#faf87d",
        "#7dfaf8",
        "#69876b",
        "#db2ad2",
        "#03fc2c",
        "#2ac4db",
        "#000000"
)

fg1_names <- c(
    "Abiotic",
    "Forb",
    "Graminoid",
    "Lichen",
    "Moss",
    "ShrubDecid",
    "ShrubEvergreen",
    "TreeBroadleaf",
    "TreeConifer",
    "Unknown"
)

fg1_breaks <- c(
    '0','1','2','3','4','5','6','7','8','9'
)

fg1_palette_map <- function(value) {
    # note: value is 0-indexed and R is 1-indexed
    return (fg1_palette[[value]])
}


plot_categorical_raster <- function(ras,  plot_options, colors = fg1_palette) {
    ras_plot <- rasterVis::gplot(ras, 100000000) + 
        theme_classic() +
        labs(
            title = plot_options$title,
            x = plot_options$xLabel,
            y = plot_options$yLabel,
            color = "Plant Type"
            ) + 
        scale_fill_manual(
            breaks = fg1_breaks,
            labels = fg1_names,
            values = fg1_palette, 
            name = "Functional Type"
            ) + 
        geom_tile(aes(
            fill = factor(
                value,
                ordered = FALSE))) + 
        coord_quickmap()


    return(ras_plot)
}



#' Lone line explanation
#'
#' Long Description here
#'
#' @return 
#' @param x
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
#'
plot_agg_results <- function(df, save_file = "Output/results.jpeg") {
    my_plot <- ggplot2::ggplot(data = df) +
        geom_point(aes(df$x, df$y, color=df$z))
    print(my_plot)
    return(my_plot)
}



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

plot_by_pft <- function(df){
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
}

