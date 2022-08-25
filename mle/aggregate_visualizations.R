library(tidyverse)
library(ggplot2)


merged_validation_path <- "figures/merged_validation.csv"
validation_df <- read.csv(merged_validation_path, header = TRUE)
plt <- ggplot2::ggplot(data = validation_df) + 
    geom_point(
        aes(
            
        )
    )
windows();plt
