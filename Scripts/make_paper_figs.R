library(tidyverse)

aggregated_data <- read.csv("figures/merged_validation_sw.csv", header = TRUE)

agg <- aggregated_data %>% group_by(key) %>% summarise_at(.funs = sum, .vars = c("validation_counts"))
print(agg)
