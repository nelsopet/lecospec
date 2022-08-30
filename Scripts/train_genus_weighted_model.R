source("Functions/lecospectR.R")
library(caret)
library(ranger)
library(xgboost)


genus_validation_df <- read.csv("figures/merged_validation_t.csv", header = TRUE)
total_observations <- sum(genus_validation_df$validation_counts)
weights <- (1/ genus_validation_df$validation_prop)
print(genus_validation_df$validation_prop)

total_by_genus <- genus_validation_df %>% group_by(key) %>% summarise(., total = sum(validation_counts))

total_by_genus$weights <- (5 + total_by_genus$total) / total_observations

