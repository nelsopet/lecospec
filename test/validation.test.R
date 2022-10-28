source("Functions/lecospectR.R")
source("Scripts/validation_defs.R")
source("test/test_defs.R")
# these load validation_df also

template <- read.csv("assets/pft1_template.csv")
print(template)
veg_index_path <- "./test/validation_speclib/D_002_SpecLib_Derivs.csv"
veg_indices <- read.csv(veg_index_path)
veg_indices <- veg_indices[!is.na(veg_indices$Functional_group1),]
test_df <- veg_indices$Functional_group1 %>% as.data.frame()
print(colnames(test_df))
colnames(test_df) <- c("z")

test_summary <- get_prediction_distribution(test_df)
print(summary(test_summary))

print("Input Data Distribution")
print(veg_indices$Functional_group1 %>% as.factor() %>% table())
validation <- veg_indices$Functional_group1 %>% 
    as.factor() %>% 
    table() %>% 
    as.data.frame()
print(colnames(validation))
colnames(validation) <- c("Plant", "cover_prn")
validation$cover_prn <- 100 * validation$cover_prn / sum(validation$cover_prn)
print("Aggregating")

result_df <- aggregate_result_template(
    test_summary,
    validation,
    template
    )
print("Validation Distribution")
print(result_df)
print("Aggregating")

result_df <- aggregate_result_template(
    test_summary,
    validation_df,
    template
    )
print("Validation Distribution")
print(result_df)
write.csv(result_df, file = "./test/validation_test_aggregate.csv")

