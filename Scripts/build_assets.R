source("./Functions/lecospectR.R")

# build the species adjacency list
pft_path <- "Data/SpeciesTable_20230417.csv"
pft_table <- read.csv(pft_path)

# build validation templates
print(pft1_template)
pft0_template <- build_validation_template(pft_table, col = 6)
pft1_template <- build_validation_template(pft_table, col = 5) 
pft2_template <- build_validation_template(pft_table, col = 4)
genus_template <- build_validation_template(pft_table, col = 3)
species_template <- build_validation_template(pft_table, col = 2)
print(pft0_template)

write.csv(pft0_template, "./assets/pft0_template.csv")
write.csv(pft1_template, "./assets/pft1_template.csv")
write.csv(pft2_template, "./assets/pft2_template.csv")
write.csv(genus_template, "./assets/pft3_template.csv")
write.csv(species_template, "./assets/pft4_template.csv")
print(pft0_template)


# build the plant functional type list
pft_conv <- build_adjacency_list(pft_path)
print(pft_conv)

#save the converter to JSON for reuse
jsonData <- rjson::toJSON(pft_conv)
write(jsonData, "./assets/pft_adj_list.json")
