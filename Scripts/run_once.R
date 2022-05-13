source("./Functions/lecospectR.R")

# build validation templates
print(pft1_template)
pft1_template <- build_validation_template(pft_table) 
pft2_template <- build_validation_template(pft_table, col=4)
genus_template <- build_validation_template(pft_table, col=3)
species_template <- build_validation_template(pft_table, col=2)
print(species_template)

write.csv(pft1_template, "./pft1_template.csv")
write.csv(pft2_template, "./pft2_template.csv")
write.csv(genus_template, "./genus_template.csv")
write.csv(species_template, "./species_template.csv")
print(pft1_template)


# build the plant functional type list
pft_conv <- build_adjacency_list(pft_path)
print(pft_conv)

# build the species adjacency list
pft_path <- "Data/species_table_new.csv"
pft_table <- read.csv(pft_path)

pft_conv <- build_adjacency_list(pft_path)
print(str(pft_conv))

#save the converter to JSON for reuse
jsonData <- rjson::toJSON(pft_conv)
write(jsonData, "./pft_adj_list.json")


crs_from_epsg("NAD83")
