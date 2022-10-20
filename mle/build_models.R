source("Functions/lecospectR.R")

save_path_names <- dir(
    "./mle/experiments/ground_trained/",
    include.dirs=TRUE
    )

base_path <- "./mle/experiments/ground_expanded/"
for(filename in save_path_names){
    new_path <- paste0(base_path, filename)
    dir.create(new_path)
}

file_paths <- dir(base_path, full.names=TRUE, include.dirs=TRUE)



print(save_path_names)


# Read the training data
target_path <- "Data/Ground_Validation/training/labels.csv"
n4path <- "mle/no_noise_no_norm._expcsv"
n2path <- "mle/training_data_noise_norm_exp.csv"
noise_no_norm_path <- "mle/noise_no_norm_exp.csv"
no_noise_norm_path <- "mle/no_noise_norm_exp.csv"
scaled_clean_path <- "mle/scaled_ground_no_noise_exp.csv"
scaled_noise_path <- "mle/scaled_ground_noise_exp.csv"


speclib_filepath <- "Data/D_002_SpecLib_Derivs.csv"
speclib <- read.csv(speclib_filepath, header = TRUE)


#print(nrow(no_noise_no_norm))
no_noise_no_norm <- subset(read.csv(n4path), select = -c(X))
noise_no_norm <- subset(read.csv(noise_no_norm_path), select = -c(X))
no_noise_norm <- subset(read.csv(no_noise_norm_path), select = -c(X))
noise_norm <- subset(read.csv(n2path), select = -c(X))


scaled_noise <- subset(read.csv(scaled_noise_path), select = -c(X))
scaled_clean <- subset(read.csv(scaled_clean_path), select = -c(X))
print(colnames(noise_no_norm))

targets <- speclib$Functional_group1
targets <- read.csv(target_path, header = TRUE)$x %>% as.factor()
# Read the training weights
prior_weights_path <- "mle/weights.json"
posterior_weights_path <- "mle/pweights.json"

prior_weights <- rjson::fromJSON(file = prior_weights_path)
print(prior_weights)
print(length(prior_weights))
print(length(targets))
posterior_weights <- rjson::fromJSON(file = posterior_weights_path)
print(length(posterior_weights))

print(summary(no_noise_no_norm))
##########################################################
#     Model Training
##########################################################

# 3 models with norm and noise
norm_noise_post <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = noise_norm,
    y = targets %>% as.factor()
)
print(norm_noise_post$forest$independent.variable.names)
print(colnames(noise_norm))
save(norm_noise_post,file = "mle/models/norm_noise_post_exp.rda")
results <- check_output_distribution(
    noise_norm, 
    norm_noise_post, 
    targets %>% as.factor())
print(results)


norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    classification = TRUE,
    x = noise_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/norm_noise_no_weight_exp.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = noise_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/norm_noise_prior_exp.rda")

# 3 models with no norm, but noise
no_norm_noise_post<- norm_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = noise_no_norm,
    y = targets
)
save(norm_noise_post, file = "mle/models/no_norm_noise_post_exp.rda")

norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = noise_no_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/no_norm_noise_no_weight_exp.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = noise_no_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/no_norm_noise_prior_exp.rda")

# 3 models with no noise, but normalized
norm_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = no_noise_norm,
    y = targets
)
save(norm_noise_post, file = "mle/models/norm_no_noise_post_exp.rda")

norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = no_noise_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/norm_no_noise_no_weight_exp.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = no_noise_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/norm_no_noise_prior_exp.rda")

# 3 raw models (neither norm nor noise)
norm_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = no_noise_no_norm,
    y = targets
)
save(norm_noise_post, file = "mle/models/no_norm_no_noise_post_exp.rda")

norm_noise_no_weight <- ranger::ranger(
    num.trees = 1000,
    mtry = 51,
    max.depth = 11,
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = no_noise_no_norm,
    y = targets
)
print(norm_noise_no_weight$prediction.error)
var_imp <- sort(norm_noise_no_weight$variable.importance, decreasing=TRUE)
print(names(var_imp)[1:30])
save(norm_noise_no_weight, file = "mle/models/no_norm_no_noise_no_weight_exp.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = no_noise_no_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/no_norm_no_noise_prior_exp.rda")


##########################################################
#     Scaled models
##########################################################

scaled_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_post, file = "mle/models/scaled_noise_post_exp.rda")

scaled_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    mtry = 51,
    replace = TRUE,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_no_weight, file = "mle/models/scaled_noise_no_weight_exp.rda")

scaled_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_prior, file = "mle/models/scaled_noise_prior_exp.rda")

# clean
scaled_clean_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = scaled_clean,
    y = targets
)
save(scaled_clean_post, file = "mle/models/scaled_clean_post_exp.rda")

scaled_clean_no_weight <- ranger::ranger(
    num.trees = 1000,
    mtry = 51,
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = scaled_clean,
    y = targets
)
print(scaled_clean_no_weight$prediction.error)
print(scaled_clean_no_weight$variable.importance)
save(scaled_clean_no_weight, file = "mle/models/scaled_clean_no_weight_exp.rda")

scaled_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_prior, file = "mle/models/scaled_clean_prior_exp.rda")

crazy_model <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = no_noise_no_norm[, names(var_imp)[1:35]],
    y = targets
)

print(crazy_model$prediction.error)

raster::beginCluster()
cl <- raster::getCluster()

results <- validate_model(
    crazy_model,
    "mle/experiments/crazy/",
    normalize_input = FALSE,
    scale_input = FALSE,
    cluster = cl)

raster::endCluster()
aggregated_results <- aggregate_results("mle/experiments/crazy/") %>% as.data.frame()
try(

    plot_by_pft(
        aggregated_results,
        save_path = "mle/experiments/crazy/aggregates.html",
        open = FALSE
    )
    )
try(

    write_validation_table(
        aggregated_results,
        save_path = "mle/experiments/crazy/table.html",
        open = FALSE
        )
    )

