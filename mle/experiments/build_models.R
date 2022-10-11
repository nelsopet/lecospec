save_paths <- dir(
    "./mle/experiments/ground_trained/",
    full.names = TRUE, include.dirs=TRUE
    )
print(save_paths)


# Read the training data
target_path <- "mle/y.csv"
n4path <- "mle/no_noise_no_norm.csv" 
n2path <- "mle/training_data_noise_norm.csv"
noise_no_norm_path <- "mle/noise_no_norm.csv"
no_noise_norm_path <- "mle/no_noise_norm.csv"
scaled_clean_path <- "mle/scaled_ground_no_noise.csv"
scaled_noise_path <- "mle/scaled_ground_noise.csv"



no_noise_no_norm <- subset(read.csv(n4path), select = -c(X))
noise_no_norm <- subset(read.csv(noise_no_norm_path), select = -c(X))
no_noise_norm <- subset(read.csv(no_noise_norm_path), select = -c(X))
noise_norm <- subset(read.csv(n2path), select = -c(X))

scaled_noise <- subset(read.csv(scaled_noise_path), select = -c(X))
scaled_clean <- subset(read.csv(scaled_clean_path), select = -c(X))
print(colnames(noise_no_norm))


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

##########################################################
#     Model Training
##########################################################

# 3 models with norm and noise
norm_noise_post <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights,
    classification = TRUE,
    x = noise_norm,
    y = targets
)
save(norm_noise_post,file = "mle/models/norm_noise_post.rda")

norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    classification = TRUE,
    x = noise_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/norm_noise_no_weight.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = noise_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/norm_noise_prior.rda")

# 3 models with no norm, but noise
no_norm_noise_post<- norm_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = noise_no_norm,
    y = targets
)
save(norm_noise_post, file = "mle/models/no_norm_noise_post.rda")

norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = noise_no_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/no_norm_noise_no_weight.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = noise_no_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/no_norm_noise_prior.rda")

# 3 models with no noise, but normalized
norm_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = no_noise_norm,
    y = targets
)
save(norm_noise_post, file = "mle/models/norm_no_noise_post.rda")

norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = no_noise_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/norm_no_noise_no_weight.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = no_noise_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/norm_no_noise_prior.rda")

# 3 raw models (neither norm nor noise)
norm_noise_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = no_noise_no_norm,
    y = targets
)
save(norm_noise_post, file = "mle/models/no_norm_no_noise_post.rda")

norm_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = no_noise_no_norm,
    y = targets
)
save(norm_noise_no_weight, file = "mle/models/no_norm_no_noise_no_weight.rda")

norm_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = no_noise_no_norm,
    y = targets
)
save(norm_noise_prior, file = "mle/models/no_norm_no_noise_prior.rda")


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
save(scaled_noise_post, file = "mle/models/scaled_noise_post.rda")

scaled_noise_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_no_weight, file = "mle/models/scaled_noise_no_weight.rda")

scaled_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_prior, file = "mle/models/scaled_noise_prior.rda")

# clean
scaled_clean_post <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = posterior_weights,
    classification = TRUE,
    x = scaled_clean,
    y = targets
)
save(scaled_clean_post, file = "mle/models/scaled_clean_post.rda")

scaled_clean_no_weight <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    classification = TRUE,
    x = scaled_clean,
    y = targets
)
save(scaled_clean_no_weight, file = "mle/models/scaled_clean_no_weight.rda")

scaled_noise_prior <- ranger::ranger(
    importance = "impurity",
    replace = TRUE,
    case.weights = prior_weights,
    classification = TRUE,
    x = scaled_noise,
    y = targets
)
save(scaled_noise_prior, file = "mle/models/scaled_clean_prior.rda")
