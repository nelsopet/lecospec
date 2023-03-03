source("Functions/lecospectR.R")

NOISE_POWER <- 0.01
target_path <- "Data/Ground_Validation/training/labels.json"

train_base_path <- "Data/Ground_Validation/training/base_exp.csv"
train_norm_path <- "Data/Ground_Validation/training/normed_exp.csv"
train_scale_path <- "Data/Ground_Validation/training/scaled_exp.csv"

targets <- rjson::fromJSON(file = target_path) %>% as.factor()
base_weights <- rjson::fromJSON(file = "mle/fg1_weights.json") %>% as.list()
posterior_weights <- purrr::map(targets, function(x){
    return(1 / base_weights[[x]])
})


train_base <- read.csv(train_base_path)
train_norm <- read.csv(train_norm_path)
train_scale <- read.csv(train_scale_path)

print(targets)

counts <- targets %>% table() %>% as.list()
print(counts)
weights <- purrr::map(targets, function(x){
    return (1 / counts[[x]])
})

metadata_cols <- c(
    "X",
    "UID",
    "ScanNum",
    "sample_name",
    "PFT",
    "FncGrp1"
)
data_cols <- setdiff(colnames(train_base), metadata_cols)

noised_base <- add_noise(train_base, NOISE_POWER, used_cols = data_cols)
noised_norm <- add_noise(train_norm, NOISE_POWER, used_cols = data_cols)
noised_scale <- add_noise(train_scale, NOISE_POWER, used_cols = data_cols)

print(length(posterior_weights))
print(nrow(train_scale))
###################################################
##     norm models
###################################################
# 3 models with norm and noise
norm <- ranger::ranger(
    importance = "impurity",
    classification = TRUE,
    x = train_norm[, data_cols],
    y = targets
)
save(norm,file = "mle/models/norm_img_exp.rda")


# 3 models with norm and noise
norm_pw <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = train_norm[, data_cols],
    y = targets
)
save(norm_pw,file = "mle/models/norm_img_pw_exp.rda")

# 3 models with norm and noise
norm_w <- ranger::ranger(
    importance = "impurity",
    case.weights = weights %>% as.numeric(),
    classification = TRUE,
    x = train_norm[, data_cols],
    y = targets
)
save(norm_w, file = "mle/models/norm_img_w_exp.rda")

###################################################
##     base models
###################################################
# 3 models with norm and noise
base <- ranger::ranger(
    importance = "impurity",
    #case.weights = posterior_weights,
    classification = TRUE,
    x = train_base[, data_cols],
    y = targets
)
save(base, file = "mle/models/base_img_exp.rda")


# 3 models with norm and noise
base_pw <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = train_base[, data_cols],
    y = targets
)
save(base,file = "mle/models/base_img_pw_exp.rda")

# 3 models with norm and noise
base_w <- ranger::ranger(
    importance = "impurity",
    case.weights = weights %>% as.numeric(),
    classification = TRUE,
    x = train_base[, data_cols],
    y = targets
)
save(base_w, file = "mle/models/base_img_w_exp.rda")


###################################################
##     scaled models
###################################################
# 3 models with norm and noise
scale <- ranger::ranger(
    importance = "impurity",
    #case.weights = posterior_weights,
    classification = TRUE,
    x = train_scale[, data_cols],
    y = targets
)
save(scale, file = "mle/models/scale_img_exp.rda")


# 3 models with norm and noise
scale_pw <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = train_scale[, data_cols],
    y = targets
)
save(scale,file = "mle/models/scale_img_pw_exp.rda")

# 3 models with norm and noise
scale_w <- ranger::ranger(
    importance = "impurity",
    case.weights = weights %>% as.numeric(),
    classification = TRUE,
    x = train_scale[, data_cols],
    y = targets
)
save(scale_w, file = "mle/models/scale_img_w_exp.rda")

###################################################
##     norm models noised
###################################################
# 3 models with norm and noise
norm_noise <- ranger::ranger(
    importance = "impurity",
    #case.weights = posterior_weights,
    classification = TRUE,
    x = noised_norm[, data_cols],
    y = targets
)
save(norm_noise,file = "mle/models/norm_noise_img_exp.rda")


# 3 models with norm and noise
norm_noise_pw <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = noised_norm[, data_cols],
    y = targets
)
save(norm_noise_pw,file = "mle/models/norm_noise_img_pw_exp.rda")

# 3 models with norm and noise
norm_noise_w <- ranger::ranger(
    importance = "impurity",
    case.weights = weights %>% as.numeric(),
    classification = TRUE,
    x = noised_norm[, data_cols],
    y = targets
)
save(norm_noise_w, file = "mle/models/norm_noise_img_w_exp.rda")

###################################################
##     base models noised
###################################################
# 3 models with norm and noise
base_noise <- ranger::ranger(
    importance = "impurity",
    #case.weights = posterior_weights,
    classification = TRUE,
    x = noised_base[, data_cols],
    y = targets
)
save(base_noise, file = "mle/models/base_noise_img_exp.rda")


# 3 models with norm and noise
base_noise_pw <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = noised_base[, data_cols],
    y = targets
)
save(base_noise,file = "mle/models/base_noise_img_pw_exp.rda")

# 3 models with norm and noise
base_noise_w <- ranger::ranger(
    importance = "impurity",
    case.weights = weights %>% as.numeric(),
    classification = TRUE,
    x = noised_base[, data_cols],
    y = targets
)
save(base_noise_w, file = "mle/models/base_noise_img_w_exp.rda")


###################################################
##     scaled models noised
###################################################
# 3 models with norm and noise
scale_noise <- ranger::ranger(
    importance = "impurity",
    #case.weights = posterior_weights,
    classification = TRUE,
    x = noised_scale[, data_cols],
    y = targets
)
save(scale_noise, file = "mle/models/scale_noise_img_exp.rda")


# 3 models with norm and noise
scale_noise_pw <- ranger::ranger(
    importance = "impurity",
    case.weights = posterior_weights %>% as.numeric(),
    classification = TRUE,
    x = noised_scale[, data_cols],
    y = targets
)
save(scale_noise,file = "mle/models/scale_noise_img_pw_exp.rda")

# 3 models with norm and noise
scale_noise_w <- ranger::ranger(
    importance = "impurity",
    case.weights = weights %>% as.numeric(),
    classification = TRUE,
    x = noised_scale[, data_cols],
    y = targets
)
save(scale_noise_w, file = "mle/models/scale_noise_img_w_exp.rda")







