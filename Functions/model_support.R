#' Gets the variable names required for the given model (via UseMethod hook)
#' 
#' Long
#' 
#' @param ml_model the model whos input paramaters are required
#' @param ... additional parameters passed to composite function (via UseMethod hook)
#' @return a character vector of variable names for the model
#' @export
#' 
get_var_names <- function(ml_model, ...) {
    return(UseMethod("get_var_names", ml_model))
}

#' default method for the usemethod hook
#' 
#' Long
#' 
#' @param ml_model the model
#' @param ... additional arguments passed to get_var_names
#' @return
#' @export
#' 
get_var_names.default <- function(ml_model, ...){
    return(
        read.csv(
            "assets/vegIndicesUsed.csv",
            header = TRUE
        )$x %>% 
            as.character())
}

#' Gets the required variables for a ranger model
#' 
#' Gets the required packages from the ranger model.
#' It is calledd automatically from get_var_names
#' 
#' @param
#' @return
#' @export
#' 
get_var_names.ranger <- function(ml_model, ...){
    Vars_names <- c(ml_model$forest$independent.variable.names)
    return( gsub("^X", "", Vars_names))
}

#' Gets the required variable names from a LSModel instance
#'
#' called automatically from the get_var_names functon.  
#' Gets the character vector of variable names needed for the 
#' model to run.
#'
#' @param ml_model The LSModel instance
#' @return a character vector of variable names
#' @export
#' 
get_var_names.LSModel <- function(ml_model, ...){
    return(ml_model$indices)
}

#' get the required vegetation indices from a machine learning model
#' 
#' A Prototype (S4) method for getting the names of the vegetation indices
#' required to run a model.
#' 
#' @param
#' @return
#' @export
#' 
get_required_veg_indices <- function(ml_model){
    return(UseMethod("get_required_veg_indices", ml_model))
}

#' gets the vegetation indices required for a LSModel instance
#' 
#' 
#' 
#' @param ml_model a LSModel instance
#' @return a character vector of vegetation index names
#' @export
#' 
get_required_veg_indices.LSModel <- function(ml_model, ...){
    return(ml_model$indices)
}

#' Gets the required vegetation indices for a ranger model
#' 
#' Long
#' 
#' @param ml_model
#' @param ... other arguments passed from
#' get_required_veg_indices
#' @return
#' @export
get_required_veg_indices.ranger <- function(ml_model, ...) {
    var_names <- get_var_names(ml_model)

    if(is.null(var_names)){
        var_names <- read.csv(
            "assets/vegIndicesUsed.csv",
            header = TRUE
        )$x %>% 
        as.character()
    }
     av <- sort(
                c(
                    "NDVI","OSAVI","SAVI","MTVI","NDWI","PWI",
                    "MSI", "SRWI","GMI1","GMI2","MCARI","TVI",
                    "Vogelmann4","Boochs","Boochs2",
                    "CARI","CI","Carter","Carter2","Carter3","Carter4",
                    "Carter5","Carter6","Datt","Datt2","Datt3","Datt4",
                    "Datt5","Datt6","DD","DDn","D1","D2","EVI","EGFR","EGFN",
                    "GI","Gitelson","Gitelson2","Green NDVI","MCARI/OSAVI",
                    "MCARI2","MCARI2/OSAVI2","mNDVI","mND705","Maccioni",
                    "mREIP","MSAVI","mSR","mSR705","mSR2","MTCI","NDVI2",
                    "NDVI3","NPCI","OSAVI2","RDVI","REP_LE","REP_Li",
                    "SIPI","SPVI","SR","SR1","SR2","SR3","SR4","SR5","SR6",
                    "SR7", "SR8","SRPI","Sum_Dr1","Sum_Dr2","TCARI","TCARI2",
                    "TCARI/OSAVI","TCARI2/OSAVI2","Vogelmann","NDLI",
                    "Vogelmann2","Vogelmann3","PRI","CAI","NDNI",
                    "PSSR", "PSND", "CRI1", "CRI2", "CRI3",
                    "CRI4", "MPRI", "PRI*CI2", "CI2", "PSRI", "ClAInt", 
                    "TGI", "PRI_norm","PARS","DPI","Datt7","Datt8",
                    "GDVI_2","GDVI_3","GDVI_4","LWVI1","LWVI2",
                    "DWSI1","DWSI2","DWSI3","DWSI4","DWSI5",
                    "SWIR FI", "SWIR LI", "SWIR SI", "SWIR VI"
                )
            )
        # get the items in both vectors 
        # (i.e. veg indeices used by the classifier)
    var_names <- gsub("MCARI2OSAVI2", "MCARI2/OSAVI2", var_names)
    var_names <- gsub("MCARIOSAVI", "MCARI/OSAVI", var_names)
    var_names <- gsub("TCARIOSAVI", "TCARI/OSAVI", var_names)
    var_names <- gsub("TCARI2OSAVI2", "TCARI2/OSAVI2", var_names)
    var_names <- gsub("PRICI2", "PRI*CI2", var_names)
    var_names <- gsub("SWIRFI", "SWIR FI", var_names)
    var_names <- gsub("SWIRLI", "SWIR LI", var_names)
    var_names <- gsub("SWIRSI", "SWIR SI", var_names)
    var_names <- gsub("SWIRVI", "SWIR VI", var_names)
    var_names <- gsub("GDVI2", "GDVI_2", var_names)
    var_names <- gsub("GDVI3", "GDVI_3", var_names)
    var_names <- gsub("GDVI4", "GDVI_4", var_names)
    var_names <- gsub("GreenNDVI", "Green NDVI", var_names)
    var_names <- gsub("PRInorm", "PRI_norm", var_names)
    var_names <- gsub("REPLE", "REP_LE", var_names)
    var_names <- gsub("REPLi", "REP_Li", var_names)
    var_names <- gsub("SumDr1", "Sum_Dr1", var_names)
    var_names <- gsub("SumDr2", "Sum_Dr2", var_names)



    veg_indices <- intersect(av, var_names)

    return(veg_indices)
}

#' default list of vegetation indices if model type is not supported
#' 
#' This loads the default list of vegetation indices from disk
#' The default list can be modified in the lecospec assets folder, 
#' see assets/vegIndicesUsed.csv. This file, by default, contains all 
#' vegetation indices supported by the package `hsdar`.
#' 
#' @param ml_model  The machine learning mode (can be any object, actually)
#' @return a character vector of vegetation indices
#' @export
#' 
get_required_veg_indices.default <- function(ml_model, ...) {
    return(read.csv("assets/vegIndicesUsed.csv", header = TRUE)$x)
}



#' Applies a model to the provided data
#'
#' The apply model function is an S4 prototype for applying a model
#' to a dataframe of observations.  This method, unlike `predict`, 
#' always returns a data frame with a predicatable schema.  
#' 
#' The returned dataframe always has three columns (x,y,z):
#' x: contains spatial information (easting/longitude)
#' y: contains spatial information (northing/latitude)
#' z: contains predictions, either as a factor or numeric variable
#' 
#' Since the output of the function maintains a consistent data type
#' and schema, it's much easier to use in pipelines.  It also allows
#' for consistent raterization via `raster::rasterFromXYZ()`.
#'
#' @return 
#' @param df: A data.frame
#' @param model the machine learning model for the 
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
apply_model <- function(df, model, threads = 1, clean_names = TRUE){

   return(UseMethod("apply_model", model))
    #print(prediction$prediction)
}


# bind together a model and the function to complete inference
# into a single S3 Object
#' An S3 object "class" for generalizing support for complex model types
#' 
#' This wrapper class contains the model itself, the method reqiured to
#' apply it, and the list of features required for its use.
#' It enables users to quickly enable the pipeline to run on composite
#' models and model types not supplorted by apply_model(...).
#' 
#' @param model The model to do the predictions.  This can be any R object
#' which will be passed to the 'apply_function' (below) for
#' inference.  For example, this can be a list of models to create
#' a ensemble model, a model that is not supported by the pipeline directly,
#' or any other data you wish.
#' @param apply_function the function that will be called by apply_model when
#' the pipeline runs.  The call signature of the  function should be as follows:
#' application function([this object]$model, df).  The first arguement is the
#' model slot of this object, and the second is the data to make predictions
#' as a data frame.
#' @param indices The vegetation indices used in by the model;
#' this defaults to loading it from the assets folder,
#' assets/vegindicesUsed.csv.
#' Users should plan to provide a character vector here, unless that list
#' of all vegetation indices at that location will work
#' @return the S3 object combining the above inputs
#' @export
#' @seealso apply_model.LSModel
LSModel <- function(
    model, 
    apply_function, 
    indices = read.csv("assets/vegIndicesUsed.csv", header = TRUE)$x
    ){
    output <- list(
        model = model,
        application = apply_function,
        indices = indices
    )
    class(output) <- "LSModel"
    return(output)
}

#' The apply_model implementation for the LSModel S3 object
#' 
#' 
#' 
#' @param df a data.frame of input data
#' @param model an LSModel instance
#' @return The output of the model$apple_function(model$model, data.frame) 
#' as a data.frame
#' @export
#' @seealso LSModel, apply_model
apply_model.LSModel <- function(df, model){
    return(
        as.data.frame(model$application(model$model, df))
    )
}

#' The apply_model implementation for ranger models
#' 
#' Implements apply_model for Ranger S4 objects
#' 
#' @param df The input data for the model
#' @param model the ranger model
#' @return a dataframe of predictions
#' @export
#' 
apply_model.ranger <- function(df, model){
    predictions <- tryCatch(
        {
            predict(
                model,
                data = df,
                type='response',
                num.threads = 2L
            )$prediction %>% as.data.frame() #(Ranger model)

    }, 
    error = function(cond){
        message("Error applying model - likely an empty file:")
        message(cond)
        # return df of NAs
        y <- data.frame()
        y$prediction <- df[, 5]
        y

    })

    return(predictions)
}


#' NOT YET IMPLEMENTED
#' 
#' Long
#' 
#' @param
#' @return
#' @export
#' 
apply_model.xgboost <- function(){

}

#' The apply_model implementation for caret models
#' 
#' This function implements the apply_model internals for 
#' the output of caret::train()
#' 
#' @param df a data.frame of input data
#' @param model the caret model
#' @return the data.frame of predictions.
#' @export
#' @seealso apply_model
apply_model.train <- function(df, model, ...) {
    predictions <- predict(
        model, 
        newdata = as.data.frame(df)) %>% 
        as.data.frame()
    colnames(predictions) <- c("z")
    return(predictions)
}

#' The implementation of apply_model for SVMs
#' 
#' 
#' 
#' @param df input data for creating predictions
#' @param model  the SVM model
#' @return a dataframe of predictions
#' @export
#' @seealso apply_model
apply_model.ksvm <- function(df, model, ...){
    require(kernlab, quietly = TRUE)
    predictions <- predict(model, as.data.frame(df))$response %>% as.data.frame()
    colnames(predictions) <- c("z")
    return(predictions)

}

#' The apply_model implementation for knn models
#' 
#' Long
#' 
#' @param df
#' @param model
#' @return a data.frame of predictions
#' @export
#' @seealso apply_model
apply_model.knn3 <- function(df, model, ...){
    require(caret, quietly = TRUE)
    predictions <- predict(model, df, type = "class") %>% as.data.frame()
    colnames(predictions) <- c("z")
    return(predictions)
}

#' The apply_model implementation for LightGBM models
#' 
#' Unfortunately, the LightGBM model metadata is not carried with
#' the model the way it is for R models.  This is due to the 
#' fact that the model is trained external to R in a separate 
#' process (Clang) and trained on a separate Dataset object from
#' lightGBM itself rather than a dataframe.  This necessitates the 
#' requirement that the model metadata is loaded externally.  
#' 
#' The default location for the metadata is assets/lightbgm_metadata.json.
#' It is possible to provide an additional argument specifying the location 
#' where the metadata JSON file is located.  The file must, however, be
#' a valid JSON text file, containing the following keys: 
#' - columns: an array of column names from the original training data
#' Since LightGBM does not track column names, this is used to ensure that
#' the column order of the input data is matched to the data provided here. 
#' In addition, it filters the data to the list of provided column names.
#' - level: an array of factor level names for the model output.  Since
#' lightgbm only tracks outputs as integers (starting at 0), the function
#' automatically returns the output to an R factor using the names provided 
#' in this key of the JSON file.
#' 
#' Overall, this is intended to make the LightGBM model behave the same way
#' as other R-native models.  It also outputs a single factor as a response
#' as other apply_model implementations do.  
#' 
#' @param df
#' @param model 
#' @return a data.frame of predictions
#' @export
#' @seealso apply_model
apply_model.lgb.Booster <- function(
    df, 
    model, 
    metadata = "assets/lightgbm_metadata.json",
    ...){
    # should load factor levels, column names from assets?
    require(lightgbm)
    print("Applying LightGBM Model")

    # get the class probabilities
    lgm_metadata <- rjson::fromJSON(file = metadata)
    prediction_matrix <- predict(
        model,
        as.matrix(df[,lgm_metadata$columns]),
        reshape = TRUE
    )

    # convert to factor and reset class names
    prediction_factor <- as.factor(
        ramify::argmax(prediction_matrix, rows = TRUE) - 1
        )
    levels(predictions) <- lgm_metadata$levels
    
    # convert to data.frame for downstream processing
    predictions <- as.data.frame(prediction_factor)
    colnames(predictions) <- c("z")

    print(summary(predictions))

    return(predictions)

}
