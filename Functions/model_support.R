
get_var_names <- function(ml_model, ...) {
    return(UseMethod("get_var_names", ml_model))
}

get_var_names.default <- function(ml_model, ...){
    return(
        read.csv(
            "assets/vegIndicesUsed.csv",
            header = TRUE
        )$x %>% 
            as.character())
}

get_var_names.ranger <- function(ml_model, ...){
    Vars_names <- c(ml_model$forest$independent.variable.names)
    return( gsub("^X", "", Vars_names))
}

get_var_names.LSModel <- function(ml_model, ...){
    return(ml_model$indices)
}

get_required_veg_indices <- function(ml_model){
    return(UseMethod("get_required_veg_indices", ml_model))
}

get_required_veg_indices.LSModel <- function(ml_model, ...){
    return(ml_model$indices)
}

get_required_veg_indices.ranger <- function(ml_model, ...) {
    var_names <- get_var_names(ml_model)
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




#' a quick function based on the original code
#'
#' Long Description here
#'
#' @return 
#' @param df: A data.frame
#' @seealso None
#' @export 
#' @examples Not Yet Implmented
apply_model <- function(df, model, threads = 1, clean_names = TRUE){

   return(UseMethod("apply_model", model))
    #print(prediction$prediction)
}


# bind together a model and the function to complete inference
# into a single S3 Object
LSModel <- function(
    model, 
    apply_function, 
    indices = read.csv("assets/vegIndicesUsed.csv", header = TRUE)$x){
    output <- list(
        model = model,
        application = apply_function,
        indices = indices
    )
    class(output) <- "LSModel"
    return(output)
}

apply_model.LSModel <- function(df, model){
    return(
        as.data.frame(model$application(model$model, df))
    )
}

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



apply_model.xgboost <- function(){

}
