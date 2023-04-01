create_robust_transform <- function(raster){

    df <- raster::rasterToPoints(raster)

    centers <- list()
    scales <- list()
    for(i in seq.int(ncol(df))){
        if(is.numeric(df[,i])){
            append(
                centers,
                median(df[,i], na.rm = TRUE)
            )

            append(
                scales,
                IQR(df[,i], na.rm = TRUE)
            )

        } else {
            centers[i] <- NULL
            append(
                centers,
                NULL
            )

            append(
                scales, 
                NULL
            )
        }
    }

    return(list(
        center = centers,
        scale = scales
    ))
}


create_standard_transform <- function(raster){
    df <- raster::rasterToPoints(raster)

    centers <- list()
    scales <- list()
    for(i in seq.int(ncol(df))){
        if(is.numeric(df[,i])){
            append(
                centers,
                mean(df[,i], na.rm = TRUE)
            )

            append(
                scales,
                sd(df[,i], na.rm = TRUE)
            )

        } else {
            centers[i] <- NULL
            append(
                centers,
                NULL
            )

            append(
                scales, 
                NULL
            )
        }
    }

    return(list(
        center = centers,
        scale = scales
    ))

}

create_minmax_transform <- function(raster){
    
    df <- raster::rasterToPoints(raster)

    centers <- list()
    scales <- list()
    for(i in seq.int(ncol(df))){
        if(is.numeric(df[,i])){
            append(
                centers,
                min(df[,i], na.rm = TRUE)
            )

            append(
                scales,
                (max(df[,i], na.rm = TRUE) - min(df[,i], na.rm = TRUE))
            )

        } else {
            centers[i] <- NULL
            append(
                centers,
                NULL
            )

            append(
                scales, 
                NULL
            )
        }
    }

    return(list(
        center = centers,
        scale = scales
    ))

}

apply_transform <- function(df, ){

}

create_site_transform <- function(raster_path){
    
}