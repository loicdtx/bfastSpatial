#' @title Trim raster time series
#' 
#' @description Trim a time series RasterBrickStack based on a specified Landsat sensor or threshold date (year)
#' 
#' @param x RasterBrick or RasterStack
#' @param sceneID Character. Landsat scene ID'. Length and order of vector should correspond with layers of x. Can be omitted if names(x) correspond to sceneID's. See \code{\link{getSceneinfo}} for more information on Landsat sceneID's.
#' @param sensor Character. Limit time series to specified sensor(s). Can take any combination of "ETM+ SLC-off", "ETM+ SLC-on", "ETM+", "TM", "OLI". Defaults to "all" (use all sensors).
#' @param minDate Numeric. Optional: minumum date (in format c(year, julian day)) before which all layers will be removed from the RasterBrickStack.
#' @param maxDate Numeric. Optional: maximum date (in format c(year, julian day)) after which all layers will be removed form the RasterBrickStack.
#' @param ... Additional arguments to be passed to \code{\link{raster::subset}}
#' 
#' @details This function only supports Landsat data with associated scene ID's at this time. Support for MODIS (and possibly other datasets) wil follow in future versions.
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @export
#' 
subsetRasterTS <- function(x, sceneID=NULL, sensor="all", minDate=NULL, maxDate=NULL, ...){
    
    if(is.character(x)) {
        x <- brick(x)
    }
    
    # get scene information either from sceneID or from layer names
    if(!is.null(sceneID)){
        s <- getSceneinfo(sceneID)
    } else {
        s <- getSceneinfo(names(x))
        ### TODO: insert .hasLandsatMetadata() here as an additional check
    }
    
    
    ## modify conditions:
    # sensor ID's
    if("all" %in% sensor)
        sensor <- c("TM", "ETM+ SLC-on", "ETM+ SLC-off", "OLI")
    if ("ETM+" %in% sensor) {
        sensor <- unique(c(sensor, "ETM+ SLC-on", "ETM+ SLC-off"))
    }
    
    # minDate
    if(!is.null(minDate)){
        minDate <- as.Date(paste(minDate, collapse="-"), format="%Y-%j")
    } else {
        minDate <- min(s$date)
    }
    
    # maxDate
    if(!is.null(maxDate)){
        maxDate <- as.Date(paste(maxDate, collapse="-"), format="%Y-%j")
    } else {
        maxDate <- max(s$date)
    }
    
    # select scenes that correspond to conditions
    sel <- which(s$sensor %in% sensor & s$date >= minDate & s$date <= maxDate)
    sel <- sort(unique(sel))
    if(all(c(1:nrow(s)) %in% sel))
        stop("All scenes satisfy given criteria, so no trim is necessary.")
    if(is.null(sel))
        stop("No scenes found which apply to trim criteria")
    sel <- sort(sel)
        
    # subset x based on sel
    y <- subset(x, subset=sel, ...)
    
    return(y)
}