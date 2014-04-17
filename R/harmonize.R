#' @title Harmonize Raster Extents
#' 
#' @description Crops a list of rasters to a common intersecting extent.
#' 
#' @param x A list of raster layers or a character vector of filenames to be directly read as rasters.
#' @param filename Character. Optional: character vector
#' @param ... Additional arguments to pass to \link{\code{crop}}
#' @details Landsat scenes are usually delivered with slightly different extents. It is recommended to crop these to an AOI, but in case the analysis is not constrained to a particular AOI, this function provides a convenient way to crop all rasters in a time series to a common intersecting extent.
#' @seealso \linke{\code{intersectExtents}}
#' @return List of Raster layers with a common extent.
#' @import raster
#' @export

harmonize <- function(x, filename=NULL, ...){
    # crop raster objects to their intersecting extent
    # required before creating a brick (if extents differ)
    
    # check that filenames correspond with input
    if(!is.null(filename) & length(filename) != length(x))
        stop("Number of filenames does not match number of input rasters.")
    
    # load rasters of x is a character vector
    if(class(x) == "character")
        x <- lapply(x, raster)
    
    # determine union extent
    e <- intersectExtents(x)
    
    # crop all inputs to this common extent
    if(is.null(filename)){
        y <- lapply(x, FUN=function(a) crop(a, e, ...))
    } else {
        y <- mapply(x, filename, FUN=function(a, fl) crop(a, e, filename=fl, ...)) 
    }
    
    return(y)
}
