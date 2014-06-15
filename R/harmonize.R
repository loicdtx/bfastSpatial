#' @title Harmonize Raster Extents
#' 
#' @description Extends extents of rasters in a list to their union extent
#' 
#' @param x A list of raster layers or a character vector of filenames to be directly read as rasters.
#' @param filename Character. Optional: character vector
#' @param ... Additional arguments to pass to \code{\link{extend}}
#' @details Landsat scenes are usually delivered with slightly different extents. It is recommended to crop these to an area of interest (AOI), but in case the analysis is not constrained to a particular AOI, this function provides a convenient way to extend all raster extents in a time series to a union extent.
#' @seealso \code{\link{unionExtent}}
#' @return List of Raster layers with a common extent.
#' @import raster
#' @export
#' 

harmonize <- function(x, filename=NULL, ...){
    
    # check that filenames correspond with input
    if(!is.null(filename) & length(filename) != length(x))
        stop("Number of filenames does not match number of input rasters.")
    
    # load rasters if x is a character vector
    if(class(x) == "character")
        x <- lapply(x, raster)
    
    # determine union extent
    e <- unionExtent(x)
    
    # crop all inputs to this common extent
    if(is.null(filename)){
        y <- lapply(x, FUN=function(a) extend(a, e, ...))
    } else {
        y <- mapply(x, filename, FUN=function(a, fl) extend(a, e, filename=fl, ...)) 
    }
    
    return(y)
}
