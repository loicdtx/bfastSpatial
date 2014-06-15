#' @title Union Spatial Extent
#' 
#' @description Computes the union extent from a list of spatial objects.
#' 
#' @param x List. List of raster, sp, or extent objects.
#' 
#' @return Object of class \code{extent} representing the union extent of all objects in \code{x}.
#' 
#' @seealso \code{\link{harmonize}}
#' 
#' @import raster
#' @export
#' 

unionExtent <- function(x){
    
    # check if input is a list
    if(!is.list(x)){
        stop("x should be a list of RasterLayers, sp objects or Extent objects.\n")
    }
    
    # check object classes in list
    classes <- unique(unlist(sapply(x, class)))
    if(length(classes) > 1 | !classes %in% c("RasterLayer", "RasterBrick", "RasterStack", "Extent", "SpatialPolygons", "SpatialPolygonsDataFrame")){
        stop("x should be a list of RasterLayers, sp objects or Extent objects.\n")
    }
    
    # extract extents
    if(classes=="Extent"){
        e <- x
    } else {
        e <- lapply(x, extent)
    }
    
    # define union Extent
    unionxmin <- min(unlist(lapply(e, xmin)))
    unionxmax <- max(unlist(lapply(e, xmax)))
    unionymin <- min(unlist(lapply(e, ymin)))
    unionymax <- max(unlist(lapply(e, ymax)))
    unione <- extent(c(unionxmin, unionxmax, unionymin, unionymax))
    
    return(unione)
}