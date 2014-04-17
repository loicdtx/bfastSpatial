#' @title Intersecting Extent
#' 
#' @description Finds the intersecting extent from a list of spatial objects
#' 
#' @param x List. List of spatial objects from which to determine the intersecting extents
#' 
#' @return An object of class \code{extent} representing the intersecting extent of all objects in x.
#' @details This function works similarly to \link{\code{intersect}}, except that it can handle more than two spatial objects. In order to do this, the input should be supplied as a list (an can therefore contain a variety of spatial object types).
#' 
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' @import raster
#' @import sp
#' @export

intersectExtents <- function(x){
  # output an extent object representing the union of all extents
  # NOTE: intesect() in the raster package only does this for 2 objects
  # arguments:
    # x - a list of rasterLayers, sp objects, and/or Extent objects
  
  # check if input is a list
  if(!is.list(x))
    stop("x should be a list of RasterLayers, sp objects or Extent objects.\n")
  
  # check object classes in list
  classes <- unique(unlist(sapply(x, class)))
  if(length(classes) > 1 | !classes %in% c("RasterLayer", "RasterBrick", "RasterStack", "Extent", "SpatialPolygons", "SpatialPolygonsDataFrame")){
    stop("x should be a list of Raster* objects, sp objects or Extent objects.\n")
  }
  
  # extract extents
  e <- lapply(x, extent) # note that even if("extent" %in% e), this will still return an extent!
  
  # define intersecting Extent
  isectxmin <- max(unlist(lapply(e, xmin)))
  isectxmax <- min(unlist(lapply(e, xmax)))
  isectymin <- max(unlist(lapply(e, ymin)))
  isectymax <- min(unlist(lapply(e, ymax)))
  isecte <- extent(c(isectxmin, isectxmax, isectymin, isectymax))
  
  # check that the extent object makes sense
  if(xmin(isecte) > xmax(isecte) | ymin(isecte) > ymax(isecte)){
    isecte <- NULL
    warning("Not all extents overlap. No intersecting extent returned.")
  }
  
  return(isecte)
}
