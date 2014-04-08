#' @title Convert a RasterBrick or RasterStack to a RasterLayer based on the first encountered value
#' 
#' @description Flatten a brick by iteratively masking layer values by non-NA values in preceding layers. An optional threshold can be supplied, where preceding values exceeding the threshold are ignored in the masking procedure (and are therefore themselves removed).
#' 
#' @param x RasterBrick or RasterStack.
#' @param thresh Numeric. Optional: threshold to apply to preceding values in deciding whether to mask subsequent values.
#' @param ... Additional arguments to be bassed to \code{\link{writeRaster}}.
#' @return RasterLayer with values representing 'earliest' encountered values not exceeding \code{thresh}.
#' @details \code{thresh} only works on a 'greater-than' basis. To achieve the reverse, first let \code{x <- -1*x} then convert the result back by \code{y <- -1*y}.
#' @author Ben DeVries \email{devries.br@gmail.com}
#' @import raster
#' @export

flattenBrick <- function(x, thresh=NULL, filename="", ...){

  # extract first layer
  y <- x[[1]]
  
  # apply threshold
  if(!is.null(thresh))
    y[y < thresh] <- NA
  
  # cycle through remaining layers and assign values to y if not already assigned
  for(i in 2:nlayers(x)){
    z <- x[[i]]
    if(!is.null(thresh))
      z[z < thresh] <- NA
    y[is.na(y)] <- z[is.na(y)]
  }
  
  # write to file
  if(hasArg(filename)){
    writeRaster(y, ...)
  }
  
  return(y)
}
