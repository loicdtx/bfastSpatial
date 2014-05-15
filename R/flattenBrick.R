#' @title Convert a RasterBrick or RasterStack to a RasterLayer based on the first encountered value
#' 
#' @description Flatten a brick by iteratively masking layer values by non-NA values in preceding layers. An optional threshold can be supplied, where preceding values exceeding the threshold are ignored in the masking procedure (and are therefore themselves removed).
#' 
#' @param x RasterBrick or RasterStack.
#' @param thresh Numeric. Optional: threshold to apply to preceding values in deciding whether to mask subsequent values.
#' @param ... Additional arguments to be bassed to \code{\link{mc.calc}}.
#' 
#' @return RasterLayer with values representing 'earliest' encountered values not exceeding \code{thresh}.
#' 
#' @details \code{thresh} only works on a 'greater-than' basis. To achieve the reverse, first let \code{x <- -1*x} then convert the result back by \code{y <- -1*y}.
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @export
#' 
#' @examples
#' # load Tura dataset
#' data(tura)
#' 
#' ## apply a very crude gap-fill using 2 Landsat
#' # the 1st layer has alot of gaps from the cloud mask
#' # suppose this is our target image and we want to use the 2nd layer to fill
#' # just take these two layers
#' gapfill <- tura[[c(1:2)]]
#' names(gapfill) <- c("target", "fill")
#' plot(gapfill)
#' 
#' # flatten the Brick to fill the gaps (NA's in the 1st layer)
#' filled <- flattenBrick(gapfill)
#' plot(filled)
#' 

flattenBrick <- function(x, thresh=NULL, ...){
    
    # pixelwise flatten function
    flat <- function(x){
        z <- x[!is.na(x)]
        
        # optional threshold
        if(!is.null(thresh))
            z <- z[z <= thresh]
        
        if(length(z) == 0){
            z <- NA
        } else {
            z <- z[1]
        }
        return(z)
    }
    
    y <- mc.calc(x, fun=flat, ...)
    
    return(y)
}
