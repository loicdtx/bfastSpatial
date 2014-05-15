
#' @title Multicore implementation of the raster::\code{\link{calc}} function.
#' 
#' @description Allows functions to be applied to raster objects, with multicore support.
#' 
#' @param x Raster* object
#' @param fun Function to be applied to the raster object.
#' @param ... Arguments to be passed to \code{link{writeRaster}}; only filename and overwrite are supported at the moment.
#' @details For further help, see \code{\link{calc}}. Warnings of the parallel package (see \code{\link{mclapply}} for instance) apply to this function.
#' @seealso \code{\link{calc}}
#' @import raster
#' @import parallel
#' @export
#' 

# Author: Loic Dutrieux
# June 2013
# loic.dutrieux@wur.nl

mc.calc <- function(x, fun, mc.cores=1, ...) {
    
    if(mc.cores == 1) { # Normal calc
        out <- calc(x=x, fun=fun, ...)
        return(out)
    } else {
        
        s <- blockSize(x, minblocks=mc.cores)
        blocs <- seq(1, s$n)
        
        
        # Create blocks and run the function on that bloc
        fun2 <- function(i) {
            e <- extent(x, r1=s$row[i], r2=s$row[i]+s$nrows[i]-1)
            # tmp <- rasterTmpFile()
            b <- crop(x, e)
            out <- calc(x=b, fun=fun) # Does this line need an elipsis
            return(out)
        }
        
        listOut <- mclapply(X=blocs, FUN=fun2, mc.cores=mc.cores)
        
        dots <- list(...)
        # Mosaic and write to filename
        if(hasArg(filename)){
            listOut$filename <- dots$filename
        }
        if(hasArg(overwrite)){
            listOut$overwrite <- dots$overwrite
        }
        listOut$fun <- max
        out <- do.call(mosaic, listOut)
        
        return(out)
    }
    
}