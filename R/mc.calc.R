
#' @title Multicore implementation of the raster::\code{\link{calc}} function.
#' 
#' @description Allows functions to be applied to raster objects, with multicore support.
#' 
#' @param x Raster* object
#' @param fun Function to be applied to the raster object.
#' @param ... Arguments to be passed to \code{link{writeRaster}}.
#' @details For further help, see \code{\link{calc}}. Warnings of the parallel package (see \code{\link{mclapply}} for instance) apply to this function.
#' @return a Raster* object 
#' @author Loic Dutrieux
#' @seealso \code{\link{calc}}
#' @import raster
#' @import parallel
#' @export
#' 

# Author: Loic Dutrieux
# June 2013
# loic.dutrieux@wur.nl

mc.calc <- function(x, fun, mc.cores=1, ...) {
    
    
    ## filename checks (if given)
    if(!hasArg(overwrite))
        overwrite <- FALSE
    
    if(hasArg(filename)) {
        
        # check if it exists
        if(file.exists(filename) & !overwrite) {
            stop(sprintf("%s exists, use overwrite=TRUE to overwrite.", filename))
            
        # check if it can be overwritten
        } else if(file.exists(filename) & overwrite) {
            if(file.access(filename, mode = 2) == -1) {
                stop(sprintf("%s cannot be overwritten. Check permissions or enter a different filename.", filename))
            }
        }
        
        
    }
    
    
    
    if(mc.cores == 1) { # Normal calc
        
        out <- calc(x=x, fun=fun, ...)
        return(out)
        
    } else { # mc.calc
        
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
        
        # Add ALL arguments passed in the ellipsis in the listOut object
        dots <- list(...)
        listOut <- c(listOut, dots)
        
        listOut$fun <- max
        out <- do.call(mosaic, listOut)
        
        return(out)
    }
    
}