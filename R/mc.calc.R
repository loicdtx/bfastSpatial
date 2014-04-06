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