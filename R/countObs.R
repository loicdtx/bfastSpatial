#' @title Valid observations per pixel
#' 
#' @description Calculates the number of valid observations per pixel of a time series RasterBrick.
#' 
#' @param x RasterBrick or RasterStack
#' @param navalues Integer. Values representing NA's in time series. Defaults to NA, but multiple values can be supplied (e.g. \code{navalue = c(0, NA)})
#' @param as.perc Logical. Express result as a percentage of nlayers(x)?
#' @param ... Arguments to be passed to \link{\code{mc.calc}}
#' 
#' @import raster
#' @import parallel
#' @author Ben DeVries (\email{devries.br@@gmail.com})
#' @export
#' 

countObs <- function(x, navalues=c(NA), sensor = "all", as.perc=FALSE, ...){
    
    # include data only from desired sensor(s)
    if (sensor != "all") {
        # get scene information from layer names
        s <- getSceneinfo(names(x))
        
        if ("ETM+" %in% sensor) {
            sensor <- unique(c(sensor, "ETM+ SLC-on", "ETM+ SLC-off"))
        }
        x <- dropLayer(x, which(!s$sensor %in% sensor))
        s <- s[which(s$sensor %in% sensor), ]
        names(x) <- row.names(s)
    }
    
    # function to calculate # of observations per pixel
    fun <- function(b){
        n <- length(b[!b %in% navalues])
        if(as.perc)
            n <- n / nlayers(x) * 100
        
        return(n)
    }
    
    # apply fun within mc.calc()
    out <- mc.calc(x, fun=fun, ...)
    
    return(out)   
}
