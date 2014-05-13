#' @title Valid observations per pixel
#' 
#' @description Calculates the number of valid observations per pixel of a time series RasterBrick.
#' 
#' @param x RasterBrick or RasterStack
#' @param navalues Integer. Values representing NA's in time series. Defaults to NA, but multiple values can be supplied (e.g. \code{navalue = c(0, NA)})
#' @param as.perc Logical. Express result as a percentage of nlayers(x)?
#' @param ... Arguments to be passed to \code{\link{mc.calc}}
#' 
#' @return A raster layer with the number of valid observations per pixel, either as an absolute value or expressed as a percent of \code{nlayers(x)} if \code{is.perc = TRUE}.
#' 
#' @seealso \code{\link{summaryBrick}}
#' 
#' @import raster
#' @import parallel
#' 
#' @author Ben DeVries
#' @export
#' 

countObs <- function(x, navalues=c(NA), sensor = "all", as.perc=FALSE, ...){
    
    # include data only from desired sensor(s), only if Landsat scene ID's are provided
    if(sensor != "all" & !all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=names(x)))){
        warning("Scene IDs should be supplied as names(x) or as sceneID to subset by sensor. Ignoring...\n")
        sensor <- "all"
    }
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
