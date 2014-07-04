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

countObs <- function(x, navalues=c(NA), sensor = NULL, as.perc=FALSE, ...){
    
    # if sensor is given (!is.null(sensor)), then limit the analysis to a particular sensor
    if(!is.null(sensor)){
        if ("ETM+" %in% sensor) {
            sensor <- unique(c(sensor, "ETM+ SLC-on", "ETM+ SLC-off"))
        }
        if(!.isLandsatSceneID(x)){
            warning("Scene IDs should be supplied as names(x) to subset by sensor. Ignoring...\n")
            scenes <- NULL
        } else {
            # 'allowed' scenes
            scenes <- which(getSceneinfo(names(x))$sensor %in% sensor)
        }
    } else {
        scenes <- NULL
    }
    
    # function to calculate # of observations per pixel
    fun <- function(b){
        if(!is.null(scenes)){
            b <- b[scenes]
        }
        n <- length(b[!b %in% navalues])
        if(as.perc)
            n <- n / nlayers(x) * 100
        
        return(n)
    }
    
    # apply fun within mc.calc()
    out <- mc.calc(x, fun=fun, ...)
    
    return(out)   
}
