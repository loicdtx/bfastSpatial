#' bfmSpatial
#' 
#' Function to run bfastmonitor on any kind of raster brick, with parallel support
#' 
#' @description Implements bfastmonitor function, from the bfast package on any kind of rasterBrick object. Time information is provided as an extra object and the time series can be regular or irregular.
#' 
#' @param x rasterBrick or rasterStack object, or file name to a multilayer raster object stored on disk.
#' @param dates A date vector. The number of dates must match the number of layers of x.
#' @param pptype Character. Type of preprocessing to be applied to individual time series vectors. The two options are 'irregular' and '16-days'. See bfastts for more details.
#' @param start See \code{\link{bfastmonitor}}
#' @param formula See \code{\link{bfastmonitor}}
#' @param order See \code{\link{bfastmonitor}}
#' @param lag See \code{\link{bfastmonitor}}
#' @param slag See \code{\link{bfastmonitor}}
#' @param history See \code{\link{bfastmonitor}}
#' @param type See \code{\link{bfastmonitor}}
#' @param n See \code{\link{bfastmonitor}}
#' @param level See \code{\link{bfastmonitor}}
#' @param mc.cores Numeric. Number of cores to be used for the job.
#' @param ... Arguments to be passed to \code{\link{mc.calc}}
#' @return A rasterBrick, with 2 layers. One layer is the timing of change and the other layer the magnitude of change. See \code{\link{bfastmonitor}}
#' @author Loic Dutrieux
#' 
#' 


# Author: Loic Dutrieux
# January 2014

bfmSpatial <- function(x, dates, pptype='irregular', start,
                       formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL,
                       history = c("ROC", "BP", "all"),
                       type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, mc.cores=1, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }

    fun <- function(x) {
        ts <- bfastts(x, dates=dates, type=pptype)
        bfm <- try(bfastmonitor(data=ts, start=start,
                                formula=formula,
                                order=order, lag=lag, slag=slag,
                                history=history,
                                type=type, h=h,
                                end=end, level=level), silent=TRUE)
        if(class(bfm) == 'try-error') {
            return(cbind(NA, NA))
        } else {
            return(cbind(bfm$breakpoint, bfm$magnitude))
        }
    }
    
    out <- mc.calc(x=x, fun=fun, mc.cores=mc.cores, ...)
    return(out)
    
}