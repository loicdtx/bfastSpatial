
#' @title Function to run bfastmonitor on any kind of raster brick, with parallel support
#' 
#' @description Implements bfastmonitor function, from the bfast package on any kind of rasterBrick object. Time information is provided as an extra object and the time series can be regular or irregular.
#' 
#' @param x rasterBrick or rasterStack object, or file name to a multilayer raster object stored on disk.
#' @param dates A date vector. The number of dates must match the number of layers of x.
#' @param pptype Character. Type of preprocessing to be applied to individual time series vectors. The two options are 'irregular' and '16-days'. See \link{\code{bfastts}} for more details.
#' @param start See \code{\link{bfastmonitor}}
#' @param monend Numeric. Optional: end of the monitoring period in the format c(year, julian day). All raster data after this time will be removed before running \code{bfastmonitor}
#' @param formula See \code{\link{bfastmonitor}}
#' @param order See \code{\link{bfastmonitor}}
#' @param lag See \code{\link{bfastmonitor}}
#' @param slag See \code{\link{bfastmonitor}}
#' @param history See \code{\link{bfastmonitor}}
#' @param type See \code{\link{bfastmonitor}}
#' @param n See \code{\link{bfastmonitor}}
#' @param level See \code{\link{bfastmonitor}}
#' @param mc.cores Numeric. Number of cores to be used for the job.
#' @param sensor Character. Optional: Limit analysis to a particular sensor. Can be one or more of \code{c("ETM+", "ETM+ SLC-on", "ETM+ SLC-off", "TM", or "OLI")}
#' @param ... Arguments to be passed to \code{\link{mc.calc}}
#' @return A rasterBrick, with 3 layers. (1) Breakpoints (time of change); (2) change magnitude; and (3) error flag (1, NA). See \code{\link{bfastmonitor}}
#' 
#' @details
#' \code{bfmSpatial} applies \link{\code{bfastmonitor}} over a raster time series. For large raster datasets, processing times can be long. Given the number of parameters that can be set, it is recommended to first run \link{\code{bfmPixel}} over some test pixels or \code{bfmSpatial} over a small test area to gain familiarity with the time series being analyzed and to test several parameters.
#' 
#' Note that there is a difference between the \code{monend} argument included here and the \code{end} argument passed to \link{\code{bfastmonitor}}. Supplying a date in the format \code{c(year, Julian day)} to \code{monend} will result in the time series being trimmed \emph{before} running \link{\code{bfastmonitor}}. While this may seem identical to trimming the resulting \code{bfastmonitor} object per pixel, trimming the time serie before running \code{bfastmonitor} will have an impact on the change magnitude layer, which is calculated as the median residual withint the entire monitoring period, whether or not a breakpoint is detected.
#' 
#' While \code{bfmSpatial} can be applied over any raster time series with a time dimension (implicit or externally supplied), an additional feature relating to the type of Landsat sensor is also included here. This feature allows the user to specify data from a particular sensor, excluding all others. This can be useful if bias in a particular sensor is of concern, and can be tested without re-making the input RasterBrick. The \code{sensor} argument accepts any combination of the following characters (also see \link{\code{getSceneinfo}}):
#' \begin{itemize}
#' \item "all" - all layers
#' \item "TM" - Landsat 5 Thematic Mapper
#' \item "ETM+" - Landsat 7 Enhanced Thematic Mapper Plus (all)
#' \item "ETM+ SLC-on" - ETM+ data before failure of Scan Line Corrector
#' \item "ETM+ SLC-off" - ETM+ data after failure of the Scan Line Corrector
#' \end{itemize}
#' 
#' @author Loic Dutrieux and Ben DeVries
#' @import bfast
#' @import parallel
#' @import raster
#' @seealso \link{\code{bfastmonitor}}, \link{\code{bfmPixel}}
#' 
#' @examples
#' # load tura dataset
#' data(tura)
#' 
#' # run BFM over entire time series with a monitoring period starting at the beginning of 2009
#' t1 <- system.time(bfm <- bfmSpatial(tura, start=c(2009, 1)))
#' plot(t1)
#' 
#' # with multi-core support
#' t2 <- system.time(bfm <- bfmSpatial(tura, start=c(2009, 1), mc.cores=2))
#' t1 - t2
#' 
#' @export
#' 


# Author: Loic Dutrieux
# January 2014

bfmSpatial <- function(x, dates=NULL, pptype='irregular', start, monend=NULL,
                       formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL,
                       history = c("ROC", "BP", "all"),
                       type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, mc.cores=1, sensor=NULL, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }
    
    if(is.null(dates)) {
        if(is.null(getZ(x))) {
            if(!all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=names(x)))){ # Check if dates can be extracted from layernames
                stop('A date vector must be supplied, either via the date argument, the z dimention of x or comprised in names(x)')
            } else {
                dates <- as.Date(getSceneinfo(names(x))$date)
            }
        } else {
            dates <- getZ(x)
        }
    }
    
    # optional: reformat sensor if needed
    if("ETM+" %in% sensor)
        sensor <- c(sensor, "ETM+ SLC-on", "ETM+ SLC-off")
    
    # optional: get sceneinfo and change dates if sensor is supplied
    if(!is.null(sensor)){
        s <- getSceneinfo(names(x))
        s <- s[which(s$sensor %in% sensor), ]
        dates <- s$date
    }

    fun <- function(x) {
        # optional: subset x by sensor
        if(!is.null(sensor))
            x <- x[which(s$sensor %in% sensor)]
        
        # convert to bfast ts
        ts <- bfastts(x, dates=dates, type=pptype)
        
        #optional: apply window() if monend is supplied
        if(!is.null(monend))
            ts <- window(ts, end=monend)
        # run bfastmonitor()
        bfm <- try(bfastmonitor(data=ts, start=start,
                                formula=formula,
                                order=order, lag=lag, slag=slag,
                                history=history,
                                type=type, h=h,
                                end=end, level=level), silent=TRUE)
        if(class(bfm) == 'try-error') {
            res <- cbind(NA, NA, 1)
        } else {
            res <- cbind(bfm$breakpoint, bfm$magnitude, NA)
        }
        names(res) <- c("breakpoint", "magnitude", "error")
        return(res)
    }
    
    out <- mc.calc(x=x, fun=fun, mc.cores=mc.cores, ...)
    return(out)
}