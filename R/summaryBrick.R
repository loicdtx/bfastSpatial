#' @title Summarize a RasterBrick
#' 
#' @description Computes pixel-based summary statistics for a multi-layered raster object
#' 
#' @param x RasterBrick or RasterStack to be summarized
#' @param fun Function to apply to vectors extracted from each pixel
#' @param dates Date. Optional: vector of dates corresponding exactly to layers of \code{x}
#' @param sceneID Character. Optional: vector of Landsat scene ID's corresponding exactly to layers of \code{x}
#' @param minDate Date, Character or Numeric. Optional: minimum date to include in the calculation (see \link{\code{subsetRasterTS}}). Should either be supplied as a \code{date} or \code{numeric} of length 2 (see Details)
#' @param maxDate Date, Character or Numeric. Optional: maximum date to include in the calculation (see \link{\code{subsetRasterTS}}). Should either be supplied as a \code{date} or \code{numeric} of length 2 (see Details)
#' @param sensor Character. Optional: limit calculation to selected (Landsat) sensors. Defaults to "all" for all data.
#' @param ... Additional arguments to be passed to \link{\code{mc.calc}}
#' 
#' @return A Raster layer representing the summary statistic of each pixel in the input RasterBrick or RasterStack
#' 
#' @details
#' If \code{fun} takes a \code{na.rm} argument and none is supplied, it will be ignored and the default value for \code{na.rm} for that function will be used.
#' 
#' If \code{fun} returns a vector of length greater than one, a RasterBrick object will be returned (see \code{fun=range} example in examples)
#' 
#' \code{minDate} and \code{maxDate} are optional arguments to limit the calculation to a specific date range. These arguments can be supplied as Date or Character objects in the form "%Y-%m-%d" or as a Numeric of length 2. In the latter case, the first element is the year, and the second is the Julian day (from 1 to 365).
#' 
#' @seealso \link{\code{annualSummary}}

summaryBrick <- function(x, fun, dates=NULL, sceneID=NULL, na.rm=NULL, minDate=NULL, maxDate=NULL, sensor="all", ...){
    
    # if min and max Date are supplied, then establish and allowed date range
    if(!is.null(minDate) | !is.null(maxDate)){
        # if numeric, convert to dates
        if(length(minDate) == 2){
            minDate <- as.Date(paste(minDate, collapse="-"), format="%Y-%j")
        }
        if(length(maxDate) == 2){
            minDate <- as.Date(paste(maxDate, collapse="-"), format="%Y-%j")
        }
        
        # get dates
        if(is.null(dates) & is.null(sceneID)){
            dates <- getSceneinfo(names(x))$date
        } else if(is.null(dates) & !is.null(sceneID)){
            dates <- getSceneinfo(sceneID)$date
        }
        
        # make a new vector of allowable dates
        if(is.null(minDate))
            minDate <- min(dates)
        if(is.null(maxDate))
            maxDate <- max(dates)
        dateRange <- dates[dates >= minDate & dates <= maxDate]
    } else {
        dateRange <- NULL
    }
    
    # if sensor != "all", then limit the analysis to a particular sensor
    if(sensor != "all"){
        if ("ETM+" %in% sensor) {
            sensor <- unique(c(sensor, "ETM+ SLC-on", "ETM+ SLC-off"))
        }
        if(is.null(sceneID)){
            s <- getSceneinfo(names(x))
        } else {
            s <- getSceneinfo(sceneID)
        }
        # 'allowed' scenes
        scenes <- which(s$sensor %in% sensor)
    } else {
        scenes <- NULL
    }
    
    # function to be applied over each pixel in the RasterBrickStack
    pixStat <- function(b){
        if(!is.null(dateRange))
            b <- b[which(dates %in% dateRange)]
        if(!is.null(scenes))
            b <- b[scenes]
        
        args <- list(b)

        # add an na.rm argument if supplied by user
        if(is.logical(na.rm))
            args$na.rm <- na.rm
        ps <- do.call(fun, args)
        
        return(ps)
    }
    
    out <- mc.calc(x, fun=pixStat, ...)
    
    return(out)
}

