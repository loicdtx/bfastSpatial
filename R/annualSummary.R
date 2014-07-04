#' @title Annual summary of a time series RasterBrick
#' 
#' @description Calculates pixel-based statistics for every year represented by a time series RasterBrick
#' 
#' @param x RasterBrick or RasterStack
#' @param fun Function to apply over each pixel for each year
#' @param dates Date. Optional: vector of dates exactly corresponding to the layers of x. If not included, dates must be included in the z dimension of x (see \code{\link{getZ}}) or in \code{names(x)}
#' @param years Numeric. Optional: Vector of years to which to limit the summary.
#' @param sensor Character. Optional: limit calculation to images from a particular sensor. Defaults to "all", but can take any of "TM", "ETM+", "ETM+ SLC-off" or "ETM+ SLC-on". Will be ignored with a warning if \code{names(x)} do not correspond to Landsat scene ID's.
#' @param ... Arguments to be passed to \code{\link{mc.calc}}
#' 
#' @return RasterBrick with results of \code{fun} for each year represented in the input time series RasterBrick.
#' 
#' @details
#' If \code{fun} takes a \code{na.rm} argument and none is supplied, the default value for \code{na.rm} for that function will be used.
#' 
#' @author Ben DeVries
#' 
#' @import raster
#' @export
#' 
#' @seealso \code{\link{summaryBrick}}
#' 
#' @examples
#' # load tura RasterBrick
#' data(tura)
#' 
#' # calculate mean and standard deviation values per year for ETM+ data only
#' annualMean <- annualSummary(tura, fun=mean, na.rm=TRUE, sensor="ETM+")
#' plot(annualMean)
#' annualSD <- annualSummary(tura, fun=sd, na.rm=TRUE, sensor="ETM+")
#' plot(annualSD)
#' 
#' # custom function to calculate # of non-NA values per pixel per year (similar to countObs())
#' ff <- function(x)
#'  length(x[!is.na(x)])
#' annualObs <- annualSummary(tura, fun=ff, sensor="ETM+")

annualSummary <- function(x, fun, dates=NULL, years=NULL, sensor=NULL, na.rm=NULL, ...){

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
        
    # get dates (if is.null(dates))
    if(is.null(dates)) {
        if(is.null(getZ(x))) {
            if(!.isLandsatSceneID(x)){ # Check if dates can be extracted from layernames
                stop('A date vector must be supplied, either via the date argument, the z dimension of x or comprised in names(x)')
            } else {
                dates <- as.Date(getSceneinfo(names(x))$date)
            }
        } else {
            dates <- getZ(x)
        }
    } else {
        if(length(dates) != nlayers(x)){
            stop("dates should be of same length as nlayers(x)")
        }
    }
    
    # trim dates if a sensor had been supplied
    if(!is.null(scenes)){
        dates <- dates[scenes]
    }
    
    # extract years
    y <- substr(dates, 1, 4)
    
    # vector of years over which to process
    yrs <- sort(unique(y))
    
    # limit to user-defined period
    if(!is.null(years))
        yrs <- yrs[yrs %in% years]
    
    # function to be applied over each pixel in the RasterBrickStack
    pixStat <- function(b){
        if(!is.null(scenes))
            b <- b[scenes]
        ps <- vector("numeric", length(yrs))
        for(i in 1:length(yrs)){
            args <- list(b[which(y == yrs[i])])
            if(is.logical(na.rm))
                args$na.rm <- na.rm
            ps[i] <- do.call(fun, args)
        }
        
        names(ps) <- yrs
        return(ps)
    }
    
    out <- mc.calc(x, fun=pixStat, ...)
    
    return(out)
}