#' @title Annual temporal composite from a time series RasterBrick
#' 
#' @description Generates annual temporal composites based on a time series RasterBrick with the option to restrict to a given period of the year
#' 
#' @param x RasterBrick or RasterStack
#' @param fun Function to apply over each pixel for each defined period
#' @param period Numeric. Optional: vector of julian days to limit the calculation. Can be used to restrict compositing to a specific season, for example.
#' @param dates Date. Optional: vector of dates exactly corresponding to the layers of x. If not included, dates must be included in the z dimension of x (see \code{\link{getZ}}) or in \code{names(x)}.
#' @param years Numeric. Optional: Vector of years to which to limit the composite.
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
#' # calculate mean NDVI and standard deviation per year for ETM+ data only
#' annualMean <- annualComposite(tura, fun=mean, na.rm=TRUE, sensor="ETM+")
#' plot(annualMean, zlim = c(4000, 10000))
#' annualSD <- annualComposite(tura, fun=sd, na.rm=TRUE, sensor="ETM+")
#' plot(annualSD, zlim = c(0, 4000))
#' 
#' # custom function to calculate # of non-NA values per pixel per year (similar to countObs())
#' ff <- function(x)
#'  sum(!is.na(x))
#' annualObs <- annualComposite(tura, fun = ff, sensor = "ETM+")
#' plot(annualObs, zlim = c(0, 15))
#' 
#' ## median NDVI for dry and rainy seasons (approx. October to April)
#' 
#' # get start and end of rainy season as julian days
#' rainStart <- as.numeric(format(as.Date('2000-05-01'), format = '%j'))
#' rainEnd <- as.numeric(format(as.Date('2000-09-30'), format = '%j'))
#' 
#' # compute median NDVI for dry season periods only
#' medNDVIdry <- annualComposite(tura, fun = median, na.rm = TRUE, period = c(1:rainStart-1, rainEnd+1:365), sensor = "ETM+")
#' plot(medNDVIdry, zlim = c(2500, 10000))
#' 
#' # compute median NDVI for rainy season periods only
#' medNDVIrain <- annualComposite(tura, fun = median, na.rm = TRUE, period = c(rainStart:rainEnd), sensor = "ETM+")
#' plot(medNDVIrain, zlim = c(2500, 10000))
#'
#' # observation counts per season
#' nobsDry <- annualComposite(tura, fun = ff, period = c(1:rainStart-1, rainEnd+1:365), sensor = "ETM+")
#' plot(nobsDry, zlim = c(0, 16))
#' nobsRain <- annualComposite(tura, fun = ff, period = c(rainStart:rainEnd), sensor = "ETM+")
#' plot(nobsRain, zlim = c(0, 16))
#' plot(nobsDry - nobsRain, zlim = c(-10, 10))
#' 

annualComposite <- function(x, fun, period = NULL, dates=NULL, years=NULL, sensor=NULL, na.rm=NULL, ...){

    
    
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
    y <- as.numeric(substr(dates, 1, 4))
    
    # vector of julian days (DOY)
    jd <- as.numeric(format(dates, format = "%j"))
    
    # vector of years over which to process
    yrs <- sort(unique(y))
    
    # limit to user-defined period
    if(!is.null(years))
        yrs <- yrs[yrs %in% years]
    
    
    
    # function to be applied over each pixel in the RasterBrickStack
    pixStat <- function(b){
        
        # subset vector b by sensor
        if(!is.null(scenes))
            b <- b[scenes]
        
        # subset vectors b and y by period
        if(!is.null(period)){
            b <- b[which(jd %in% period)]
            y <- y[which(jd %in% period)]
        }
        
        # vector of length length(yrs) to contain result
        ps <- vector("numeric", length(yrs))
        
        # run fun() for each year in yrs
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