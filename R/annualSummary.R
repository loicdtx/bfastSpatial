#' @title Annual summary of a time series RasterBrick
#' 
#' @description Calculates pixel-based statistics for every year represented by a time series RasterBrick
#' 
#' @param RasterBrick or RasterStack
#' @param fun Function to apply over each pixel for each year
#' @param sceneID Character. Optional: Landsat scene ID's for each layer of the input RasterBrick or RasterStack. If not given, sceneID's must be contained in the layer names
#' @param years Numeric. Vector of years to which to limit the summary.
#' @param sensor Character. Optional: limit calculation to images from a particular sensor. Defaults to "all", but can take any of "TM", "ETM+", "ETM+ SLC-off" or "ETM+ SLC-on"
#' @param ... Arguments to be passed to \link{\code{mc.calc}}
#' @return RasterBrick with results of fun for each year represtented in the input time series RasterBrick.
#' 
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' 
#' @import raster
#' @export
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

annualSummary <- function(x, fun, sceneID=NULL, years=NULL, sensor="all", na.rm=NULL, ...){

    # get scene information from layer names
    if(is.null(sceneID)){
        s <- getSceneinfo(names(x))
    } else {
        s <- getSceneinfo(sceneID)
        names(x) <- row.names(s)
    }
    
    # include data only from desired sensor(s) and update s accordingly
    if (sensor != "all") {
        if ("ETM+" %in% sensor) {
            sensor <- unique(c(sensor, "ETM+ SLC-on", "ETM+ SLC-off"))
        }
        x <- dropLayer(x, which(!s$sensor %in% sensor))
        s <- s[which(s$sensor %in% sensor), ]
        names(x) <- row.names(s)
    }
    
    # add year column to s
    s$year <- as.numeric(substr(s$date, 1, 4))
    
    # vector of years over which to process
    yrs <- sort(unique(s$year))
    
    # limit to user-defined period
    if(!is.null(years))
        yrs <- yrs[yrs %in% years]
    
    # function to be applied over each pixel in the RasterBrickStack
    pixStat <- function(b){
        ps <- numeric()
        for(i in 1:length(yrs)){
            tmp <- list(b[which(s$year == yrs[i])])
            if(!is.null(na.rm))
                tmp$na.rm <- na.rm
            ps[i] <- do.call(fun, tmp)
        }
        
        names(ps) <- yrs
        return(ps)
    }
    
    out <- mc.calc(x, fun=pixStat, ...)
    
    return(out)
    
}