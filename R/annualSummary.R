#' @title Annual summary of a time series RasterBrick
#' 
#' @description Calculates pixel-based statistics for every year represented by a time series RasterBrick
#' 
#' @param RasterBrick or RasterStack
#' @param fun Function to apply over each pixel for each year
#' @param sceneID Character. Optional: Landsat scene ID's for each layer of the input RasterBrick or RasterStack. If not given, sceneID's must be contained in the layer names
#' @param sensor Character. Optional: limit calculation to images from a particular sensor. Defaults to "all", but can take any of "TM", "ETM+", "ETM+ SLC-off" or "ETM+ SLC-on"
#' @param ... Arguments to be passed to \link{\code{mc.calc}}
#' 
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' 
#' @examples
#' # load tura RasterBrick
#' data(tura)
#' 
#' # calculate mean value per year for ETM+ data only
#' annualMean <- annualSummary(tura, fun=mean)
#' 

annualSummary <- function(x, fun, sceneID=NULL, sensor="all", na.rm=NULL, ...){
    ###### na.rm doesn't work now for sum() or mean(), etc...!

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
    years <- sort(unique(s$year))
    
    # function to be applied over each pixel in the RasterBrickStack
    pixStat <- function(b){
        ps <- numeric()
        for(i in 1:length(years)){
            tmp <- list(b[which(s$year == years[i])])
            if(!is.null(na.rm))
                tmp$na.rm <- na.rm
            ps[i] <- do.call(fun, tmp)
        }
        
        names(ps) <- years
        return(ps)
    }
    
    out <- mc.calc(x, fun=pixStat, ...)
    
    return(out)
    
}