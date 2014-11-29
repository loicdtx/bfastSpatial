#' Runs bfastmonitor for a spatial subset with aggregation
#' 
#' @description Runs \link{bfastmonitor} on a rasterBrick object for a set of locations, determined by an object of class \link{Spatial-class}.
#' 
#' @param x A rasterBrick or rasterStack, ideally with time written to the z dimension. In case time is not written to the z dimension, the \code{dates=} argument has to be supplied (see \link{zooExtract})
#' @param y A SpatialPoints, SpatialPointsDataFrame, SpatialPolygons, SpatialPolygonsDataFrame, SpatialLines, SpatialLinesDataFrame, or extent. \link{bfastmonitor} will be ran at these locations. In case each feature of the object covers several pixels (typically SpatialPolygons(DataFrames), SpatialLines(DataFrames) and extent), an aggregation function (\code{fun=}) has to be supplied (see \link{extract}).
#' @param start See \code{\link{bfastmonitor}}
#' @param formula See \code{\link{bfastmonitor}}
#' @param order See \code{\link{bfastmonitor}}
#' @param lag See \code{\link{bfastmonitor}}
#' @param slag See \code{\link{bfastmonitor}}
#' @param history See \code{\link{bfastmonitor}}
#' @param type See \code{\link{bfastmonitor}}
#' @param h See \code{\link{bfastmonitor}}
#' @param level See \code{\link{bfastmonitor}}
#' @param mc.cores Numeric NUmber of cores to use (for parallel processing)
#' @param ... Arguments to be passed to \link{zooExtract}
#' 
#' @author Loic Dutrieux
#' 
#' @examples
#' # Load data
#' data(tura)
#' 
#' # 1- SpatialPoints case
#' # Generate SpatialPoints
#' sp <- sampleRegular(x = tura, size = 20, sp=TRUE)
#' 
#' # Run bfmSpOver with monitoring period starting year 2005 and all other default parameters of bfastmonitor
#' out <- bfmSpOver(tura, y = sp, start=c(2005,1))
#' 
#' # Visualize the results
#' plot(tura, 166)
#' 
#' # Build color palette
#' colfunc <- colorRampPalette(c("yellow", "red"))
#' colList <- colfunc(2013 - 2005)
#' points(out, col= colList[out$breakpoint - 2005], pch=16, cex = abs(out$magnitude/max(out$magnitude)))
#' # Color corresponds to timing of break and size to magnitude
#' 
#' # 2 - SpatialPolygons case
#' data(turaSp)
#' # Run bfmSpOver with monitoring period starting year 2002 and mean spatial aggregation function
#' out2 <- bfmSpOver(tura, y = turaSp, fun = mean, start=c(2002,1))
#' 
#' # Visualize
#' plot(tura, 166)
#' # Build color palette
#' colfunc <- colorRampPalette(c("yellow", "red"))
#' colList <- colfunc(2013 - 2002)
#' plot(out2, col = colList[out2$breakpoint - 2002], add = TRUE)
#' # Interpretation: The redder the latter the break was detected. If transparent, no break detected in spatially aggregated polygon time-series.
#'
#' 
#' @import raster
#' @import sp
#' 
#' @export

bfmSpOver <- function(x, y, start, formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL, history = c("ROC", "BP", "all"), type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, mc.cores = 1, ...) {
    
    ts <- zooExtract(x, y, ...)
    bfm <- bfmZoo(ts, mc.cores = mc.cores, start = start,
                  formula=formula,
                  order=order, lag=lag, slag=slag,
                  history=history,
                  type=type, h=h,
                  end=end, level=level)
    
    if(inherits(y, 'SpatialPoints')) {
        y <- SpatialPoints(y) # Not sure if that is necessary
        out <- SpatialPointsDataFrame(y, data = bfm)
    } else if(inherits(y, 'SpatialPolygons')) {
        out <- SpatialPolygonsDataFrame(y, data = bfm, match.ID = FALSE)
    } else if(inherits(y, 'SpatialLines')) {
        out <- SpatialLinesDataFrame(y, data = bfm, match.ID = FALSE)
    } else if(class(y) == 'extent') {
        y <- polygonFromExtent(y)
        out <- SpatialPolygonsDataFrame(y, data = bfm, match.ID = FALSE)
        proj4string(out) <- CRS(projection(x))
    }
    return(out)
}