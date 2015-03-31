#' Runs breakpoints for a spatial subset with aggregation
#' 
#' @description Runs \link{breakpoints} on a rasterBrick object for a set of locations, determined by an object of class \link{Spatial-class}.
#' 
#' @param x A rasterBrick or rasterStack, ideally with time written to the z dimension. In case time is not written to the z dimension, the \code{dates=} argument has to be supplied (see \link{zooExtract})
#' @param y A SpatialPoints, SpatialPointsDataFrame, SpatialPolygons, SpatialPolygonsDataFrame, SpatialLines, SpatialLinesDataFrame, or extent. \link{breakpoints} will be ran at these locations. In case each feature of the object covers several pixels (typically SpatialPolygons(DataFrames), SpatialLines(DataFrames) and extent), an aggregation function (\code{fun=}) has to be supplied (see \link{extract}).
#' @param formula See \code{\link{breakpoints}}
#' @param order See \code{\link{bfastpp}}
#' @param breaks See \code{\link{breakpoints}}
#' @param h See \code{\link{breakpoints}}
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
#' # Run for each pixel overlapped by a point
#' out <- bpPhenoSp(tura, sp, formula = response ~ trend, h = 0.1)
#' 
#' # Visualize the results
#' plot(tura, 166)
#' plot(out, pch=16, cex = out$nbreaks, add = TRUE)
#' 
#' # 2 - SpatialPolygons case
#' data(turaSp)
#' 
#' out2 <- bpPhenoSp(tura, turaSp, fun = mean, formula = response ~ trend, h = 0.1)
#' # Visualize
#' plot(tura, 166)
#' # Build color palette
#' colfunc <- colorRampPalette(c("yellow", "red"))
#' colList <- colfunc(2008 - 1987)
#' plot(out2, col = colList[floor(out2$break1) - 1986], add = TRUE)
#' # Interpretation: The redder the latter the first break was detected. If transparent, no break detected in spatially aggregated polygon time-series.
#'
#' @import raster
#' @import sp
#' @import strucchange
#' @import bfast
#' @import zoo
#' 
#' @export

bpPhenoSp <- function(x, y, formula = response ~ trend + harmon, order = 1, h = 0.1, breaks = NULL, mc.cores = 1, ...) {
    
    ts <- zooExtract(x, y, ...)
    
    # Get max segments
    nl <- nlayers(x)
    if (!is.null(breaks)) {
        segMax <- breaks + 1
    } else {
        segMax <- ceiling(nl * h)
    }
    
    
    # Run function (must apply to a zoo object with multiple ts and return a dataframe)
    bp <- bpPheno(x = ts, order = order, formula = formula, breaks = breaks, h = h, nbreaks = segMax, mc.cores = mc.cores)
    
    
    
    if(inherits(y, 'SpatialPoints')) {
        y <- SpatialPoints(y) # Not sure if that is necessary
        out <- SpatialPointsDataFrame(y, data = bp)
    } else if(inherits(y, 'SpatialPolygons')) {
        out <- SpatialPolygonsDataFrame(y, data = bp, match.ID = FALSE)
    } else if(inherits(y, 'SpatialLines')) {
        out <- SpatialLinesDataFrame(y, data = bp, match.ID = FALSE)
    } else if(class(y) == 'extent') {
        y <- as(y, 'SpatialPolygons')
        out <- SpatialPolygonsDataFrame(y, data = bp, match.ID = FALSE)
        proj4string(out) <- CRS(projection(x))
    }
    return(out)
}