#' Runs bfastmonitor for a spatial subset with aggregation
#' 
#' @description Runs \link{bfastmonitor} on a rasterBrick object for a set of locations, determined by an object of class \link{Spatial-class}.
#' 
#' @param x A rasterBrick or rasterStack, ideally with time written to the z dimension. In case time is not written to the z dimension, the \code{dates=} argument has to be supplied (see \link{zooExtract})
#' @param y A SpatialPoints, SpatialPointsDataFrame, SpatialPolygons, SpatialPolygonsDataFrame, SpatialLines, SpatialLinesDataFrame, or extent. \link{bfastmonitor} will be ran at these locations. In case each feature of the object covers several pixels (typically SpatialPolygons(DataFrames), SpatialLines(DataFrames) and extent), an aggregation function (\code{fun=}) has to be supplied (see \link{extract}).
#' @param mc.cores Numeric NUmber of cores to use (for parallel processing)
#' @param ... Arguments to be passed to \link{zooExtract}
#' 
#' @author Loic Dutrieux
#' 
#' @import raster
#' @import sp
#' 
#' @export

bfmSpOver <- function(x, y, mc.cores = 1, ...) {
    
    ts <- zooExtract(x, y, ...)
    bfm <- bfmZoo(ts, mc.cores = mc.cores)
    
    if(inherits(y, 'SpatialPoints')) {
        y <- SpatialPoints(y) # Not sure if that is necessary
        out <- SpatialPointsDataFrame(y, data = bfm)
    } else if(inherits(y, 'SpatialPolygons')) {
        y <- SpatialPolygons(y)
        out <- SpatialPolygonsDataFrame(y, data = bfm)
    } else if(inherits(y, 'SpatialLines')) {
        y <- SpatialLines(y)
        out <- SpatialLinesDataFrame(y, data = bfm)
    } else if(class(y) == 'extent') {
        y <- polygonFromExtent(y)
        out <- SpatialPolygonsDataFrame(y, data = bfm)
        proj4string(out) <- CRS(projection(x))
    }
    return(out)
}