#' @title Utility to extract a zoo object from a raster time-series
#' 
#' @description This function extracts a zoo object from a multilayer raster object with time written to the z dimension or a date vector supplied externally. The aim of this utility is to facilitate the use of the bfmApp for bfastmonitor parameters investigation. The app allows the user to supply a zoo (time-series) object and visualize the effect of parameters change on the algorithm output. We outline in the vignette of the bfastSpatial package that this is an interesting step for parameter selection prior to more heavy processing when starting to work in a study area.
#' 
#' @param x rasterBrick or rasterStack object, or file name to a multilayer raster object stored on disk.
#' @param sample The point(s) to extract. Default to 'click' where a point is interactively choosen by clicking. Or any of object accepted by \code{\link{extract}}. (points represented by a two-column matrix or data.frame, or \code{\link{SpatialPoints}*}; \code{\link{SpatialPolygons}*}; \code{\link[sp]{SpatialLines}}; \code{\link{Extent}}; or a numeric vector representing cell numbers). For 'click', a layer of \code{x} needs to be plotted first.
#' @param dates A date vector (optional, only if time is not yet contained in the z dimension of the raster object, or comprised in its layer names.)
#' @param file character rds filename where to write the output.
#' @param ... Arguments to be passed to \link{extract}.
#'
#' @author Loic Dutrieux
#' @return A zoo object that may contain multiple time-series
#' @seealso \link{sr2vi}
#' 
#' @import zoo
#' @import raster
#' 
#' @examples
#' 
#' # Single time-series selected interactively
#' data(tura)
#' plot(tura, 2)
#' zooExtract(x = tura, sample = 'click', file = file.path(tempdir(), 'zooClick.rds'))
#' 
#' # Then in order to explore the time series and the bfastmonitor parameters, run the following line
#' \dontrun{
#' runGitHub("bfmApp", "dutri001")
#' }
#' # And upload the file zooClick.rds
#' 
#' @export


zooExtract <- function(x, sample = 'click', dates = NULL, file = NULL, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }
    
    # date collection snippet 
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
    }
    
    if(is.character(sample)) {
        if(sample == 'click') {
            v <- click(x, show=FALSE)        
        }
    } else {
        v <- extract(x, sample, ...)
    }
    
    out <- zoo(t(v), dates)
    
    if(!is.null(file)) {
        saveRDS(out, file = file)
    }
    
}