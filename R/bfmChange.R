#' @title Extract breakpoints from bfm RasterBrick
#' 
#' @description Creates a raster layer with breakpoint timing values from the output bfm raster brick
#' 
#' @param bfm RasterBrick output from \code{\link{bfmSpatial}}. The 1st layer is the breakpoint timing and the 2nd is the change magnitude.
#' @param ... Additional arguments to pass to \code{\link{writeRaster}}.
#' 
#' @details The output of this function can also be used as a parameter in the \code{\link{bfmMagn}} function to filter out pixels where no breakpoint was detected.
#' 
#' @return RasterLayer with breakpoint timing per pixel. Breakpoint times are in the format yyyy.jjj (where y is year and j is JulianDay/365).
#' 
#' @author Ben DeVries
#' 
#' @seealso \code{\link{bfmSpatial}}, \code{\link{bfmMagn}}, \code{\link{changeMonth}}
#' 
#' @import raster
#' @export


bfmChange <- function(bfm, ...)
{
  
  change <- raster(bfm, 1)
  
  if(hasArg(filename)) 
    writeRaster(change, ...)
  
  return(change)
}
