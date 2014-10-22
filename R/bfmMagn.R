#' @title Extract change magnitude bfm RasterBrick
#' 
#' @description Creates a raster layer representing the change magnitude (median of the residuals during the monitoring period) from a resulting bfm raster brick. Magnitude is calculated for all pixels regardless of whether a breakpoint was detected or not. This function allows for filtering based on a supplied change (breakpoint) raster.
#' 
#' @param bfm RasterBrick output from \code{\link{bfmSpatial}}. 1st layer is the breakpoint timing and the 2nd is the change magnitude.
#' @param change Optional: RasterLayer output from \code{\link{bfmChange}}. Used to mask out all non-breakpoints from magnitude RasterLayer.
#' @param thresh Numeric. Optional: threshold to apply to magnitude thresholds. Any magnitude values above this threshold will be excluded.
#' @param ... Arguments to be passed to \code{\link{writeRaster}}.
#' 
#' @return RasterLayer representing the change magnitude for all pixels, unless change filter is applied, in which case only representing breakpoint pixels.
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' @seealso \code{\link{bfmChange}}
#' @examples
#' # load in raster brick
#' data(tura)
#'
#' # run bfm on brick for monitoring period 2009 and using all available data as the stable history period.
#' bfm <- bfmSpatial(tura, start=c(2009,1))
#' 
#' # extract and plot change magnitude (continuous values)
#' magn <- bfmMagn(bfm)
#' plot(magn)
#'
#' # extract magnitude only for pixels where breaktpoint were detected
#' change <- bfmChange(bfm)
#' magn.filt <- bfmMagn(bfm, change = change)
#' plot(magn.filt)
#' 
#' # extract breakpoint magnitudes for large changes (where dNDVI < -0.03)
#' magn.large <- bfmMagn(bfm, change = change, thresh=-0.03)
#' plot(magn.large)
#'
# compare the results
#' if("rasterVis" %in% row.names(installed.packages())){
#'     library(rasterVis)
#'     levelplot(brick(magn, magn.filt, magn.large))
#' }
#' @import raster
#' @export

bfmMagn <- function(bfm, change=NULL, thresh=NULL, ...)
{
  # extract magn raster layer
  magn <- raster(bfm, 2)
  
  # include only identified breakpoint pixels
  if(!is.null(change))
    magn[is.na(change)] <- NA
  
  # apply a threshold (only take values below this threshold)
  if (!is.null(thresh))
    magn[magn>thresh] <- NA
  
  # write to file
  if(hasArg(filename))
    writeRaster(magn, ...)
  
  return(magn)
}
