#' Landsat-like zip archives
#' @name LE70230282011250EDC00_sub.zip-LE70230282015250EDC00_sub_fake.zip
#' @description subset of Landsat surface reflectance data for testing pre-processing steps.
#' @docType data
#' @author NASA/USGS
#' @keywords data

NULL


#' SpatialPolygons object
#' @aliases turaSp
#' @name turaSp
#' @description Three polygons located in the tura area (ethiopia)
#' @docType data
#' @usage data(turaSp)
#' @author Ben DeVries
#' @keywords data
#' @examples
#' data(turaSp)
#' data(tura)
#' plot(tura, 2)
#' plot(turaSp, add = TRUE)

NULL

#' RasterBrick object
#' @aliases tura
#' @name tura
#' @description Time series raster brick for an area in Tura kebele, Kafa Zone, SW Ethiopia. Values are NDVI rescaled by a factor of 10000. Data originate from the Landsat 5 TM and Landsat 7 ETM+ sensors. Original Landsat scene names can be found by typing \code{names(tura)}. Dates are also contained in the z-dimension (\code{getZ(tura)}; see also \code{\link{timeStack}}).
#' @docType data
#' @usage data(tura)
#' @author Ben DeVries
#' @source http://earthexplorer.usgs.gov
#' @keywords data
#' @references DeVries B., Verbesselt J., Kooistra L. and Herold M. (2015). Robust Monitoring of Small-Scale Forest Disturbances in an Afromontane Forest Using Landsat Time Series. Remote Sensing of Environment, 161:107-121.
#' @examples
#' data(tura)
#' 
#' # scene names
#' names(tura)
#' s <- getSceneinfo(names(tura))
#' s$year <- as.numeric(format(s$date, format = "%Y"))
#' hist(s$year, breaks = c(1980:2015))
#' 
#' # plot first 9 scenes
#' plot(tura, 1:9, main = getZ(tura))
#' 
#' # plot a time series of the 50th cell
#' plot(getZ(tura), tura[50], xlab="time", ylab="NDVI (x 10000)")
#'
#' # make a new rasterBrick from only ETM+ scenes
#' x <- subsetRasterTS(tura, sensor = "ETM+")
#'
#' # plot time series from the revised brick
#' plot(getZ(x), x[50], xlab="time", ylab="NDVI (x 10000)")

NULL
