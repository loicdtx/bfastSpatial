#' @title Apply bfmastmonitor on a single pixel
#' 
#' @description Apply bfastmonitor (bfm) on a single pixel of known cell index (by supplying a single numeric value for cell), xy coordinates (\code{cell=c(x, y)}), or interactively by clicking on a plot (by setting \code{interactive=TRUE}). Outputs a list with (1) an object of class 'bfastmonitor' and (2) the resulting cell number (useful for follow-up analysis).
#' 
#' @param x RasterBrick with raster time series data.
#' @param start Numeric. Vector of length = 2 representing the start of the monitoring period (in the format c(year, julian day))
#' @param monend Numeric. Optional: the end of the monitoring period (in the format c(year, julian day)), at which point the time series will be trimmed.
#' @param cell Numeric. Can be one of: (1) a numeric of length 1 indicating the raster cell to be observed; (2) a numeric of length 2 representing the (x,y) coordinate of the raster cell to be observed. Can also be omitted, in which case 'interactive' must be set to TRUE (see below)
#' @param min.thresh Numeric. Optional: A minimum threshold below which NA's are assigned to data points.
#' @param sensor Character. Optional: Limit analysis to data from one or more sensors. Defaults to 'all' to use all available data in the time series.
#' @param interactive Logical. Select cell by clicking on an already plotted map? Defaults to \code{FALSE}. If \code{FALSE}, a value must be assigned to \code{cell} (see above).
#' @param plot Logical. Plot the result? Defaults to \code{FALSE}.
#' @param ... Arguments to be passed to \code{\link{bfastmonitor}}
#' 
#' @return A list with the following components: 1) $bfm - an object of class 'bfastmonitor' (see \code{\link{bfastmonitor()}}) 2) $cell - the cell index (an integer of length 1). This can be used to run \code{bfmPixel()} again on the same pixel (with different parameters) without having to click on a plot again to find the same pixel (in that case, be sure to set interactive=FALSE for subsequent trials!).
#' 
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' 
#' @examples
#' 
#' # load in time series raster data
#' data(tura)
#' 
#' # run bfm on a pixel of known (x,y) location
#' bfm <- bfmPixel(tura, cell=c(816750, 826020), start=c(2005, 1))
#' print(bfm$cell) # check the corresponding cell index
#' 
#' # get bfm results for a pixel of known cell index
#' bfm <- bfmPixel(tura, cell=1068, start=c(2005, 1))
#' plot(bfm$bfm)
#' # try another monitoring period
#' bfm <- bfmPixel(tura, cell=1068, start=c(2009, 1))
#' plot(bfm$bfm)
#' 
#' # try again using only ETM+ data (Landsat 7)
#' bfm <- bfmPixel(tura, cell=1068, start=c(2005, 1), sensor="ETM+")
#' 
#' # run bfm on a pixel chosen from the plot window
#' \dontrun{
#' plot(tura, 6) # a cloudless scene from 2001
#' bfm <- bfmPixel(tura, start=c(2005, 1), sensor="ETM+", interactive=TRUE)
#' plot(bfm$bfm)
#' print(targcell <- bfm$cell) # store cell index for follow-up analysis
#' }
#' ## change the model parameters
#' # 1. harmonic order
#' bfm <- bfmPixel(tura, cell=targcell, start=c(2005, 1), sensor="ETM+", order=3)
#' plot(bfm$bfm)
#' # 2. no trend
#' bfm <- bfmPixel(tura, cell=targcell, start=c(2005, 1), sensor="ETM+", order=3, formula=response~harmon)
#' plot(bfm$bfm)
#' # 3. trend only
#' bfm <- bfmPixel(tura, cell=targcell, start=c(2005, 1), sensor="ETM+", formula=response~trend)
#' plot(bfm$bfm)
#' 
#' @import raster
#' @import bfast
#' @export

bfmPixel <- function (x, sceneID=NULL, dates=NULL, start=c(), monend="full", cell=c(), min.thresh=NULL, sensor="all", interactive=FALSE, plot=FALSE, ...) 
{
  # get sceneinfo and put into a data.frame
  # layer names of the input raster brick must correspond to LS scene names!
  if(is.null(sceneID)){
    s <- getSceneinfo(names(x))
  } else {
    if(length(sceneID) != nlayers(x))
      stop("length(sceneID) should be equal to nlayers(x).")
    s <- getSceneinfo(sceneID)
  }
  
  # if sensor!='all', trim the time series 
  if (sensor!="all"){
    # if a character vector is supplied
    if("ETM+" %in% sensor)
      sensor <- c(sensor, "ETM+ SLC-on", "ETM+ SLC-off")
    layerSelect <- which(s$sensor %in% sensor)
    
    # if a numeric vector is supplied
    if(is.numeric(sensor))
      layerSelect <- which(substr(row.names(s), 3, 3) %in% sensor)
    
    # trim the brick to use only these layers; reassign layer names
    layerDrop <- c(1:nlayers(x))[-layerSelect]
    x <- dropLayer(x, layerDrop)
    s <- s[layerSelect,]
    names(x) <- row.names(s)
  }
  
  # trim time series if monend!="full" and redefine s
  ## TODO: move this step to after the ts vector is extracted
  if(monend[1]!="full"){
    end.date <- as.Date(paste(monend[1], monend[2], sep="-"), format="%Y-%j")
    x <- dropLayer(x, which(s$date > end.date))
    s <- s[-which(s$date > end.date), ]
  }
  # extract dates of new time series
  dates <- s$date
  
  # select cell from the input raster brick x in 1 of 3 ways:
  # 1) interactively (by clicking on an already plotted map)
  # 2) by supplying the cell index as an integer of length=1
  # 3) by supplying a vector of length=2 representing the (x,y) coordinates
  if (interactive) { # condition 1:
    cell <- as.data.frame(click(x, n=1, id=TRUE, cell=TRUE, show=FALSE))$cell
  } else { # conditions 2 and 3:
    cell <- ifelse(length(cell)==2, cellFromXY(x, t(as.matrix(cell))), cell)
  }
  
  # extract pixel time series
  pixelts <- bfastts(as.vector(x[cell]), dates, type=c("irregular"))
  
  # apply a threshold (if supplied)
  if (!is.null(min.thresh)) 
    pixelts[pixelts <= min.thresh] <- NA
  
  # run bfm on the pixel time series
  bfm <- bfastmonitor(data=pixelts, start=start, ...)
  
  # plot if plot=TRUE
  if(plot) 
    plot(bfm)
  
  # return a list with (1) a bfm object, and (2) the cell number (for follow-up)
  return(list(bfm=bfm, cell=cell))
}
