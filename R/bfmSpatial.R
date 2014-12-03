
#' @title Function to run bfastmonitor on any kind of raster brick, with parallel support
#' 
#' @description Implements bfastmonitor function, from the bfast package on any kind of rasterBrick object. Time information is provided as an extra object and the time series can be regular or irregular.
#' 
#' @param x rasterBrick or rasterStack object, or file name to a multilayer raster object stored on disk.
#' @param dates A date vector. The number of dates must match the number of layers of x.
#' @param pptype Character. Type of preprocessing to be applied to individual time series vectors. The two options are 'irregular' and '16-days'. See \code{\link{bfastts}} for more details.
#' @param start See \code{\link{bfastmonitor}}
#' @param monend Numeric. Optional: end of the monitoring period in the format c(year, julian day). All raster data after this time will be removed before running \code{bfastmonitor}
#' @param formula See \code{\link{bfastmonitor}}
#' @param order See \code{\link{bfastmonitor}}
#' @param lag See \code{\link{bfastmonitor}}
#' @param slag See \code{\link{bfastmonitor}}
#' @param history See \code{\link{bfastmonitor}}
#' @param type See \code{\link{bfastmonitor}}
#' @param n See \code{\link{bfastmonitor}}
#' @param level See \code{\link{bfastmonitor}}
#' @param mc.cores Numeric. Number of cores to be used for the job.
#' @param returnLayers Character. Result layers to be returned. Can be any combination of \code{c("breakpoint", "magnitude", "error", "history", "r.squared", "adj.r.squared", "coefficients")}. By default, \code{breakpoint}, \code{magnitude} and \code{error} are returned by the function. See \code{details} for more information.
#' @param sensor Character. Optional: Limit analysis to one or more particular sensors. Can be any combintation of \code{c("ETM+", "ETM+ SLC-on", "ETM+ SLC-off", "TM", or "OLI")}
#' @param ... Arguments to be passed to \code{\link{mc.calc}}
#' 
#' @return A rasterBrick with layers depending on what has been supplied to \code{returnLayers}. See details for more information.
#' 
#' @details
#' \code{bfmSpatial} applies \code{\link{bfastmonitor}} over a raster time series. For large raster datasets, processing times can be long. Given the number of parameters that can be set, it is recommended to first run \code{\link{bfmPixel}} over some test pixels or \code{bfmSpatial} over a small test area to gain familiarity with the time series being analyzed and to test several parameters.
#' 
#' Note that there is a difference between the \code{monend} argument included here and the \code{end} argument passed to \code{\link{bfastmonitor}}. Supplying a date in the format \code{c(year, Julian day)} to \code{monend} will result in the time series being trimmed \emph{before} running \code{\link{bfastmonitor}}. While this may seem identical to trimming the resulting \code{bfastmonitor} object per pixel, trimming the time series before running \code{bfastmonitor} will have an impact on the change magnitude layer, which is calculated as the median residual withint the entire monitoring period, whether or not a breakpoint is detected.
#' 
#' While \code{bfmSpatial} can be applied over any raster time series with a time dimension (implicit or externally supplied), an additional feature relating to the type of Landsat sensor is also included here. This feature allows the user to specify data from a particular sensor, excluding all others. The \code{sensor} argument accepts any combination of the following characters (also see \code{\link{getSceneinfo}}): "all" - all layers; "TM" - Landsat 5 Thematic Mapper; "ETM+" - Landsat 7 Enhanced Thematic Mapper Plus (all); "ETM+ SLC-on" - ETM+ data before failure of Scan Line Corrector; ETM+ data after failure of the Scan Line Corrector.
#' 
#' Note that for the \code{sensor} argument to work, \code{names(x)} must correspond to Landsat sceneID's (see \code{\link{getSceneinfo}}), otherwise any values passed to \code{sensor} will be ignored with a warning.
#' 
#' \code{returnLayers} can be used to specify which \code{bfasmonitor} results to return. Regardless of which parameters are assigned, the output layers will always follow the order: \code{c("breakpoint", "magnitude", "error", "history", "r.squared", "adj.r.squared", "coefficients")}. This is important if \code{mc.cores} is set to be greater than 1, since this causes the layer names in the output brick to be lost, so it is important to know which layers have been requested and in which order they will be exported. Note that if "coefficients" is included, the output will include the following: "(Intercept)" and any trend and/or harmonic coefficients depending on the values of \code{formula} and \code{order}.
#' 
#' @author Loic Dutrieux and Ben DeVries
#' @import bfast
#' @import parallel
#' @import raster
#' 
#' @seealso \code{\link{bfastmonitor}}, \code{\link{bfmPixel}}
#' 
#' @examples
#' # load tura dataset
#' data(tura)
#' 
#' # run BFM over entire time series with a monitoring period starting at the beginning of 2009
#' t1 <- system.time(bfm <- bfmSpatial(tura, start=c(2009, 1)))
#' 
#' \dontrun{
#' # with multi-core support (see ?mc.calc)
#' t2 <- system.time(bfm <- bfmSpatial(tura, start=c(2009, 1), mc.cores=4))
#' # difference processing time
#' t1 - t2
#' }
#' 
#' # plot the result
#' plot(bfm)
#' 
#' @export
#' 


# Author: Loic Dutrieux
# January 2014

bfmSpatial <- function(x, dates=NULL, pptype='irregular', start, monend=NULL,
                       formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL,
                       history = c("ROC", "BP", "all"), aggre="month",
                       type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, mc.cores=1, returnLayers = c("breakpoint", "magnitude", "error"), sensor=NULL, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }
        
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
    
    # optional: reformat sensor if needed
    if("ETM+" %in% sensor)
        sensor <- c(sensor, "ETM+ SLC-on", "ETM+ SLC-off")
    
    # optional: get Landsat sceneinfo if sensor is supplied
    # ignore sensor if names(x) are not Landsat sceneID's
    if(!is.null(sensor)){
        if(!.isLandsatSceneID(x)){
            warning("Cannot subset by sensor if names(x) do not correspond to Landsat sceneID's. Ignoring...\n")
            sensor <- NULL
        } else {
            s <- getSceneinfo(names(x))
            s <- s[which(s$sensor %in% sensor), ]
        }
    }
    
    # determine length of coefficient vector
    # = intercept [+ trend] [+ harmoncos*order] [+ harmonsin*order]
    coef_len <- 1 # intercept
    modterms <- attr(terms(formula), "term.labels")
    if("trend" %in% modterms)
        coef_len <- coef_len + 1
    if("harmon" %in% modterms)
        coef_len <- coef_len + (order * 2) # sin and cos terms

    fun <- function(x, aggre=aggre,dates=dates) {
        # subset x by sensor
        if(!is.null(sensor))
            x <- x[which(s$sensor %in% sensor)]
        
        # convert to bfast ts
       # ts <- bfastts(x, dates=dates, type=pptype)
        if (aggre  == "month")
    {  
      spt<-zoo(x,dates)
      monmean <- aggregate(spt, as.Date(as.yearmon(dates)), mean)
      
      frequency(monmean)<-12
      na.new <- function(x) ts(na.exclude(x), frequency = 12)
      
      stlmon<-stl(monmean, na.action = na.new, s.window = "per")
   
      datamon <- ts(rowSums(stlmon$time.series)) 
      tsp(datamon) <- tsp(stlmon$time.series)
      ts<-datamon
    }
    else
    {
      
      #spt<-ts( x,start=c(2000,7),end=c(2013,44),frequency=46)
      spt<-zoo(x,dates)
      frequency(spt)<-46
      na.new <- function(x) ts(na.exclude(x), frequency = 46)
      stlmon<-stl(spt, na.action = na.new, s.window = "per")
      spt <- ts(rowSums(stlmon$time.series)) 
      tsp(spt) <- tsp(stlmon$time.series)
      ts<-spt
    }
     
        #optional: apply window() if monend is supplied
        if(!is.null(monend))
            ts <- window(ts, end=monend)
        
        # run bfastmonitor(), or assign NA if only NA's (ie. if a mask has been applied)
        if(!all(is.na(ts))){
            bfm <- try(bfastmonitor(data=ts, start=start,
                                    formula=formula,
                                    order=order, lag=lag, slag=slag,
                                    history=history,
                                    type=type, h=h,
                                    end=end, level=level), silent=TRUE)
            
            # assign 1 to error and NA to all other fields if an error is encountered
            if(class(bfm) == 'try-error') {
                bkpt <- NA
                magn <- NA
                err <- 1
                history <- NA
                rsq <- NA
                adj_rsq <- NA
                coefficients <- rep(NA, coef_len)
            } else {
                bkpt <- bfm$breakpoint
                magn <- bfm$magnitude
                err <- NA
                history <- bfm$history[2] - bfm$history[1]
                rsq <- summary(bfm$model)$r.squared
                adj_rsq <- summary(bfm$model)$adj.r.squared
                coefficients <- coef(bfm$model)
            }
        } else {
            bkpt <- NA
            magn <- NA
            err <- NA
            history <- NA
            rsq <- NA
            adj_rsq <- NA
            coefficients <- rep(NA, coef_len)
        }
        res <- c(bkpt, magn, err, history, rsq, adj_rsq)
        names(res) <- c("breakpoint", "magnitude", "error", "history", "r.squared", "adj.r.squared")
        res <- res[which(names(res) %in% returnLayers)]
        if("coefficients" %in% returnLayers)
            res <- c(res, coefficients)
        return(res)
    }
    
    out <- mc.calc(x=x, fun=fun, mc.cores=mc.cores, ...)
    
    return(out)
}
