#' @title Clean the history period of a raster time series
#' 
#' @description Filter a time series RasterBrick or RasterStack using a static or statistical threshold in a defined historical period
#' 
#' @param x RasterBrick or RasterStack. Raster time series to be cleaned
#' @param monperiod Numeric. Beginning of intended monitoring period in \link{\code{bfmSpatial}} in the form \code{c(year, Julian day)}. The filter will only be applied before this period (ie. to the history period). If set to \code{NULL}, the filter will be applied to the entire time series.
#' @param thresh Either a Numeric static threshold or \code{"IQR"} to calcalate the threshold per pixel based on the interquartile zone.
#' @param dates Date. Vector of dates corresponding exactly to layers in x. If \code{NULL}, dates are either extracted from \code{sceneID} or \code{names(x)} using \link{code{getSceneinfo}}.
#' @param sceneID Character. Vector of Landsat sceneID's 
#' @param is.max Logical. Is \code{thresh} to be treated as a maximum threshold (ie. all values higher will be removed)? If \code{FALSE}, all values lower than \code{thresh} will be removed (default).
#' @param ... Additional parameters to pass to \code{\link{mc.calc}}
#' 
#' @return RasterBrick with values removed according to \code{thresh}.
#' 
#' @details If \code{dates} is not supplied, these will be extracted from sceneID's, with only support for Landsat at this time. If data come from another sensor, be sure to supply a \code{dates} vector.
#' 
#' @author Ben DeVries (\email{devries.br@@gmail.com}) and Jan Verbesselt
#' 
#' @import raster
#' @export
#' 
#' 

cleanBrick <- function(x, monperiod, thresh, dates=NULL, sceneID=NULL, is.max=FALSE, ...){
    
    # if dates==NULL, get dates vector either from sceneID or from names(x)
    if(is.null(dates) & !is.null(sceneID)){
        dates <- getSceneinfo(sceneID)$date
    } else if(is.null(dates) & is.null(sceneID)){
        dates <- getSceneinfo(names(x))$date
    }
    
    # reformat monperiod
    monperiod <- as.Date(paste(monperiod, collapse="-"), format="%Y-%j")
    
    ## define a pixelwise function depending on the nature of the threshold / conditions
    # 1a. if thresh is numeric and is.max=FALSE, apply the threshold as a min thresh
    # 1b. if thresh is numeric and is.max=TRUE, apply the threshold as a max thresh
    # 2a. if thresh=="IQR" and is.max=FALSE, apply the threshold as a max thresh defined as:
                # median - IQR
    # 2b. if thresh=="IQR" and is.max=TRUE, as above, but in reverse
    if(is.numeric(thresh)){
        fun <- function(y){
            if(!is.max)
             y[y < thresh & dates < monperiod] <- NA
            else
             y[y > thresh & dates < monperiod] <- NA
            
            return(y)
        }
    } else if(thresh=="IQR"){
        fun <- function(y){
            if(!is.max){
                thresh <- median(y, na.rm=TRUE) - IQR(y, na.rm=TRUE)
                y[y < thresh & dates < monperiod] <- NA
            } else {
                thresh <- median(y, na.rm=TRUE) + IQR(y, na.rm=TRUE)
                y[y > thresh & dates < monperiod] <- NA
            }
            
            return(y)
        }
    }
    
    # pass to mc.calc
    z <- mc.calc(x, fun=fun, ...)
    
    return(z)
}