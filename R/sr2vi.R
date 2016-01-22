
#' @title Calculate Vegetation Index from Landsat surface reflectance data file
#' 
#' @description Extract layers, apply mask (optional), crop (optional), calculate NDVI (possible other indices supported in the future) and write output to file (optional)
#' 
#' @param x Character. File name of the hdf file containing the bands, or list of filenames (geoTiffs).
#' @param vi Character. Vegetation index to be computed. See 'details' for a list of the supported indices. The function will first attempt to find if a pre-processed version of the index is present in the list supplied or in the .hdf file and will use that layer when available.
#' @param e Extent object or object that can be coerced as extent.
#' @param mask Character or NULL. The name of the cloud/Land mask to be applied to the output (e.g.: \code{mask='fmask'}) 
#' @param keep Numeric. Can take multiple values. Which values of the mask layer should be kept?
#' @param L Numeric. Soil-adjustment factor for SAVI (ignored if vi != 'savi'). L can take on values between 0 and 1, and a default of 0.5 is typically used.
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' @return A rasterLayer object
#' @author Loic Dutrieux and Ben DeVries
#' @seealso \code{\link{processLandsat}} and \code{\link{processLandsatBatch}} for wrapper and wrapper/batcher functions
#' @import gdalUtils
#' @import raster
#' @import rgdal
#' @export
#' 
#' @details
#' A number of indices can be passed to \code{vi} at the moment, includeing \code{ndvi}, \code{evi}, \code{savi}, \code{nbr}, \code{ndmi}, \code{ndwi} and \code{mndwi}. Three tasseled cap components - brightness, greenness and wetness - are also supported. For brightness, \code{tcb}, \code{brightness} and \code{tcbright} are all acceptable arguments for \code{vi}. The same holds true for the other 2 indices.
#' 
#' For tasseled cap components, note that at the moment only a single set of coefficients for surface reflectance are used (Crist, 1985). Be aware that this may cause issues if OLI data are being used with ETM+ and TM data due to radiometric differences between OLI and the other sensors.

sr2vi <- function(x, vi='ndvi', e=NULL, mask=NULL, keep=c(0), L=0.5, ...) {      
    
    # x is a character (full filename of an hdf file)
    # filename is a character, full filename of the output file
    # The function depends on the MODIS package + raster
    # mask is a numeric, the sds number of the mask to use
    # if filename is used, I recommend also setting datatype to 'INT2S'
    
    # Functions definition
    
    # Masking function for overlay
    clean <- function(x,y) {
        x[!(y %in% keep)] <- NA
        return(x)
    }
    
    # get sceneinfo (for sensor to be passed to vi formula)
    s <- getSceneinfo(x[1])
    sensor <- s$sensor
    
    
    if(extension(x[1]) == '.hdf') { 
        x <- unlist(sapply(FUN=function(x){try(get_subdatasets(x), silent=TRUE)}, X=x), use.names=FALSE) #get_subdataset returns an error in case one of the hdfs contains no more than one sds (which can be the case when VIs are ordered via espa)
    }
    
    
    
    ###########################################################################
    # When the indices are already pre-processed ------------------------------
       
    if(any(grepl(pattern=sprintf("^.*%s($|\\.tif)", vi), x=x, ignore.case=TRUE))) { 
        vi <- raster(grep(pattern=sprintf("^.*%s($|\\.tif)", vi), x=x, value=TRUE, ignore.case=TRUE))
        if(!is.null(mask)) {
            mask <- raster(grep(pattern=sprintf("^.*%s($|\\.tif|_band$)", mask), x=x, value=TRUE)) # Called 'fmask_band' in the hdf file
        }
        
        if(!is.null(e)) {
            if(class(e) != 'extent') {
                e <- extent(e)
            }
            vi <- crop(vi, e)
            if(!is.null(mask)) {
                mask <- crop(mask, e)
            }
        }
        
        if(!is.null(mask)) {
            vi <- overlay(x=vi, y=mask, fun=clean, ...)
        }
        return(vi)
        
        
    } else {
    
    ###########################################################################
    # vi needs to be processed ------------------------------    
    # see R/LandsatVIs.R for formulas
        
        # convert vi to lower case
        vi <- tolower(vi)
    
        if(vi == 'ndvi') {
            viFormula <- .ndvi(sensor = sensor)
        } else if(vi == 'evi') {
            viFormula <- .evi(sensor = sensor)
        } else if(vi == 'nbr') {
            viFormula <- .nbr(sensor = sensor)
        } else if(vi == 'savi') {
            viFormula <- .savi(sensor = sensor, L = L)
        } else if(vi == 'ndmi') {
            viFormula <- .ndmi(sensor = sensor)
        } else if(vi == 'ndwi') {
            viFormula <- .ndwi(sensor = sensor)
        } else if(vi == 'mndwi') {
            viFormula <- .mndwi(sensor = sensor)
        } else if(vi %in% c('tcbright', 'tcbrightness', 'brightness', 'tcb')) {
            viFormula <- .tasscap(sensor = sensor, component = 'brightness')
        } else if(vi %in% c('tcgreen', 'tcgreenness', 'greenness', 'tcg')) {
            viFormula <- .tasscap(sensor = sensor, component = 'greenness')
        } else if(vi %in% c('tcwet', 'tcwetness', 'wetness', 'tcw')) {
            viFormula <- .tasscap(sensor = sensor, component = 'wetness')
        } else {
            stop("Unsupported vi")
        }

        
        
        ind <- viFormula$ind
        x0 <- grep(pattern=sprintf("^.*(%s)($|\\.tif)", paste(ind, collapse='|')), x=x, value=TRUE)
        
        bands <- lapply(X=x0, FUN=raster)
        
        
        if(!is.null(mask)) {
            mask <- raster(grep(pattern=sprintf("^.*%s($|\\.tif|_band$)", mask), x=x, value=TRUE))
        }
        
        
        if(!is.null(e)) {
            if(class(e) != 'extent') {
                e <- extent(e)
            }
            bands <- lapply(X=bands, FUN=crop, y=e)
            if(!is.null(mask)) {
                mask <- crop(mask, e)
            }
        }
        
        # VI function
        fun <- viFormula$fun
        
        
        # Prepare the list to be passed to do.call-overlay
        dots <- list(...)
        doListDots <- c(bands, fun=fun, dots)
        doList <- c(bands, fun=fun)
        
        
        if(is.null(mask)) {
            vi <- do.call(what=raster::overlay, args=doListDots)
        } else {
            previ <- do.call(what=raster::overlay, args=doList)
            vi <- overlay(x=previ, y=mask, fun=clean, ...)
        }
        
        return(vi)
    }
}

