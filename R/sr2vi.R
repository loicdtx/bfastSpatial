
#' @title Calculate Vegetation Index from Landsat surface reflectance data file
#' 
#' @description Extract layers, apply mask (optional), crop (optional), calculate NDVI (possible other indices supported in the future) and write output to file (optional)
#' 
#' @param x Character. File name of the hdf file containing the bands, or list of filenames (geoTiffs).
#' @param vi Character. Vegetation index to be computed. 'ndvi', 'evi' and 'savi' are supported at the moment. The function will first attempt to find if a pre-processed version of the index is present in the list supplied or in the .hdf file and will use that layer when available.
#' @param e Extent object or object that can be coerced as extent.
#' @param mask Character or NULL. The name of the cloud/Land mask to be applied to the output (e.g.: \code{mask='fmask'}) 
#' @param keep Numeric. Can take multiple values. Which values of the mask layer should be kept?
#' @param L Numeric. Soil-adjustment factor for SAVI (ignored if vi != 'savi'). L can take on values between 0 and 1, and a default of 0.5 is typically used.
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' @return A rasterLayer object
#' @author Loic Dutrieux
#' @seealso \code{\link{processLandsat}} for wrapper function
#' @import gdalUtils
#' @import raster
#' @import rgdal
#' @export

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
    
    
        if(vi == 'ndvi') {
            viFormula <- .ndvi()
        } else if(vi == 'evi') {
            viFormula <- .evi()
        } else if(vi == 'nbr') {
            viFormula <- .nbr()
        } else if(vi == 'savi') {
            viFormula <- .savi(L=L)
        } else {
            stop("Unsupported vi")
        }
        # TODO: for tasseled cap to work, information on the Landsat sensor is needed here:
        # e.g. 
            # if(sensor==7 & vi=="tcgreen") viFormula <- .tcgreen(sensor=7)
        # or if 'sensor' is added as an argument, simply:
            # if(vi == "tcgreen") viFormula <- .tcgreen(sensor=sensor)
        
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
            s <- crop(s, e)
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

