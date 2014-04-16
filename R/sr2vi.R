
#' @title Calculate Vegetation Index from Landsat surface reflectance data file
#' 
#' @description Extract layers, apply mask (optional), crop (optional), calculate NDVI (possible other indices supported in the future) and write output to file (optional)
#' 
#' @param x Character. File name of the hdf file containing the bands, or list of filenames (geoTiffs).
#' @param vi Character. Vegetation index to be computed. Can be either 'ndvi' or 'evi'
#' @param e Extent object or object that can be coerced as extent.
#' @param mask Numeric or NULL. The subdataset number of the mask to be applied to the bands.
#' @param keep umeric. Can take multiple values. Which values of the mask layer should be kept?
#' @param ... Arguments to be passed to \code{\link{writeRaster}}
#' @return A rasterLayer object
#' @author Loic Dutrieux
#' @seealso \code{\link{processLandsat}} for wrapper function
#' @import gdalUtils
#' @import raster
#' @import rgdal
#' @export

sr2vi <- function(x, vi='ndvi', e=NULL, mask=NULL, keep=c(0), ...) {      
    
    # x is a character (full filename of an hdf file)
    # filename is a character, full filename of the output file
    # The function depends on the MODIS package + raster
    # mask is a numeric, the sds number of the mask to use
    # if filename is used, I recommend also setting datatype to 'INT2S'
    if(extension(x) == '.hdf') { # Also should be the only case when length(x) == 1
        x <- get_subdatasets(x[1])
    }
    
    if(vi == 'ndvi') {
        viFormula <- .ndvi()
    } else if(vi == 'evi') {
        viFormula <- .evi()
    } else {
        stop("Non supported vi")
    }
        
    
    ind <- viFormula$ind
    x0 <- x[ind]
    
    bands <- lapply(X=x0, FUN=raster)
    
    
    if(!is.null(mask)) {
        mask <- raster(x[mask])
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
    
    # Masking function for overlay
    clean <- function(x,y) {
        x[!(y %in% keep)] <- NA
        return(x)
    }
    
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