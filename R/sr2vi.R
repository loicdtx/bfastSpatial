
#' @title Calculate Vegetation Index from Landsat surface reflectance data file
#' 
#' @description Extract layers, apply mask (optional), crop (optional), calculate NDVI (possible other indices supported in the future) and write output to file (optional)
#' 
#' @param x Character. File name of the hdf file containing the bands, or list of filenames (geoTiffs).
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

sr2vi <- function(x, e=NULL, mask=NULL, keep=c(0), ...) {      
    
    # x is a character (full filename of an hdf file)
    # filename is a character, full filename of the output file
    # The function depends on the MODIS package + raster
    # mask is a numeric, the sds number of the mask to use
    # if filename is used, I recommend also setting datatype to 'INT2S'
    if(extension(x) == '.hdf') { # Also should be the only case when length(x) == 1
        x <- get_subdatasets(x[1])
    }
    
    ind <- c(3,4) # To facilitate introduction of other indices later
    x0 <- x[ind]
    
    bands <- sapply(X=x0, FUN=raster)
    
    
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
    
    
    # NDVI function for calc
    fun <- function(x, y){
        10000 * (y - x)/(y + x) # Watch the scaling factor here
    }
    
    # Masking function for overlay
    clean <- function(x,y) {
        x[!(y %in% keep)] <- NA
        return(x)
    }
    
    if(is.null(mask)) {
        ndvi <- overlay(x=bands[1], y=bands[2], fun=fun, ...)
    } else {
        prendvi <- overlay(x=bands[1], y=bands[2], fun=fun, ...)
        ndvi <- overlay(x=prendvi, y=mask, fun=clean, ...)
    }
    
    return(ndvi)
}