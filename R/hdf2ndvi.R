

hdf2ndvi <- function(x, e=NULL, mask=NULL, keep=c(0), ...) {      
    
    # x is a character (full filename of an hdf file)
    # filename is a character, full filename of the output file
    # The function depends on the MODIS package + raster
    # mask is a numeric, the sds number of the mask to use
    # if filename is used, I recommend also setting datatype to 'INT2S'
    
    SDS <- get_subdatasets(x)
    b4 <- raster(SDS[4])
    b3 <- raster(SDS[3])
    if(!is.null(mask)) {
        mask <- raster(SDS[mask])
    }
    
    
    if(!is.null(e)) {
        if(class(e) != 'extent') {
            e <- extent(e)
        }
        b3 <- crop(b3, e)
        b4 <- crop(b4, e)
        if(!is.null(mask)) {
            mask <- crop(mask, e)
        }
    }
    
    
    # NDVI function for overlay
    fun <- function(x,y){
        out <- 10000 * (y-x)/(y+x) # Watch the scaling factor here
        return(out)
    }
    
    # Masking function for overlay
    clean <- function(x,y) {
        x[!(y %in% keep)] <- NA
        return(x)
    }
    
    if(is.null(mask)) {
        ndvi <- overlay(x=b3, y=b4, fun=fun, ...)
    } else {
        prendvi <- overlay(x=b3, y=b4, fun=fun)
        ndvi <- overlay(x=prendvi, y=mask, fun=clean, ...)
    }
    
    return(ndvi)
}