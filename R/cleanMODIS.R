#' Clean MODIS
#' 
#' @param x Character. Filename (full path) of a hdf dataset containing data and QC layers
#' @param data_SDS Numeric. Index of the SDS containing the data in the hdf infrastructure
#' @param QC_SDS Numeric. Index of the SDS containing the Quality control in the hdf infrastructure
#' @param bit Logical. Is QC information provided bitwise?
#' @param QC_val Numeric or vector of numerics. Quality control values to \textbf{keep} in the data. If \code{bit} is set to \code{TRUE} \code{QC_val} is a BYTE (hexadecimal or decimal) where each bit refers to a bit in the QC layer element (i.e.: bitpos = 0xA1 targets bits 7, 5 and 0 --- 1010 0001). When bits targetted by \code{QC_val} are activated, the corresponding observation in the data layer is filtered out.
#' @param ... Arguments to be passed to \link{\code{writeRaster}}.
#' @param fill (vector of) Numeric or NULL. Fill values in the data layer to be filtered out. (important to do before reprojection with resampling method other than Nearest Neighbours)
#' 
#' @return A rasterLayer
#' 
#' @author Loic Dutrieux
#' 
#' @import raster
#' @import gdalUtils
#' @import bitops
#' @import rgdal
#' @export
#' 
#' 

cleanMODIS <- function(x, data_SDS, QC_SDS, bit=FALSE, QC_val,  ...){
    
    sds <- get_subdatasets(x)
    data <- raster(readGDAL(sds[data_SDS], as.is=TRUE))
    QC <- raster(sds[QC_SDS])
    
    clean <- function(x, y) {
        if(bit) {
            x[bitAnd(y, QC_val) != 0] <- NA
        } else {
            x[!y %in% QC_val] <- NA
        }
        if(!is.null(fill)) {
            x[x %in% fill] <- NA
        }
        return(x)
    }
    
    
    overlay(x = data, y = QC, fun = clean,...)

}