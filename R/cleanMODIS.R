#' Clean MODIS
#' 
#' @description Uses MODIS quality layer to perform cleaning over a single data layer. Quality band can be encoded bitewise or in numerics. This function needs rgdal to be configured with HDF4 driver in order to work.
#' 
#' @param x Character. Filename (full path) of a hdf dataset containing data and QC layers
#' @param data_SDS Numeric. Index of the SDS containing the data in the hdf infrastructure
#' @param QC_SDS Numeric. Index of the SDS containing the Quality control in the hdf infrastructure
#' @param bit Logical. Is QC information provided bitwise?
#' @param QC_val Numeric or vector of numerics. Quality control values to \textbf{keep} in the data. If \code{bit} is set to \code{TRUE} \code{QC_val} is a BYTE (hexadecimal or decimal) where each bit refers to a bit in the QC layer element (i.e.: bitpos = 0xA1 targets bits 7, 5 and 0 --- 1010 0001). When bits targetted by \code{QC_val} are activated, the corresponding observation in the data layer is filtered out.
#' @param fill (vector of) Numeric or NULL. Fill values in the data layer to be filtered out. (important to do before reprojection with resampling method other than Nearest Neighbours)
#' @param ... Arguments to be passed to \link{\code{writeRaster}}.
#' 
#' @return A rasterLayer
#' 
#' @author Loic Dutrieux
#' 
#' @seealso \code{\link{processMODISbatch}} for batcher
#' 
#' @examples
#' \dontrun{
#' library(raster)
#' dir <- file.path(tmpDir(), 'MODIStest'); dir.create(path = dir)
#' 
#' fileDL <- 'http://e4ftl01.cr.usgs.gov/MOLT/MOD17A2.055/2003.07.12/MOD17A2.A2003193.h19v07.055.2011269202445.hdf'
#' modis <- file.path(dir, basename(fileDL)); download.file(url = fileDL, destfile = modis)
#'
#' # Now We've just downloaded a MODIS file that is stored in a subdirectory of the raster tmp directory
#' sprintf('These data have been acquired around %s', getMODISinfo(modis)$date)
#' 
#' # Clean dataset and replace fill values by NAs
#' # Product details at https://lpdaac.usgs.gov/products/modis_products_table/mod17a2
#' MODISclean <- cleanMODIS(x=modis, data_SDS=1, QC_SDS=3, bit=TRUE, QC_val=0x19, fill = (32761:32767))
#' plot(MODISclean)
#' 
#' # In that case we did not write the result back to disk, but note that this is possible
#' # For that use the filename= argument, and the datatype= argument (recommended)
#' }
#' 
#' @import raster
#' @import gdalUtils
#' @import bitops
#' @import rgdal
#' @export
#' 
#' 

cleanMODIS <- function(x, data_SDS, QC_SDS, bit=FALSE, QC_val, fill=NULL, ...){
    
    if(!'HDF4' %in% gdalDrivers()$name){
        stop('This command requires rgdal to be configured with HDF4 driver')
    }
    
    
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