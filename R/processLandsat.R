
#' @title Wrapper function to process Landsat data
#' 
#' @description Processes a single Landsat scene, from tarball (or hdf/tiff if untar is set to FALSE) to vegetation index (only NDVI supported at the moment). Easy to batch using sapply or mclapply for parallel implementation.
#' @param x Character. filename of the tarball or of the hdf/tiff file.
#' @param vi Character. Vegetation index to be computed. Can be either 'ndvi' or 'evi'
#' @param srdir Character. Directory where the tarball should be uncompressed. Can be ommited if \code{untar} is set to \code{FALSE}
#' @param outdir Character. Directory where the vegetation index rasterLayer should be written.
#' @param untar Logical. IS there a need to untar data, or have they been previously unpacked.
#' @param delete Logical. Should surface reflectance files (hdf/tiff) be deleted after vegetation index calculated? (usefull for disk space management; surface reflectance files are very voluminous and a user may want to keep the Landsat archive in compressed format only)
#' @param mask Numeric or NULL. The subdataset number of the mask to be applied to the bands.
#' @param L Numeric. Soil-adjustment factor for SAVI (ignored if vi != 'savi'). L can take on values between 0 and 1, and a default of 0.5 is typically used.
#' @param ... Arguments to be passed to \link{sr2vi}. Do not specify \code{filename} since it is automatically generated
#' @author Loic Dutrieux
#' @return rasterLayer Object also written to file (in \code{outdir}) with an automatically generated filename
#' @seealso \link{sr2vi}
#' @import stringr
#' @import raster
#' @import rgdal
#' @import gdalUtils
#' @export
#' 

processLandsat <- function(x, vi='ndvi', srdir, outdir, untar=TRUE, delete=FALSE, mask=NULL, L=0.5, ...) {
    # x is the full path of a tarball containing the Landsat data or the path of a hdf file
    # hdf dir is where the hdf files are extracted
    # Output layers (NDVI for example) are generated in outdir
    # ... arguments to be passed to hdf2ndvi (filename is automatically generated and therefore does not need to be passed)
    
    
    if(untar){
        tarlist <- untar(x, list=TRUE)
        if(any(grepl(pattern="^.*\\.hdf$", x=tarlist))) { # are there any hdf files
            x0 <- grep(pattern="^.*\\.hdf$", x=tarlist, value=TRUE)
        } else if (any(grepl(pattern="^.*\\.tif$", x=tarlist))) { # Contains tiff
            if(any(grepl(pattern=sprintf("^.*%s\\.tif$", vi), x=tarlist))) { # Contains pre-processed vi
                x0 <- grep(pattern=sprintf("^.*%s\\.tif$", vi), x=tarlist, value=TRUE)
            } else { # extract the bands needed to process vi
                # Get viFormula object
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
                x0 <- grep(pattern=sprintf("^.*(%s)\\.tif$", paste(viFormula$ind, collapse='|')), x=tarlist, value=TRUE)
            }
            
        } else {
            stop("Did not find any .tif or .hdf files in the archive")
        }
        if (!is.null(mask)) {
            x0 <- c(x0, grep(pattern=sprintf("^.*%s\\.tif$", mask), x=tarlist, value=TRUE))
        }
        untar(x, files=x0, exdir=srdir)
        x <- file.path(srdir, x0)
    }
    name <- str_extract(string=basename(x[1]), '(LT4|LT5|LE7|LC8)\\d{13}')   
    # Filename generation (below) will have to be edited when dynamic indices will be implemented
    # Also note that in case of geotiff length(x)>1
    sr2vi(x=x, vi=vi, filename=sprintf('%s/%s.%s.grd', outdir, vi, name), datatype='INT2S', mask=mask, ...)
    if(delete) {
        file.remove(x)
    } 
}