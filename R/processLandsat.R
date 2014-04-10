
#' @title Wrapper function to process Landsat data
#' 
#' @description Processes a single Landsat scene, from tarball (or hdf/tiff if untar is set to FALSE) to vegetation index (only NDVI supported at the moment). Easy to batch using sapply or mclapply for parallel implementation.
#' @param x Character. filename of the tarball or of the hdf/tiff file.
#' @param srdir Character. Directory where the tarball should be uncompressed. Can be ommited if \code{untar} is set to \code{FALSE}
#' @param outdir Character. Directory where the vegetation index rasterLayer should be written.
#' @param untar Logical. IS there a need to untar data, or have they been previously unpacked.
#' @param delete Logical. Should surface reflectance files (hdf/tiff) be deleted after vegetation index calculated? (usefull for disk space management; surface reflectance files are very voluminous and a user may want to keep the Landsat archive in compressed format only)
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

processLandsat <- function(x, srdir, outdir, untar=TRUE, delete=FALSE, ...) {
    # x is the full path of a tarball containing the Landsat data or the path of a hdf file
    # hdf dir is where the hdf files are extracted
    # Output layers (NDVI for example) are generated in outdir
    # ... arguments to be passed to hdf2ndvi (filename is automatically generated and therefore does not need to be passed)
    if(untar){
        tarlist <- untar(x, list=TRUE)
        x0 <- grep(pattern="^.*\\.(hdf|tif)$", tarlist, value=TRUE)
        untar(x, files=x0, exdir=srdir)
        x <- file.path(srdir, x0)
    }
    name <- str_extract(string=basename(x), '(LT4|LT5|LE7)\\d{13}')   
    # Filename generation (below) will have to be edited when dynamic indices will be implemented
    sr2vi(x=x, filename=sprintf('%s/ndvi.%s.grd', outdir, name), datatype='INT2S', ...)
    if(delete) {
        file.remove(x)
    } 
}