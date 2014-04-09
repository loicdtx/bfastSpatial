
#' @title Process Landsat data in batch mode
#' 
#' @description Batcher to process Landsat data from tarball or hdf to a list of Vegetation Index files. Runs \link{processLandsat} sequentially or in parallel
#' 
#'
#' @param x Character. Directory where the data is located (hdf or tar.gz)
#' @param pattern. Only useful if x if of length 1. See \link{list.files} for more details
#' @param outdir. Character. Directory where the vegetation index rasterLayer should be written.
#' @param hdfdir Character. Directory where the tarball should be uncompressed. Can be ommited if \code{untar} is set to \code{FALSE}
#' @param mc.cores Numeric. For multicore implementation only. See \link{mclapply}
#' @param ... Arguments to be passed to \link{processLandsat} (\code{untar}, \code{delete}) or to \link{hdf2ndvi} (\code{e}, \code{mask}, \code{keep})
#' @author Loic Dutrieux
#' @return Function is used for its side effect of calculating in batch Vegetation indices fron surface reflectance Lantsat data.
#' @seealso \link{processLandsat} and \link{hdf2ndvi}
#' @examples
#' # Get the directory where the Landsat archives are storred
#' dir <- system.file('external', package='bfastSpatial')
#' hdfdir <- dirout <- rasterOptions()$tmpdir
#' processLandsatBatch(x=dir, pattern=glob2rx('*.tar.gz'), outdir=dirout, hdfdir=hdfdir, delete=TRUE, mask=17)
#' 
#' # Visualize one of the layers produced
#' 
#' 
#' 
#' 
#' 
#' @import stringr
#' @import raster
#' @import rgdal
#' @import gdalUtils
#' @import parallel
#' @export
#' 


processLandsatBatch <- function(x, pattern=NULL, outdir, hdfdir, mc.cores=1, ...) {
    
    if (!is.character(x)) {
        stop('x needs to be of class character')
    }
    
    if(length(x) == 1) {
        x <- list.files(path=x, pattern=pattern, full.names=TRUE)
    }
    
    
    mclapply(X=x, FUN=processLandsat, outdir=outdir, hdfdir=hdfdir, mc.cores=mc.cores, ...)
    
    
    
}