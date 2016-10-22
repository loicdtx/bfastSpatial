
#' @title Wrapper function to process Landsat data
#' 
#' @description Processes a single Landsat scene, from tarball or zip archive (or hdf/tiff if untar is set to FALSE) to vegetation index. Easy to batch using sapply or mclapply for parallel implementation. Data obtained from espi may already contained pre-processed indices layers, in which case they are directly used.
#' @param x Character. filename of the tarball or zip archive of the hdf/tiff file.
#' @param vi Character. Vegetation index to be computed or extracted from the archive. Can be either 'ndvi', 'evi', 'savi', 'ndmi', 'ndwi', 'mndwi', 'tcb', 'tcg', 'tcw', 'nbr', 'nbr2'* or 'msavi'*. Indices with * need to be present in the archive. Note that it is also possible to extract single bands using the \code{vi=} argument. \code{vi='sr_band1'} for instance will extract surface reflectance band 1 from the archive and perform the same pre-processing steps as if it was a vegetation index layer.
#' @param srdir Character. Directory where the tarball should be uncompressed. Can be ommited if \code{untar} is set to \code{FALSE}
#' @param outdir Character. Directory where the vegetation index rasterLayer should be written.
#' @param untar Logical. Is there a need to untar data, or have they been previously unpacked.
#' @param delete Logical. Should surface reflectance files (hdf/tiff) be deleted after vegetation index calculated? (usefull for disk space management; surface reflectance files are very voluminous and a user may want to keep the Landsat archive in compressed format only)
#' @param mask Character or NULL. The name of the mask to be applied to the bands (e.g.: \code{mask = 'fmask'})
#' @param L Numeric. Soil-adjustment factor for SAVI (ignored if \code{vi != 'savi'}). L can take on values between 0 and 1, and a default of 0.5 is typically used.
#' @param fileExt Character. Extension of the file to be generated. Note that \code{filename} is automatically generated
#' @param ... Arguments to be passed to \link{sr2vi}. Do not specify \code{filename} since it is automatically generated
#' @author Loic Dutrieux
#' @return rasterLayer Object also written to file (in \code{outdir}) with an automatically generated filename
#' @seealso \link{sr2vi}
#' 
#' @import stringr
#' @import raster
#' @import rgdal
#' @import gdalUtils
#' 
#' @examples
#' # Get list of test data files
#' dir <- system.file('external', package='bfastSpatial')
#' list <- list.files(dir, full.names=TRUE)
#' 
#' # Set the location of output and intermediary directories (everything in tmpdir in that case)
#' srdir <- dirout <- file.path(rasterOptions()$tmpdir, 'bfmspatial')
#' dir.create(dirout, showWarning=FALSE)
#' # Generate (or extract, depending on whether the layer is already in the archive or not) NDVI for the first archive file
#' processLandsat(x=list[1], vi='ndvi', outdir=dirout, srdir=srdir, delete=TRUE, mask='fmask', keep=0, overwrite=TRUE)
#' 
#' # Visualize one of the layers produced
#' list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
#' plot(r <- raster(list[1]))
#' 
#' @export
#' 

processLandsat <- function(x, vi='ndvi', srdir, outdir, untar=TRUE, delete=FALSE, mask=NULL, L=0.5, fileExt = 'grd', ...) {
    # x is the full path of a tarball containing the Landsat data or the path of a hdf file
    # hdf dir is where the hdf files are extracted
    # Output layers (NDVI for example) are generated in outdir
    # ... arguments to be passed to hdf2ndvi (filename is automatically generated and therefore does not need to be passed)
    
    # get sceneinfo (sensor needed)
    s <- getSceneinfo(basename(x))
    sensor <- as.character(s$sensor)
    
    # Although x can be a zip archive, Names are untar, tarlist, etc, since the function was first developped to deal with tar.gz compressed Landsat data
    if(untar){
        ex <- extension(x)
        if(ex == '.gz') {
            tarlist <- untar(x, list=TRUE)
        } else if(ex == '.zip') {
            tarlist <- unzip(x, list=TRUE)$Name
        } else {
            stop('The archive is neither tar.gz nor .zip; we don\'t know what to do with that.')
        }
        
        
        if(any(grepl(pattern="^.*\\.hdf$", x=tarlist))) { # are there any hdf files
            x0 <- grep(pattern="^.*\\.hdf$", x=tarlist, value=TRUE)
        } else if (any(grepl(pattern="^.*\\.tif$", x=tarlist))) { # Contains tiff
            if(any(grepl(pattern=sprintf("^.*_%s\\.tif$", vi), x=tarlist))) { # Contains pre-processed vi
                x0 <- grep(pattern=sprintf("^.*_%s\\.tif$", vi), x=tarlist, value=TRUE)
            } else { # extract the bands needed to process vi
                # Get viFormula object (in order to know which bands to extract)
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
                x0 <- grep(pattern=sprintf("^.*_(%s)\\.tif$", paste(viFormula$ind, collapse='|')), x=tarlist, value=TRUE)
            }
            
        } else {
            stop("Did not find any .tif or .hdf files in the archive")
        }
        if (!is.null(mask)) {
            x0 <- c(x0, grep(pattern=sprintf("^.*%s\\.tif$", mask), x=tarlist, value=TRUE))
        }
        if(ex == '.gz') {
            untar(x, files=x0, exdir=srdir)
        } else if(ex == '.zip') {
            unzip(x, files=x0, exdir=srdir)
        }
        
        x <- sort(file.path(srdir, x0))
    }
    
    name <- str_extract(string=basename(x[1]), '(LT4|LT5|LE7|LC8)\\d{13}')   
    # Filename generation (below) will have to be edited when dynamic indices will be implemented
    # Also note that in case of geotiff length(x)>1
    sr2vi(x=x, vi=vi, filename=sprintf('%s/%s.%s.%s', outdir, vi, name, fileExt), datatype='INT2S', mask=mask, ...)
    if(delete) {
        file.remove(x)
    } 
}