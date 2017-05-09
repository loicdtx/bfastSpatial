#' Internal function to prepare the data extraction and processing needs, designed to be used within .ppInit
#' 
#' @param vi Character. Name of a vegetation index (usually passed to processLandsat as the vi argument)
#' @param pp list. The list containing the preprocessing parameters
#' 
#' @return The list passed as pp argument with toExtract and processingMeta slots filled
.viAction <- function(vi, pp) {
    # Get variable from bfastSpatial package environment
    VEGETATION_INDICES <- bfastSpatial.env$VEGETATION_INDICES
    # Build vi pattern
    viPattern = sprintf('.*sr_(%s)\\.(tif|grd)$', vi)
    if(any(grepl(pattern = viPattern, x = pp$unpackedBands))) {
        # check if vi is directly present in unpackedBand
        processingMeta <- list(vi = vi,
                              needProcessing = FALSE,
                              vi_file = grep(pattern = viPattern, x = pp$unpackedBands, value = TRUE),
                              filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
        pp$processingMeta[[vi]] <- processingMeta
    } else if(any(grepl(pattern = viPattern, x = pp$archivedBands))) {
        # check if vi is directly present in archived bands
        archived_band <- grep(pattern = viPattern, x = pp$archivedBands, value = TRUE)
        pp$toExtract <- union(pp$toExtract, archived_band)
        processingMeta <- list(vi = vi,
                              needProcessing = FALSE,
                              vi_file = file.path(pp$srdir, archived_band),
                              filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
        pp$processingMeta[[vi]] <- processingMeta
    } else {
        # We need to get the SR bands
        sr_bands <- VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['bands']]
        bands_pattern <- sprintf('.*_%s\\.(tif|grd)$', paste(sr_bands, collapse = '|'))
        if(sum(grepl(pattern = bands_pattern, pp$unpackedBands)) == length(sr_bands)){ # In srdir, Number of matches equals number of required spectral bands
            processingMeta <- list(vi = vi,
                                  needProcessing = TRUE,
                                  sr_files = unname(sapply(sr_bands, function(x){grep(pattern = sprintf('.*_%s\\.(tif|grd)$', x), pp$unpackedBands, value = TRUE)})),
                                  # sapply above is to keep the same order as supplied in the VEGETATION_INDICES variable
                                  fun = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['fun']],
                                  datatype = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['datatype']],
                                  filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
            pp$processingMeta[[vi]] <- processingMeta
        } else { # We need to extract the SR bands from the archive
            pp$toExtract <- union(pp$toExtract, grep(pattern = bands_pattern, x = pp$archivedBands, value = TRUE))
            processingMeta <- list(vi = vi,
                                  needProcessing = TRUE,
                                  sr_files = file.path(pp$srdir, unname(sapply(sr_bands, function(x){grep(pattern = sprintf('.*_%s\\.(tif|grd)$', x), pp$archivedBands, value = TRUE)}))),
                                  fun = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['fun']],
                                  datatype = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['datatype']],
                                  filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
            pp$processingMeta[[vi]] <- processingMeta
        }
    }
    return(pp)
}

#' Creates a list containing organizing all information required for the following pre-processing steps
#' 
#' @param x character. Path to a tar.gz archive or to a directory containing landsat surface reflectance bands
#' @param outdir character. Directory where the pre-processed data will be written
#' @param vi character or list of characters. The vegetation indices to be processed.
#' @param srdir character. Optional directory where the intermediary surface reflectance and espa processed vegetation indices will be stored after unpacking. If NULL (default), a temporary directory will be used. srdir can also be a directory that already contains some (but possibly not all) of surface reflectance bands contained in the archive.
#' @param delete Logical. Should surface reflectance files be deleted after vegetation index calculated? (usefull for disk space management; surface reflectance files are very voluminous and a user may want to keep the Landsat archive in compressed format only)
#' @param mask Character or NULL. The name of the mask to be applied to the bands (e.g.: \code{mask = 'cfmask'})
#' @param keep Numeric. Can take multiple values. Which values of the mask layer should be kept?
#' @param e Extent object or object that can be coerced as extent.
#' @param fileExt Character. Extension of the file to be generated. Note that \code{filename} is automatically generated
#' @param overwrite overwrite existing files?
#' 
#' @return A list with parameters for further pre-processing
#' 
.ppInit <- function(x, outdir, vi='ndvi', srdir=NULL, delete=FALSE, mask=NULL, keep=c(0), e=NULL, fileExt='grd', overwrite=FALSE){
    pp <- list(x=x,
               sensor_letter=getSceneinfo(x)$sensor_letter,
               sceneID=row.names(getSceneinfo(x)), # TODO: This is not ideal since archive name has old conventions
               archivedBands=NULL,
               unpackedBands=NULL,
               srdir=NULL,
               outdir=outdir,
               vis=vi,
               toExtract=NULL,
               processingMeta=list(),
               fileExt=fileExt,
               mask=mask,
               keep=keep,
               e=e,
               delete=delete,
               overwrite=overwrite)
    if(is.null(srdir)) {
        pp$srdir <- file.path(tmpDir(), row.names(getSceneinfo(x)))
    }
    # Inventory bands/indices that are present in srdir and in the tar archive
    if(file.info(x)$isdir) {
        pp$unpackedBands <- list.files(x, full.names = TRUE, pattern = '.*\\.(tif|grd)$') # adding sr_ to pattern exclude potential cloud mask...
        pp$srdir <- x
        if(length(pp$unpackedBands) == 0) {
            stop('x directory does not contain any landsat bands that conform with the standard surface reflectance pattern')
        }
    } else if(raster::extension(x) == '.gz') {
        tarList <- untar(x, list = TRUE)
        pp$archivedBands <- grep(pattern = '.*\\.tif', x = tarList, value = TRUE)
        # It's also possible to provide a srdir that already contains some bands in addition to the archive
        if(!is.null(srdir)) {
            pp$unpackedBands <- list.files(srdir, full.names = TRUE, pattern = '.*\\.(tif|grd)$')
        }
    } else {
        stop('x neither points to a tar.gz archive nor a directory with Landsat bands')
    }
    # For every vi, check if it needs to be processed/extracted
    for (vi in pp$vis){
        pp <- .viAction(vi, pp)
    }
    # Check whether mask is already in the srdir, if not add it to the $toExtract slot
    if(!is.null(mask)){
        if(any(grepl(pattern = sprintf('.*_%s\\.(tif|grd)$', mask), pp$unpackedBands))){ # any() is because grepl(x=NULL) returns logical(0)
            pp$mask <- grep(pattern = sprintf('.*_%s\\.(tif|grd)$', mask), x = pp$unpackedBands, value = TRUE)
        } else {
            # OF course if the mask is not present in the archive, this will result in a file not exist error later
            pp$toExtract <- c(pp$toExtract, grep(pattern = sprintf('.*_%s\\.tif$', mask), x = pp$archivedBands, value = TRUE))
            pp$mask <- file.path(pp$srdir, grep(pattern = sprintf('.*_%s\\.tif$', mask), x = pp$archivedBands, value = TRUE))
        }
    }
    return(pp)
}

.createDirs <- function(pp){
    dir.create(pp$srdir, showWarnings = FALSE)
    for (vi in pp$vis){
        dir.create(file.path(pp$outdir, vi), showWarnings = FALSE)
    }
}

.extract <- function(pp){
    # UNpack required bands from the archive
    if(length(pp$toExtract) > 0) {
        untar(pp$x, files=pp$toExtract, exdir=pp$srdir)
    }
}

#' @importFrom raster crop raster
.crop <- function(x, e){
    # Handles case where the extent is NULL
    # x is a character NOT a RasterLayer
    r <- raster(x)
    if(!is.null(e)){
        r <- crop(r, e)
    }
    return(r)
}

#' @importFrom raster extent overlay raster writeRaster dataType
.process <- function(pp, vi){
    # Function to be passed to overlay for data cleaning (masking)
    # Needs to be defined within .process for having access to pp$keep (function environment)
    .clean <- function(x, y) {
        x[!(y %in% pp$keep)] <- NA
        return(x)
    }
    # Also includes cropping and masking
    # Prepare extent
    e <- pp$e
    if(!is.null(e)) {
        if(class(e) != 'extent') {
            e <- extent(e)
        }
    }
    if(pp$processingMeta[[vi]]$needProcessing) {
        # Crop
        r_list <- sapply(X = pp$processingMeta[[vi]]$sr_files, FUN = .crop, e = pp$e)
        if(!is.null(pp$mask)){
            mask_sub <- .crop(pp$mask, pp$e)
        }
        # Compute
        overlayList <- c(unname(r_list), fun=pp$processingMeta[[vi]]$fun)
        r_out <- do.call(what=raster::overlay, args=overlayList)
        # Mask
        if(!is.null(pp$mask)){
            r_out <- overlay(x=r_out, y=mask_sub, fun=.clean)
        }
        # Write to disk
        writeRaster(r_out, filename = pp$processingMeta[[vi]]$filename, datatype = pp$processingMeta[[vi]]$datatype, overwrite=pp$overwrite)
    } else { # Does not need processing
        # Crop
        r_out <- .crop(pp$processingMeta[[vi]]$vi_file, pp$e)
        datatype <- dataType(r_out)
        # Mask
        if(!is.null(pp$mask)){
            mask_sub <- .crop(pp$mask, pp$e)
            r_out <- overlay(x=r_out, y=mask_sub, fun=.clean)
        } else {
            
        }
        # Write to disk
        writeRaster(r_out, filename = pp$processingMeta[[vi]]$filename, datatype = datatype, overwrite=pp$overwrite)
    }
}


.delete <- function(pp){
    if(pp$delete) {
        to_delete <- file.path(pp$srdir, pp$toExtract)
        file.remove(to_delete)
    }
}

#' @title Wrapper functions to process Landsat data
#' 
#' @description Processes Landsat data (a single scene for \code{processLandsat} and multiple scenes for \code{processLandsatBatch}), from tarball or directory containing surface reflectance bands to vegetation index. The data must be surface reflectance obtained from espa (\url{https://espa.cr.usgs.gov/}). These data may already contained pre-processed indices layers, in which case they are directly used. The batcher (\code{processLandsatBatch}) allows to process multiple scenes with one command; sequentially or in parallel (parallel processing only work on unix systems (linux and mac)).
#' @param x Character. filename of the tarball or directory containing geotiff files. Or simply a directory containing the archives in the case of the batcher (\code{processLandsatBatch}).
#' @param outdir Character. Parent directory where the vegetation index rasterLayer should be written. Each vegetation index (or band) processed will be written to a sub directory of outdir.
#' @param vi Character or vector of characters. Vegetation index to be computed or extracted from the archive. The supported indices at the moment are 'ndvi', 'evi', 'savi', 'ndmi', 'ndwi', 'mndwi', 'tcb', 'tcg', 'tcw', 'nbr', 'nbr2'* or 'msavi'*. Indices with * need to be present in the archive. Note that the \code{vi=} argument can also be used to directly extract surface reflectance bands. \code{vi='sr_band1'} for instance will extract surface reflectance band 1 from the archive and perform the same pre-processing steps as if it was a vegetation index layer.
#' @param srdir Character. Directory where the tarball should be uncompressed. If set to NULL (default), a temporary location (see \code{raster::tmpDir()}) is used. When the \code{x} argument points to a directory, the same value is automatically assigned to both arguments. 
#' @param delete Logical. Should surface reflectance files that have been unpacked (in \code{srdir}) for the processing be deleted at the end of the process? (usefull for disk space management; surface reflectance files are very voluminous and a user may want to keep the Landsat archive in compressed format only)
#' @param mask Character or NULL. The name of the mask to be applied to the bands (e.g.: \code{mask = 'fmask'})
#' @param keep Numeric. Can take multiple values. Which values of the mask layer should be kept?
#' @param e Extent object or object that can be coerced as extent.
#' @param fileExt Character. Extension of the file to be generated. Note that \code{filename} is automatically generated
#' @param overwrite Logical. Overwrite exiting files if they already exist.
#' @param pattern character. Applies only to \code{landsatProcessBatch} when \code{x} is of length 1. Allows to 'filter' the input files or directories. See \link{list.files} for more details.
#' @param mc.cores Numeric. \code{landsatProcessBatch} only. For multicore implementation only. See \link{mclapply}
#' @author Loic Dutrieux
#' @return The function is used for its side effect of processing raster files written to subdirectories of \code{outdir} and does not return anything.
#' 
#' 
#' @examples
#' ### processLandsat Example
#' # Get list of test data files
#' library(raster)
#' 
#' dir <- system.file('external', package='bfastSpatial')
#' list <- list.files(dir, full.names=TRUE)
#' 
#' # Set the location of output directory (in tmpdir for the example)
#' dirout <- file.path(rasterOptions()$tmpdir, 'bfastSpatial')
#' dir.create(dirout, showWarning=FALSE)
#' # Generate (or extract, depending on whether the layers are already in the archive or not) NDVI and tcg for the first archive file
#' processLandsat(x=list[1], vi=c('ndvi', 'tcg'), outdir=dirout, delete=TRUE, mask='cfmask', keep=0, overwrite=TRUE)
#' 
#' # Visualize one of the layers produced
#' list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
#' plot(r <- raster(list[1]))
#' 
#' ### processLandsatBatch Example
#' # Get the directory where the Landsat archives are stored
#' dir <- system.file('external', package='bfastSpatial')
#' 
#' # Set the location of output directory (we'll use the temporary directory from the raster package)
#' dirout <- file.path(rasterOptions()$tmpdir, 'bfmspatial')
#' dir.create(dirout, showWarning=FALSE)
#' processLandsatBatch(x=dir, pattern=glob2rx('*.gz'), outdir=dirout, delete=TRUE, vi=c('ndvi', 'sr_band4'), mask='cfmask', keep=0, overwrite=TRUE)
#' 
#' # Visualize one of the layers produced
#' list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
#' plot(r <- raster(list[1]))
#' @export
#' 
# TODO: implement L= argument for savi to work
processLandsat <- function(x, outdir, vi='ndvi', srdir=NULL, delete=FALSE, mask=NULL, keep=c(0), e=NULL, fileExt='grd', overwrite=FALSE) {
    pp <- .ppInit(x=x, outdir=outdir, vi=vi, srdir=srdir, delete=delete, mask=mask, keep=keep, e=e, fileExt=fileExt, overwrite=overwrite)
    .createDirs(pp)
    .extract(pp)
    for (vi in pp$vis) {
        .process(pp, vi)
    }
    .delete(pp)
}


#' @importFrom parallel mclapply
#' @export
#' @rdname processLandsat
processLandsatBatch <- function(x, pattern=NULL, outdir, mc.cores=1, ...) {
    if (!is.character(x)) {
        stop('x needs to be of class character')
    }
    if(length(x) == 1) {
        x <- list.files(path=x, pattern=pattern, full.names=TRUE)
    }
    mclapply(X=x, FUN=processLandsat, outdir=outdir, mc.cores=mc.cores, ...)
    
}
