#' Internal function to prepare the data extraction and processing needs, designed to be used within .ppInit
#' 
#' @param vi Character. Name of a vegetation index (usually passed to processLandsat as the vi argument)
#' @param pp list. The list containing the preprocessing parameters
#' 
#' @return The list passed as pp argument with toExtract and processingMeta slots filled
.viAction <- function(vi, pp) {
    # Build vi pattern
    viPattern = sprintf('.*sr_(%s)\\.(tif|grd)$', vi)
    if(any(grepl(pattern = viPattern, x = pp$unpackedBands))) {
        # check if vi is directly present in unpackedBand
        processingMeta <- list(vi = vi,
                              needProcessing = FALSE,
                              vi_file = grep(pattern = viPattern, x = pp$unpackedBands, value = TRUE),
                              filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
        pp$processingMeta <- c(pp$processingMeta, processingMeta)
    } else if(any(grepl(pattern = viPattern, x = pp$archivedBands))) {
        # check if vi is directly present in archived bands
        archived_band <- grep(pattern = viPattern, x = pp$archivedBands, value = TRUE)
        pp$toExtract <- union(pp$toExtract, archived_band)
        processingMeta <- list(vi = vi,
                              needProcessing = FALSE,
                              vi_file = file.path(pp$srdir, archived_band),
                              filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
        pp$processingMeta <- c(pp$processingMeta, processingMeta)
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
            pp$processingMeta <- c(pp$processingMeta, processingMeta)
        } else { # We need to extract the SR bands from the archive
            pp$toExtract <- union(pp$toExtract, grep(pattern = bands_pattern, x = pp$archivedBands, value = TRUE))
            processingMeta <- list(vi = vi,
                                  needProcessing = TRUE,
                                  sr_files = file.path(pp$srdir, unname(sapply(sr_bands, function(x){grep(pattern = sprintf('.*_%s\\.(tif|grd)$', x), pp$archivedBands, value = TRUE)}))),
                                  fun = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['fun']],
                                  datatype = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['datatype']],
                                  filename = file.path(pp$outdir, vi, sprintf('%s_%s.%s', pp$sceneID, vi, pp$fileExt)))
            pp$processingMeta <- c(pp$processingMeta, processingMeta)
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
               processingMeta=NULL,
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

.extract <- function(pp){
    # UNpack required bands from the archive
    if(length(pp$toExtract) > 0) {
        untar(pp$x, files=pp$toExtract, exdir=pp$srdir)
    }
}

.process <- function(pp, vi){
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
        # Compute
        # Mask
        # Write to disk
    } else { # Does not need processing
        # Crop
        # Mask
        # Write to disk
    }
}


.delete <- function(pp){
    if(pp$delete) {
        to_delete <- file.path(pp$srdir, pp$toExtract)
        file.remove(to_delete)
    }
}

processLandsat <- function(x, outdir, vi='ndvi', srdir=NULL, delete=FALSE, mask=NULL, keep=c(0), e=NULL, fileExt='grd', overwrite=overwrite) {
    pp <- .ppInit(x=x, outdir=outdir, vi=vi, srdir=srdir, delete=delete, mask=mask, keep=keep, e=e, fileExt=fileExt)
    .extract(pp)
    for (vi in pp$vis) {
        .process(pp, vi)
    }
    .delete(pp)
}
