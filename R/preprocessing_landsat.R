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
                              vi_file = grep(pattern = viPattern, x = pp$unpackedBands, value = TRUE))
        pp$processingMeta <- c(pp$processingMeta, processingMeta)
    } else if(any(grepl(pattern = viPattern, x = pp$archivedBands))) {
        # check if vi is directly present in archived bands
        archived_band <- grep(pattern = viPattern, x = pp$archivedBands, value = TRUE)
        pp$toExtract <- union(pp$toExtract, archived_band)
        processingMeta <- list(vi = vi,
                              needProcessing = FALSE,
                              vi_file = file.path(pp$srdir, archived_band))
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
                                  datatype = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['datatype']])
            pp$processingMeta <- c(pp$processingMeta, processingMeta)
        } else { # We need to extract the SR bands from the archive
            pp$toExtract <- union(pp$toExtract, grep(pattern = bands_pattern, x = pp$archivedBands, value = TRUE))
            processingMeta <- list(vi = vi,
                                  needProcessing = TRUE,
                                  sr_files = file.path(pp$srdir, unname(sapply(sr_bands, function(x){grep(pattern = sprintf('.*_%s\\.(tif|grd)$', x), pp$archivedBands, value = TRUE)}))),
                                  fun = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['fun']],
                                  datatype = VEGETATION_INDICES[[pp$sensor_letter]][[vi]][['datatype']])
            pp$processingMeta <- c(pp$processingMeta, processingMeta)
        }
    }
    return(pp)
}


.ppInit <- function(x, outdir, vi='ndvi', srdir = NULL,  delete=FALSE, mask=NULL, keep=c(0), e=NULL, fileExt='grd'){
    pp <- list(x=x,
               sensor_letter=getSceneinfo(x)$sensor_letter,
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
               e=e)
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
    
}

.process <- function(pp){
    
}

.mask <- function(pp){
    
}

.crop <- function(pp){
    
}

.clean <- function(pp){
    
}

# x, vi='ndvi', srdir, outdir, untar=TRUE, delete=FALSE, mask=NULL, L=0.5, fileExt = 'grd', ...

archive <- '/home/ldutrieux/sandbox/test_data/landsat/LE070190462017013001T1-SC20170330202642.tar.gz'
.ppInit(archive, outdir='/media/loic')
.ppInit(archive, outdir='/media/loic', c('ndvi', 'tcg'), mask = 'cfmask')
.ppInit('/home/ldutrieux/sandbox/test_data/landsat/LC080190462017031101T1-SC20170330215812/', outdir = '/media/loic', c('ndvi', 'tcg'))
