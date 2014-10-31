#' Wrapper to pre-process MODIS data
#' 
#' @description Processes MODIS data for subsequentent use in time-series analysis
#' 
#' @param x Character. Directory where the data is located. Or list of file names.
#' @param pattern Only useful if x if of length 1. See \link{list.files} for more details
#' @param outdir Character. Directory where the output should be written.


processMODIS <- function(x, pattern = NULL, data_SDS, QC_SDS, bit=FALSE, QC_val, fill=NULL, outdir, mc.cores=1) {
    if (!is.character(x)) {
        stop('x needs to be of class character')
    }
    
    if(length(x) == 1) {
        x <- list.files(path=x, pattern=pattern, full.names=TRUE)
    }
    
    dates <- getMODISinfo(x)
    datesU <- unique(dates)
    
    fun <- function(date) { # Function to perform processing steps (to be applied over datesU object)
        ind <- which(dates == datesU)
        tiles <- x[ind]
        tilesClean <- sapply(X=tiles, FUN=cleanMODIS, data_SDS = data_SDS, QC_SDS = QC_SDS, bit=bit, QC_val = QC_val, fill=fill)
        
        # Generate filename
        outname <- sprintf('%stif', str_match(tiles[1], pattern='^.*\\.A\\d{7}\\.')[1])
        outname <- file.path(outdir, outname)
        tilesClean$filename <- outname
        
        # Need to find a name to handle data type as well
        out <- do.call(what=mosaic, args=tilesClean)
    }
    
    mclapply(X=datesU, FUN=fun, mc.cores=mc.cores)
    
    # When to do the stacking? Here or in another function?
    
}