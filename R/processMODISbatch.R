#' Wrapper to pre-process MODIS data in batch mode
#' 
#' @description Processes MODIS data for subsequentent use in time-series analysis
#' 
#' @param x Character. Directory where the data is located. Or list of file names.
#' @param pattern Only useful if x if of length 1. See \link{list.files} for more details
#' @param outdir Character. Directory where the output should be written.
#' @param mosaic Logical. When working with several tiles, should these be mosaicked or kept as separate output files. Default is \code{TRUE}
#' 
#' @import raster
#' @import gdalUtils
#' @import parallel
#' 
#' @export
#' 


processMODISbatch <- function(x, pattern = NULL, data_SDS, QC_SDS, bit=FALSE, QC_val, fill=NULL, outdir, mosaic=TRUE, mc.cores=1) {
    if (!is.character(x)) {
        stop('x needs to be of class character')
    }
    
    if(length(x) == 1) {
        x <- list.files(path=x, pattern=pattern, full.names=TRUE)
    }
    
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    
    dates <- getMODISinfo(x)$date
    datesU <- unique(dates)
    
    fun <- function(date) { # Function to perform processing steps (to be applied over datesU object)
        ind <- which(dates == date)
        tiles <- x[ind]
        
        # Retrieve dataType for later writing to file
        type <- dataType(raster(get_subdatasets(tiles[1])[data_SDS]))
        
        if((length(ind) > 1) & mosaic) { # We do want to mosaic
            
            tilesClean <- lapply(X=tiles, FUN=cleanMODIS, data_SDS = data_SDS, QC_SDS = QC_SDS, bit=bit, QC_val = QC_val, fill=fill)
            # Generate filename
            outname <- sprintf('mosaic.%stif', str_match(basename(tiles[1]), pattern='^.*\\.A\\d{7}\\.')[1])
            outname <- file.path(outdir, outname)
            
            # Need to find a name to handle data type as well
            
            tilesClean$filename <- outname
            tilesClean$datatype <- type
            out <- do.call(what=raster::merge, args=tilesClean)
            
        } else { # No mosaicking 
            outnames <- file.path(outdir, basename(tiles))
            extension(outnames) <- '.tif'
            
            tilesClean <- mapply(FUN=cleanMODIS, x=tiles, filename = outnames, MoreArgs = list(data_SDS = data_SDS, QC_SDS = QC_SDS, bit=bit, QC_val = QC_val, fill=fill, datatype = type))
        }
        
    }
    
    mclapply(X=datesU, FUN=fun, mc.cores=mc.cores)

    
}