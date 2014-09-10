
#' @title Creates a time stack of Landsat layers
#' 
#' @description Creates a stack of Landsat layers, reordering them chronologically
#' 
#' 
#' 
#' @param x character. dir containing the files to be stacked or character list (the files). IN the former case, it is recommended to use the \code{pattern} argument
#' @param pattern See \link{list.files}
#' @param orderChrono Logical. Should layers in the output object be orderred chronologically. If set to FALSE, layer order will be alphabetical.
#' @param ... Arguments to be passed to \link{writeRaster}. If specifying a filename, it is strongly recommended to also set a datatype.
#' @author Loic Dutrieux
#' @examples
#' # 1 - Produce individual VI layers (using processLandsatBatch())
#' # Get the directory where the Landsat archives are stored
#' dir <- system.file('external', package='bfastSpatial')
#' 
#' # Set the location of output and intermediary directories (everything in tmpdir in that case)
#' srdir <- dirout <- file.path(rasterOptions()$tmpdir, 'bfmspatial')
#' dir.create(dirout, showWarning=FALSE)
#' processLandsatBatch(x=dir, pattern=glob2rx('*.zip'), outdir=dirout, srdir=srdir, delete=TRUE, vi='ndvi', mask='fmask', keep=0, overwrite=TRUE)
#' 
#' # Visualize one of the layers produced
#' list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
#' 
#' 
#' # Stack the layers
#' stackName <- file.path(dirout, 'stack/stackTest.grd')
#' dir.create(file.path(dirout, 'stack'))
#' s <- timeStack(x=dirout, pattern=glob2rx('*.grd'), filename=stackName, datatype='INT2S')
#' 
#' plot(s)
#'
#' 
#' 
#' 
#' 
#' @import stringr
#' @import raster
#' @export
#' 

timeStack <- function(x, pattern=NULL, orderChrono = TRUE, ...) {
    
    if(!is.character(x)){
        stop('x must be a character (directory) or a list of characters')
    }
    if (length(x) == 1){
        x <- list.files(x, pattern=pattern, full.names=TRUE)
    }
    
    orderChronoFun <- function(list) {
        list2 <- list[order(substr(str_extract(string=basename(list), '(LT4|LT5|LE7|LC8)\\d{13}'), 4, 16))] 
        return(list2)        
    }
    
    if(orderChrono){
        x <- orderChronoFun(x)
    }
    
    s <- stack(x)
    
    time <- getSceneinfo(str_extract(string=basename(x), '(LT4|LT5|LE7|LC8)\\d{13}'))$date
    s <- setZ(x=s, z=time)
    
    if(hasArg(filename)) {
        out <- writeRaster(s, ...)
        return(out)
    }
    return(s)
        
}
