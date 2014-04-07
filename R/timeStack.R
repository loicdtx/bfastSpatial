#' timeStack
#' 
#' Creates a time stack of Landsat layers
#' 
#' @description Creates a stack of Landsat layers, reordering them chronologically
#' 
#' 
#' 
#' @param x character. dir containing the files to be stacked or character list (the files). IN the former case, it is recommended to use the \code{pattern} argument
#' @param pattern See \link{list.files}
#' @param ... Arguments to be passed to \link{writeRaster}. If specifying a filename, it is strongly recommended to also set a datatype.
#' @author Loic Dutrieux
#' @import stringr
#' @import raster
#' @export
#' 

timeStack <- function(x, pattern=NULL, ...) {
    
    if(!is.character(x)){
        stop('x must be a character (directory) or a list of characters')
    }
    if (length(x) == 1){
        x <- list.files(x, pattern=pattern, full.names=TRUE)
    }
    
    orderChrono <- function(list) {
        list2 <- list[order(substr(str_extract(string=basename(list), '(LT4|LT5|LE7)\\d{13}'), 4, 16))] 
        return(list2)        
    }
    
    x <- orderChrono(x)
    s <- stack(x)
    
    time <- getSceneinfo(str_extract(string=basename(x), '(LT4|LT5|LE7)\\d{13}'))$date
    s <- setZ(x=s, z=time)
    
    if(hasArg(filename)) {
        out <- writeRaster(s, ...)
        return(out)
    }
    return(s)
        
}
