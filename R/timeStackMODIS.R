#' @title Creates a time stack of Landsat layers
#'
#' @description Creates a stack of MODIS layers, with time written to z dimension. Works in anolog way to \link{\code{timeStack}}
#'
#' @param x character. dir containing the files to be stacked or character list (the files). IN the former case, it is recommended to use the \code{pattern} argument
#' @param pattern See \link{list.files}
#' @param ... Arguments to be passed to \link{writeRaster}. If specifying a filename, it is strongly recommended to also set a datatype.
#' @author Loic Dutrieux
#' 
#' @import raster
#' @export
#'
timeStackMODIS <- function(x, pattern=NULL, ...) {
    if(!is.character(x)){
        stop('x must be a character (directory) or a list of characters')
    }
    if (length(x) == 1){
        x <- list.files(x, pattern=pattern, full.names=TRUE)
    }
    
    s <- stack(x)
    time <- getMODISinfo(x)$date
    s <- setZ(x=s, z=time)
    if(hasArg(filename)) {
        out <- writeRaster(s, ...)
        return(out)
    }
    return(s)
}