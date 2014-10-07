#' @title Check if a String is a Landsat Scene ID
#' 
#' @description Parses a vector of characters to check if they conform to Landsat naming convention
#'  
#' @param x Character or object of class RasterBrick or RasterStack
#' 
#' @return \code{TRUE} if all elements of \code{x} match Landsat scene ID criteria, or \code{FALSE} otherwise.
#' 
#' @author Ben DeVries, Loic Dutrieux
#' 

.isLandsatSceneID <- function(x){
        if(is.character(x)) {
            all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x))
        } else if(inherits(x, 'RasterStackBrick')) {
            all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=names(x)))
        }
}