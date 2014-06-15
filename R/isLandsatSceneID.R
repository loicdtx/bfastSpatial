#' @title Check if a String is a Landsat Scene ID
#' 
#' @description Parses a vector of characters to check if they conform to Landsat naming convention
#'  
#' @param x Character or object of class RasterBrick or RasterStack
#' 
#' @return A logical vector of length \code{length(x)}, with values of \code{TRUE} if respective elements of \code{x} match Landsat scene ID criteria
#' 
#' @author Ben DeVries, Loic Dutrieux
#' 
#' 
#' @examples
#' # check if layer names of tura RasterBrick correspond to Landsat naming convention
#' data(tura)
#' .isLandsatSceneID(tura)

.isLandsatSceneID <- function(x){
        if(is.character(x)) {
            grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x)
        } else if(inherits(x, 'RasterStackBrick')) {
            all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=names(x)))
        }
}