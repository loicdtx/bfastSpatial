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
    p1 <- 'L(C|T|E)\\d{2}_[A-Z0-9]{4}_\\d{6}_\\d{8}_\\d{8}_\\d{2}_[A-Z0-9]{2}'
    p2 <- 'L(C|T|E)\\d{18}[A-Z0-9]{2}'
    p3 <- 'L(T|C|E)\\d{14}'
    pc <- paste(c(p1, p2, p3), collapse = '|')
    if(is.character(x)) {
        all(grepl(pattern=pc, x))
    } else if(inherits(x, 'RasterStackBrick')) {
        all(grepl(pattern=pc, x=names(x)))
    }
}