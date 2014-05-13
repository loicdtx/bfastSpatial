#' @title Check if a String is a Landsat Scene ID
#' 
#' @description Parses a vector of characters to check if they conform to Landsat naming convention
#'  
#' @param x Character. Character vector to be tested.
#' 
#' @return A logical vector of length \code{length(x)}, with values of \code{TRUE} if respective elements of \code{x} match Landsat scene ID criteria
#' 
#' @author Ben DeVries
#' 
#' @export
#' 
#' @examples
#' # check if layer names of tura RasterBrick correspond to Landsat naming convention
#' data(tura)
#' all(.isLandsatSceneID(names(tura)))

.isLandsatSceneID <- function(x){
        # matrix with a row for each element of x, and column for each condition
    test <- matrix(nr = length(x), nc = 2)
    
    # condtion 1: sensor ID is included (LT4/5, etc.) and there are at least 13 more characters following
    test[, 1] <- grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=x)
    
    # condition 2: following 13 characters are numbers (path + row + year + jday)
    pryj <- substr(x, regexpr(pattern='(LT4|LT5|LE7|LC8)', x) + 3, regexpr(pattern='(LT4|LT5|LE7|LC8)', x) + 15)
    pryj <- suppressWarnings(as.numeric(pryj))
    test[, 2] <- !is.na(pryj) # because NA's are introduced if a non-numbers are coerced to a numeric
    
    # condense test to a logical vector (ie. all conditions - columns - must be TRUE)
    test <- apply(test, 1, all)
    
    return(test)
}