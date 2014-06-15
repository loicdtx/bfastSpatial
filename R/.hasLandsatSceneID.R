#' @title Check if a String is a Landsat Scene ID
#' 
#' @description Parses a vector of characters to check if they conform to Landsat naming convention
#'  
#' @param x Character vector or raster* object to be tested.
#' 
#' @return \code{TRUE} if all elements of \code{x} match Landsat scene ID criteria and \code{FALSE} if not.
#' 
#' @author Ben DeVries and Loic Dutrieux
#'
#' @examples
#' .hasLandsatSceneID(c('LT52300702009234XXX', 'LT72300702009234XXX'))
#' .hasLandsatSceneID(c('ndvi.LT52300702009234XXX.tif'))
#'
#' data(tura)
#' .hasLandsatSceneID(tura)
#' # is equivalent to:
#' .hasLandsatSceneID(names(tura))
#'
#' # simulate a raster Brick missing layer names
#' b <- tura
#' names(b) <- NULL
#' .hasLandsatSceneID(b)

.hasLandsatSceneID <- function(x){
    # convert x to character vector if a raster object
    if(class(x) %in% c('RasterLayer', 'RasterBrick', 'RasterStack'))
        x <- names(x)
    
    # logical vector to test Landsat conditions
    test <- vector('logical', 2)
    
    # condtion 1: sensor ID is included (LT4/5, etc.) and there are at least 13 more characters following
    test[1] <- all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=x))
    
    # condition 2: following 13 characters are numbers (path + row + year + jday)
    pryj <- substr(x, regexpr(pattern='(LT4|LT5|LE7|LC8)', x) + 3, regexpr(pattern='(LT4|LT5|LE7|LC8)', x) + 15)
    pryj <- suppressWarnings(as.numeric(pryj))
    test[2] <- all(!is.na(pryj)) # because NA's are introduced if a non-numbers are coerced to a numeric
    
    # condense test to a logical vector (ie. all conditions must be TRUE)
    test <- all(test)
    
    return(test)
}