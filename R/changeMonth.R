#' @title Extract month of change from bfmChange RasterLayer
#' 
#' @description Returns one RasterLayer for each year represented by the change results. Months are represted by integers from 1 to 12.
#' 
#' @param change. RasterLayer output from \code{\link{bfmChange}} representing pixel breakpoints.
#' @param ... Additional arguments to pass to \code{\link{writeRaster}}
#' @return either a RasterLayer with values between 1 to 12 (representing month of change), or if multiple years are represented in the input change RasterLayer, a RasterBrick with one layer for each year, and values of 1 to 12 representing change months for each year.
#' @seealso \code{\link{bfmChange}}
#' @author Ben DeVries \email{devries.br@gmail.com}
#' @import raster
#' @export

changeMonth <- function(change, filename=NULL, ...){
  
  # vector of all non-NA values
  x <- change[!is.na(change)]
  
  # get the year
  yrs <- as.numeric(substr(as.character(x), 1, 4))
  
  # substract this value from x
  x <- x - yrs
  
  # convert x to date in yyyy-mm-dd format
  x <- x * 365 # julian days
  x <- paste(yrs, x, sep="") # date in yyyyjjj format
  x <- as.Date(x, format="%Y%j")
  x <- as.numeric(format(x, format="%m"))
  
  # check how many years are represented, and set the number of output rasters to this number
  year <- unique(yrs) # length of this vector will determine output
  output <- vector("list", length(year))
  
  # write new rasters one at a time with month values if the years match
  for(i in 1:length(output)){
    y <- z <- setValues(change, NA)
    y[!is.na(change)] <- x
    z[!is.na(change)] <- yrs
    y[z != year[i]] <- NA
    output[[i]] <- y
  }
  
  # if only 1 year is represented, just output that raster, otherwise create a rasterBrick and return it
  if(length(output) == 1){
    names(output[[1]]) <- paste("changeMonth", year, sep="")
    if(hasArg(filename))
        writeRaster(output[[1]], ...)
    
    return(output[[1]])
    
  } else {
    output <- do.call("brick", output)
    names(output) <- paste("changeMonth", year, sep="")
    if(hasArg(filename))
        writeRaster(output, ...)
    
    return(output)
  }  
}
