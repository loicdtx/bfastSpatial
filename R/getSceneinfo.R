
#' @title Retrieve Landsat info from filenames
#' 
#' @description Parses through typical Landsat filenames and retrieves information on sensor and acquisition date. Vectorized over \code{sourcefile}.
#' 
#' @param sourcefile Character. Filename of a landsat layer or dataset.
#' @param ... Additional arguments to pass to \code{\link{write.csv}}.
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' @return a \code{data.frame} with parsed scene information from Landsat scene names
#' @import stringr
#' @examples 
#' getSceneinfo(c('ndvi.LC82300702014234.tar.gz', 'ndvi.LT52300702008234.tif'))
#' 
#' # Load tura
#' data(tura)
#' getSceneinfo(names(tura))
#' @export



getSceneinfo <- function(sourcefile, ...)
{
  # for the sake of convenience, sourcefile can be either a character vector of scene names (or subfolders) or the original .tar.gz or .tar files
  # this will check which it is and format accordingly
  if(!all(grepl(pattern='(LT4|LT5|LE7|LC8)\\d{13}', x=sourcefile)))
      warning('Some of the characters provided do not contain recognized Landsat5/7/8 scene ID')
    
    
  sourcefile <- str_extract(sourcefile, '(LT4|LT5|LE7|LC8)\\d{13}') 
  
  
  # dates in LS naming system are formatted with year and julian day as one number - "%Y%j" (e.g. 2001036 = 2001-02-05)
  # reformat date as "%Y-%m-%d" (ie. yyyy-mm-dd)
  dates <- as.Date(substr(sourcefile, 10, 16), format="%Y%j")
  
  # identify the sensor
  sensor <- as.character(mapply(sourcefile, dates, FUN=function(x,y){
    sen <- substr(x, 1, 3)
    if(is.na(sen)) NA 
    else if(sen == "LE7" & y <= "2003-03-31")
      "ETM+ SLC-on"
    else if(sen == "LE7" & y > "2003-03-31")
      "ETM+ SLC-off"
    else if(sen == "LT5" | x == "LT4") 
      "TM" 
    else if(sen == "LC8")
      "OLI"      
  }))
  # extract path, row
  path <- as.numeric(substr(sourcefile, 4, 6))
  row <- as.numeric(substr(sourcefile, 7, 9))
  
  # throw all attributes into a data.frame
  info <- data.frame(sensor = sensor, path = path, row = row, date = dates)
  sourcefile[is.na(sourcefile)] <- 'Not recognised'
  row.names(info) <- sourcefile
  
  # optional: print to .csv for future reference
  if(hasArg(file)) 
    write.csv(info, ...)
  
  return(info)
}