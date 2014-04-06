#' getSceneinfo
#' 
#' Retrieve Landsat info from filenames
#' 
#' @description Parses through typical Landsat filenames and retrieves information on sensor and acquisition date. Vectorized over \code{sourcefile}.
#' @param sourcefile Character. Filename of a landsat layer or dataset.
#' @param filename Character or NULL. For the output dataframe to be written to a csv file.
#' @author Ben DeVries
#' @return a dataframe



getSceneinfo <- function(sourcefile, filename=NULL)
  # returns a data.frame with sensor, path, row, and date information for each scene
  # writes to a .csv if a filename/path is specified
  
  # args:
    # sourcefile - a character vector of file (*.tar.gz or *.tar), folder names or scenenames (in standard Landsat format)
    # filename - optional: write results to .csv file
{
  # for the sake of convenience, sourcefile can be either a character vector of scene names (or subfolders) or the original .tar.gz or .tar files
  # this will check which it is and format accordingly
  sourcefile <- sapply(sourcefile, FUN=function(x){if(grepl(".gz", x)) x <- substr(x, 1, nchar(x)-3);
                                                   if(grepl(".tar", x)) x <- substr(x, 1, nchar(x)-4); 
                                                   return(x)})  
  
  # dates in LS naming system are formatted with year and julian day as one number - "%Y%j" (e.g. 2001036 = 2001-02-05)
  # reformat date as "%Y-%m-%d" (ie. yyyy-mm-dd)
  dates <- as.Date(substr(sourcefile, 10, 16), format="%Y%j")
  
  # identify the sensor
  sensor <- as.character(mapply(substr(sourcefile, 1, 3), dates, FUN=function(x,y){
    if(x == "LE7" & y <= "2003-03-31")
      "ETM+ SLC-on"
    else if(x == "LE7" & y > "2003-03-31")
      "ETM+ SLC-off"
    else if(x == "LT5" | x == "LT4") 
      "TM" 
    else if(x == "LC8")
      "OLI"
    else
      stop(sourcefile, " is not a recognized Landsat5/7/8 scene ID.")
  }))
  # extract path, row
  path <- as.numeric(substr(sourcefile, 4, 6))
  row <- as.numeric(substr(sourcefile, 7, 9))
  
  # throw all attributes into a data.frame
  info <- data.frame(sensor = sensor, path = path, row = row, date = dates)
  row.names(info) <- sourcefile
  
  # optional: print to .csv for future reference
  if(!is.null(filename)) 
    write.csv(info, file=filename, quote=FALSE)
  
  return(info)
}