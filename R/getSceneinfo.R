
#' @title Retrieve Landsat info from filenames
#' 
#' @description Parses through typical Landsat filenames and retrieves information on sensor and acquisition date. Vectorized over \code{sourcefile}.
#' 
#' @param sourcefile Character. Filename of a landsat layer or dataset.
#' @param ... Additional arguments to pass to \code{\link{write.csv}}.
#' 
#' @author Ben DeVries and Loic Dutrieux
#' 
#' @return a \code{data.frame} with parsed scene information from Landsat scene names
#' 
#' @import stringr
#' 
#' @examples 
#' getSceneinfo(c('ndvi.LC82300702014234.tar.gz', 'ndvi.LT52300702008234.tif', '/home/username/LC08_L1TP_019046_20170311_20170317_01_T1_sr_band4.tif'))
#' 
#' # Load tura
#' data(tura)
#' getSceneinfo(names(tura))
#' 
#' @export


getSceneinfo <- function(sourcefile, ...)
{
  # for the sake of convenience, sourcefile can be either a character vector of scene names (or subfolders) or the original .tar.gz or .tar files
  # this will check which it is and format accordingly
    
    # Check that the vector of strings provided contains only valid landsat IDs
    # Note .isLandsatSceneID is vectorized
    if(!.isLandsatSceneID(sourcefile))
        warning('Some of the characters provided do not contain recognized Landsat5/7/8 scene ID')
    
    letterToSensor <- function(x, date) {
        if(x == "E" & date <= "2003-03-31")
            "ETM+ SLC-on"
        else if(x == "E" & date > "2003-03-31")
            "ETM+ SLC-off"
        else if(x == "T") 
            "TM" 
        else if(x == "C")
            "OLI"      
    }
    
    # FUnction to parse individual file names (returns a single df row, vetorize later with lapply and combine with do.call)
    parser <- function(x) {
        # Define patterns
        p1 <- 'L(C|T|E)(\\d{2})_([A-Z0-9]{4})_(\\d{3})(\\d{3})_(\\d{8})_(\\d{8})_(\\d{2})_([A-Z0-9]{2})'
        p2 <- 'L(C|T|E)(\\d{2})(\\d{3})(\\d{3})(\\d{8})(\\d{2})([A-Z0-9]{2})'
        p3 <- 'L(T|C|E)(\\d{1})(\\d{3})(\\d{3})(\\d{7})'
        if (grepl(p1, x = x)) {
            groups <- str_match(x, p1)
            date <- as.Date(groups[,7], '%Y%m%d')
            sensor_letter <- groups[,2]
            df <- data.frame(row.names = groups[,1],
                             sensor = letterToSensor(sensor_letter, date),
                             path = as.numeric(groups[,5]),
                             row = as.numeric(groups[,6]),
                             date = date,
                             tier = groups[,10],
                             stringsAsFactors = FALSE)
        } else if (grepl(p2, x = x)){
            groups <- str_match(x, p2)
            date <- as.Date(groups[,6], '%Y%m%d')
            sensor_letter <- groups[,2]
            df <- data.frame(row.names = groups[,1],
                             sensor = letterToSensor(sensor_letter, date),
                             path = as.numeric(groups[,4]),
                             row = as.numeric(groups[,5]),
                             date = date,
                             tier = groups[,8],
                             stringsAsFactors = FALSE)
        } else if (grepl(p3, x = x)){
            groups <- str_match(x, p3)
            date <- as.Date(groups[,6], '%Y%j')
            sensor_letter <- groups[,2]
            df <- data.frame(row.names = groups[,1],
                             sensor = letterToSensor(sensor_letter, date),
                             path = as.numeric(groups[,4]),
                             row = as.numeric(groups[,5]),
                             date = date,
                             tier = NA,
                             stringsAsFactors = FALSE)
        } else {
            df <- data.frame(row.names = x,
                             sensor = NA,
                             path = NA,
                             row = NA,
                             date = NA,
                             tier = NA)
        }
        return(df)
    }
    
    out <- do.call(rbind, lapply(sourcefile, parser))
    if(hasArg(file)) 
        write.csv(out, ...)
    return(out)
}