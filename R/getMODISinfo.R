#' Get MODIS infos
#' 
#' @description Parses through MODIS file names and retrieve information on acquisition date. Future versions will include retrieval of sensor information, tile ID, etc... The function is vectorized.
#' 
#' @param x Character. MODIS file name
#' 
#' @return
#' A dataframe with one column ($date) of class \code{Date}
#' 
#' @author Loic Dutrieux
#' 
#' @examples
#' # Simple case
#' fileName <- '/path/to/file/MOD17A2.A2008097.h11v09.005.2008142064019.hdf'
#' getMODISinfo(fileName)
#' 
#' # Multile files
#' fileNames <- c('/path/to/file/MOD17A2.A2008097.h11v09.005.2008142064019.hdf', 'MOD17A2.A2008097.h11v09.005.2008164064019.tif')
#' getMODISinfo(fileNames)
#' 
#' # Get only a vector of dates
#' getMODISinfo(fileNames)$date
#' 
#' 
#' @import stringr
#' @export
#' 


getMODISinfo <- function(x) {
    date <- as.Date(str_match(pattern='(\\.A)(\\d{7})(\\.)', basename(x))[,3], format="%Y%j")
    data.frame(date = date) # For consistency with getSceneinfo (Landsat)    
}