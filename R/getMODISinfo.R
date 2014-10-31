#' Get MODIS infos
#' 
#' @param x Character. MODIS file name
#' 
#' @description Initial version of function to parse through MODIS file names and retrieve information on acquisition date, sensor, tile number, etc... This initial version only includes date retrieval.
#' 
#' @author Loic Dutrieux
#' 
#' @import stringr
#' @export
#' 


getMODISinfo <- function(x) {
    as.Date(str_match(pattern='(\\.A)(\\d{7})(\\.)', basename(x))[,3], format="%Y%j")
}