#' Runs bfastmonitor on a zoo object
#' 
#' @description The zoo object may contain several time-series
#' 
#' @param x A zoo object that may contain several time-series
#' @param mc.cores Numeric For parallel processing. Number of workers.
#' @param ... Arguments to be passed to \link{bfastmonitor}
#' 
#' @return A dataframe
#' 
#' @author Loic Dutrieux
#' 
#' @import bfast
#' 
#' @export
#' 


bfmZoo <- function(x, mc.cores = 1, ...) {
    
    out <- mclapply(X = ts, FUN = bfastmonitor, mc.cores = mc.cores, ...)
    # Unlist
    data.frame(breakpoint = sapply(X = out, function(x) x$breakpoint),
               magnitude = sapply(out, function(x) x$magnitude))
}