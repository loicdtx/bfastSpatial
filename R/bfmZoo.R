#' Runs bfastmonitor on a zoo object
#' 
#' @description This function is analog to the \link{bfastmonitor} function, but differs in terms of inputs. In \link{bfastmonitor} an object of class ts needs to be provided, usually pre-processed using \link{bfastts}; the present function accepts directly zoo time-series. The return also differs from \link{bfastmonitor}; instead of returning an object of class 'bfastmonitor'the present function returns a dataframe. The zoo object may contain several time-series, which results in a return dataframe containing several rows.
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