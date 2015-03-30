#' Detect breakpoints in irregularly spaced time-series containing seasonal and trend components
#' 
#' @param x time-series object of class zoo
#' @param order Numeric Harmonic order of the regression model
#' @param formula See \link{breakpoints}
#' @param breaks See \link{breakpoints}
#' @param h See \link{breakpoints}
#' 
#' 
#' @import strucchange
#' @import zoo
#' @import bfast
#' 
#' @export
#' 


bpPheno <- function(x, order=1, formula = response ~ trend + harmon, breaks = NULL, h = 0.15) {
    ts <- bfastts(data = x, dates = index(x), type = 'irregular')
    pp <- bfastpp(ts, order = order)
    breaks <- breakpoints(formula = formula, data = pp, breaks = breaks, h = h)
    out <- list(df = pp,
                breaks = breaks)
    class(out) <- 'bpPheno'
    return(out)
    
    # TODO: Expand that function to export regression lines coefficients between segments
}