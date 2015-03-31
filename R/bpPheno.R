#' Detect breakpoints in irregularly spaced time-series containing seasonal and trend components
#' 
#' @param x time-series object of class zoo
#' @param order Numeric Harmonic order of the regression model
#' @param formula See \link{breakpoints}
#' @param breaks See \link{breakpoints}
#' @param h See \link{breakpoints}
#' @param nbreaks Numeric, maximum theoretical number of breaks (for spatial implementation)
#' 
#' 
#' @import strucchange
#' @import zoo
#' @import bfast
#' 
#' @export
#' 


bpPheno <- function(x, order=1, formula = response ~ trend + harmon, breaks = NULL, h = 0.15, nbreaks = NULL, mc.cores = 1) {
    
    # def function bp2df
    bp2df <- function(bpOut, pp, nbreaks = NULL) {
        if (!is.null(nbreaks)) {
            mat <- matrix(nrow = 1, ncol = nbreaks + 1)
            df <- as.data.frame(mat)
            colnames(df) <- c('nbreaks', sprintf('break%d', c(1:nbreaks)))
            if(class(bpOut) == 'try-error') {
                nBreaksOut <- -1 # So that -1 breaks means that an error occured
            } else {
                if(!is.na(bpOut$breakpoints)) {
                    nBreaksOut <- length(bpOut$breakpoints)
                    df[1,2:(nBreaksOut + 1)] <- pp$time[bpOut$breakpoints]
                } else {
                    nBreaksOut <- 0
                }
                
            }
            df[,'nbreaks'] <- nBreaksOut
            return(df)
        } else {
            stop('specify a maximum number of breaks ((Nb of observation * h) -1)')
        }
        
    }
    
    # def function bp
    bp <- function(ts, order, formula, breaks, h) {
        pp <- bfastpp(ts, order = order)
        bpOut <- try(breakpoints(formula = formula, data = pp, breaks = breaks, h = h))
        bp2df(bpOut, pp, nbreaks = nbreaks) # Returned variable
    }
    
    # def function wrapping the two above functions
    
    ts <- bfastts(data = x, dates = index(x), type = 'irregular')
    out <- mclapply(X = ts, FUN = bp, mc.cores = mc.cores, order = order, formula = formula, breaks = breaks, h = h)
    # Convert list of df to df
    do.call(rbind, out)
}

