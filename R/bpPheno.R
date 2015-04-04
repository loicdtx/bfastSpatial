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
            mat <- matrix(nrow = 1, ncol = 1 + nbreaks + (nbreaks + 1) * 5)
            df <- as.data.frame(mat)
            colnames(df) <- c('nbreaks', sprintf('break%d', c(1:nbreaks)), unlist(lapply(c(0:nbreaks), function(x) sprintf(c('meanSeg%d', 'slopeSeg%d', 'yBeginSeg%d', 'yEndSeg%d', 'durationSeg%d'), x))))
            if(class(bpOut) == 'try-error') {
                nBreaksOut <- -1 # So that -1 breaks means that an error occured
            } else {
                if(!is.na(bpOut$breakpoints)) {
                    # Input number of breaks to output df
                    nBreaksOut <- length(bpOut$breakpoints)
                    # Input breaks timing into output df
                    df[1,2:(nBreaksOut + 1)] <- pp$time[bpOut$breakpoints]
                    # Input Segments properties
                    segments <- c(pp$time[c(1,bpOut$breakpoints, nrow(pp))])
                    for (i in 1:(length(segments) - 1)) {
                        # To deal with begining of time-series boundary
                        if (i == 1) {
                            subDf <- subset(pp, time <= segments[i + 1])
                        } else {
                            subDf <- subset(pp, time <= segments[i + 1] & time > segments[i]) 
                        }
                        m <- mean(subDf$response, na.rm = TRUE)
                        model <- lm(response ~ trend, data = subDf)
                        sl <- model$coefficients[2]
                        n <- nrow(subDf)
                        yBegin <- predict(model, newdata = subDf[1,]) # Beginning of segment (!= to intersection with break)
                        yEnd <- predict(model, newdata = subDf[n,]) # End of segment (== intersection with breakLine)
                        duration <- diff(range(subDf$time))
                        segNum <- i - 1
                        colNames0 <- c('meanSeg', 'slopeSeg', 'yBeginSeg', 'yEndSeg', 'durationSeg')
                        colNames <- sprintf('%s%d', colNames0, segNum)
                        # Fill dataframe 
                        df[1, colNames] <- c(m, sl, yBegin, yEnd, duration)
                    }
                    
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

