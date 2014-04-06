# Author: Loic Dutrieux
# January 2014

bfmSpatial <- function(x, dates, pptype='irregular', start,
                       formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL,
                       history = c("ROC", "BP", "all"),
                       type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, mc.cores=1, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }

    fun <- function(x) {
        ts <- bfastts(x, dates=dates, type=pptype)
        bfm <- try(bfastmonitor(data=ts, start=start,
                                formula=formula,
                                order=order, lag=lag, slag=slag,
                                history=history,
                                type=type, h=h,
                                end=end, level=level), silent=TRUE)
        if(class(bfm) == 'try-error') {
            return(cbind(NA, NA))
        } else {
            return(cbind(bfm$breakpoint, bfm$magnitude))
        }
    }
    
    out <- mc.calc(x=x, fun=fun, mc.cores=mc.cores, ...)
    return(out)
    
}