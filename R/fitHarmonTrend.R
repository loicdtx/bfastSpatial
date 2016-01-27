#' @title Harmonic-trend fit
#' @description Fit harmonic and/or trend model to a raster time series, or temporal subset thereof.
#' 
#' @param x raster times series
#' @param formula formula to fit to time series
#' @param order order of the harmonic trend (leave as 1 if there is no harmonic component)
#' @param start The start of the subset to be fit. See 'details' for special cases
#' @param end The end of the subset to be fit. See 'details' for special cases.
#' 
#' @details
#' If \code{start} and \code{end} are left as \code{NULL}, the model will be fit to the entire time series. A \code{numeric} can be supplied to either or both arguments to subset the time series as in \code{\link{window}}. 
#'  
#' @import bfast
#' @import raster
#' @export

fitHarmonTrend <- function(x, dates=NULL, formula = response~harmon+trend, order = 1, pptype='irregular', start = NULL, end = NULL, mc.cores=1, sensor=NULL, ...) {
    
    if(is.character(x)) {
        x <- brick(x)
    }
    
    if(is.null(dates)) {
        if(is.null(getZ(x))) {
            if(!.isLandsatSceneID(x)){ # Check if dates can be extracted from layernames
                stop('A date vector must be supplied, either via the date argument, the z dimension of x or comprised in names(x)')
            } else {
                dates <- as.Date(getSceneinfo(names(x))$date)
            }
        } else {
            dates <- getZ(x)
        }
    }
    
    
    # reformat sensor if needed
    if("ETM+" %in% sensor)
        sensor <- c(sensor, "ETM+ SLC-on", "ETM+ SLC-off")
    
    # get Landsat sceneID's if sensor given
    # ignore sensor if names(x) are not Landsat sceneID's
    if(!is.null(sensor)){
        if(!.isLandsatSceneID(x)){
            warning("Cannot subset by sensor if names(x) do not correspond to Landsat sceneID's. Ignoring...\n")
            sensor <- NULL
        } else {
            s <- getSceneinfo(names(x))
            s <- s[which(s$sensor %in% sensor), ]
        }
    }
    
    
    
    # determine length of coefficient vector
    # = intercept [+ trend] [+ harmoncos*order] [+ harmonsin*order]
    coef_len <- 1 # intercept
    modterms <- attr(terms(formula), "term.labels")
    if("trend" %in% modterms)
        coef_len <- coef_len + 1
    if("harmon" %in% modterms)
        coef_len <- coef_len + (order * 2) # sin and cos terms
    
    
    
    # pixel-wise fitting function
    fun <- function(y) {
        
        # subset y by sensor
        if(!is.null(sensor))
            y <- y[which(s$sensor %in% sensor)]
        
        # convert to bfast ts
        bts <- bfastts(y, dates=dates, type=pptype)
        
        
        ### subset bpp here if needed
        
        
        # fit model
        if(!all(is.na(bts))){
            
            bpp <- bfastpp(bts, order = order)
            m <- try(lm(formula = formula, data = bpp))
            
            # assign 1 to error and NA to all other fields if an error is encountered
            if(class(m) == 'try-error') {
                rsq <- NA
                adj_rsq <- NA
                coefficients <- rep(NA, coef_len)
                err <- 1
            } else {
                rsq <- summary(m)$r.squared
                adj_rsq <- summary(m)$adj.r.squared
                coefficients <- coef(m)
                names(coefficients) <- tolower(gsub('(\\(|\\))', '', names(coefficients)))
                err <- NA
            }
        } else {
            rsq <- NA
            adj_rsq <- NA
            coefficients <- rep(NA, coef_len)
            err <- NA
        }
        
        res <- c(rsq, adj_rsq, err)
        names(res) <- c("r.squared", "adj.r.squared", "error")
        res <- c(coefficients, res)
        
        return(res)
    }
    
    
    
    
    out <- mc.calc(x=x, fun=fun, mc.cores=mc.cores, ...)
    
    return(out)
}