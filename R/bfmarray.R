bfmarray<- function(x, dates=NULL,  pptype='irregular', start, monend=NULL,
                    formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL,
                    history = c("ROC", "BP", "all"), aggre="month",
                    type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05, mc.cores=1, returnLayers = c("breakpoint", "magnitude", "error"), sensor=NULL, ...) {
  
 
  coef_len <- 1 # intercept
  modterms <- attr(terms(formula), "term.labels")
  if("trend" %in% modterms)
    coef_len <- coef_len + 1
  if("harmon" %in% modterms)
    coef_len <- coef_len + (order * 2) # sin and cos terms
  
  fun <- function(x) {
    # subset x by sensor
    #  if(!is.null(sensor))
    #    x <- x[which(s$sensor %in% sensor)]
    
    # convert to bfast ts
    # ts <- bfastts(x, dates=dates, type=pptype)
    if (aggre  == "month")
    {  
      spt<-zoo(x,dates)
      monmean <- aggregate(spt, as.Date(as.yearmon(dates)), mean)
      
      frequency(monmean)<-12
      na.new <- function(x) ts(na.exclude(x), frequency = 12)
      
      stlmon<-stl(monmean, na.action = na.new, s.window = "per")
      
      datamon <- ts(rowSums(stlmon$time.series)) 
      tsp(datamon) <- tsp(stlmon$time.series)
      ts<-datamon
    }
    else
    {
      
      #spt<-ts( x,start=c(2000,7),end=c(2013,44),frequency=46)
      spt<-zoo(x,dates)
      frequency(spt)<-46
      na.new <- function(x) ts(na.exclude(x), frequency = 46)
      stlmon<-stl(spt, na.action = na.new, s.window = "per")
      spt <- ts(rowSums(stlmon$time.series)) 
      tsp(spt) <- tsp(stlmon$time.series)
      ts<-spt
    }
    
    #optional: apply window() if monend is supplied
    if(!is.null(monend))
      ts <- window(ts, end=monend)
    
    # run bfastmonitor(), or assign NA if only NA's (ie. if a mask has been applied)
    if(!all(is.na(ts))){
      bfm <- try(bfastmonitor(data=ts, start=start,
                              formula=formula,
                              order=order, lag=lag, slag=slag,
                              history=history,
                              type=type, h=h,
                              end=end, level=level), silent=TRUE)
      
      # assign 1 to error and NA to all other fields if an error is encountered
      if(class(bfm) == 'try-error') {
        bkpt <- NA
        magn <- NA
        err <- 1
        history <- NA
        rsq <- NA
        adj_rsq <- NA
        coefficients <- rep(NA, coef_len)
      } else {
        bkpt <- bfm$breakpoint
        magn <- bfm$magnitude
        err <- NA
        history <- bfm$history[2] - bfm$history[1]
        rsq <- summary(bfm$model)$r.squared
        adj_rsq <- summary(bfm$model)$adj.r.squared
        coefficients <- coef(bfm$model)
      }
    } else {
      bkpt <- NA
      magn <- NA
      err <- NA
      history <- NA
      rsq <- NA
      adj_rsq <- NA
      coefficients <- rep(NA, coef_len)
    }
    res <- c(bkpt, magn, err, history, rsq, adj_rsq)
    names(res) <- c("breakpoint", "magnitude", "error", "history", "r.squared", "adj.r.squared")
    res <- res[which(names(res) %in% returnLayers)]
    if("coefficients" %in% returnLayers)
      res <- c(res, coefficients)
    return(res)
  }
  
  out <- apply(x , c(1,2),  fun)
  
  return(out)
}




