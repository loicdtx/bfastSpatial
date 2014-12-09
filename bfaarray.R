bfaarray2<- function(x, dates=NULL, monend=NULL,h = 0.15, season =   "harmonic" , 
                    max.iter = 3, breaks = NULL, hpc = "none", level = 0.05, type = "OLS-MOSUM",
                      aggre="month",  mc.cores=1, returnLayers = c("breakpoint", "magnitude", "error"), sensor=NULL, ...) 
  {
  #t1<-fevi9b9[1,1,]
   fun <- function(x) 
     {
 
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
 
    # run bfastmonitor(), or assign NA if only NA's (ie. if a mask has been applied)
 
 # x=fevi9b9[8,9,]
bfa <- try(bfast(ts, h = h, season = season, 
                 max.iter =max.iter ,
           breaks = breaks, hpc = hpc, level = level,type = type), silent=TRUE) 
     
   if(class(bfa) == 'try-error') 
     {
     bkpt <- NA
     magn <- NA
     #err <- 1
     
     } else if (bfa$nobp$Vt == FALSE) {
       bkpt <- bfa$output[[1]]$Vt.bp #   of 16 days e.g. year=16*50/365
       magn <- bfa$Mags[,3]       #magnitude of biggest change.. not so useful
       #tmagn<-bfa$Time          # time of biggest change.. not so useful
       #err <- NA 
      } else  {
     
        bkpt<-0
        magn<-0
              }
   
   if (is.null(breaks))
     
     breaks<-floor(1/h)
   
     LBP<-length(bkpt)   
  
   if (LBP < breaks) # just to avoid array with variables of different length
   {
       bkpt[(LBP+1):breaks]<-0
       magn[(LBP+1):breaks]<-0
   }
   res <- c(as.vector(bkpt),magn  )
   names(res) <- c(rep("breakpoint", length(bkpt)) , rep("mag",length(magn)))
   return(res)
  }
  
  out <- apply( x, c(1,2),  fun)
  
  return(out)
}

#only store time dimension
bfaarray<- function(x, dates=NULL, monend=NULL,h = 0.15, season =   "harmonic" , 
                     max.iter = 3, breaks = NULL, hpc = "none", level = 0.05, type = "OLS-MOSUM",
                     aggre="month",  mc.cores=1, returnLayers = c("breakpoint", "magnitude", "error"), sensor=NULL, ...) 
{
  #t1<-fevi9b9[1,1,]
  fun <- function(x) 
  {
    
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
    
    # run bfastmonitor(), or assign NA if only NA's (ie. if a mask has been applied)
    
    # x=fevi9b9[8,9,]
    bfa <- try(bfast(ts, h = h, season = season, 
                     max.iter =max.iter ,
                     breaks = breaks, hpc = hpc, level = level,type = type), silent=TRUE) 
    
    if(class(bfa) == 'try-error') 
    {
      bkpt <- NA
 
      
    } else if (bfa$nobp$Vt == FALSE) {
      bkpt <- bfa$output[[1]]$Vt.bp #   of 16 days e.g. year=16*50/365
 
    } else  {
      
      bkpt<-0
    
    }
    
 
    res <- c(as.vector(bkpt) )
    names(res) <-  rep("breakpoint", length(bkpt) ) 
    return(res)
  }
  
  out <- apply( x, c(1,2),  fun)
  
  return(out)
}


