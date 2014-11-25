bfmSpOver <- function(x, y, mc.cores = 1) {
    
    ts <- zooExtract(x, y)
    bfm <- bfmZoo(ts, mc.cores = mc.cores)
    
    if(inherits(y, 'SpatialPoints')) {
        y <- SpatialPoints(y) # Not sure if that is necessary
        out <- SpatialPointsDataFrame()
    } else if(inherits(y, 'SpatialPolygons')) {
        
    }
    
}