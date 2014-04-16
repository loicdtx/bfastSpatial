# Loic Dutrieux
# April 2014

# Each function needs to return a vector of indices (band numbers) and a function to be passed to overlay


# NDVI --------------------------------------------------------------------

.ndvi <- function() {
    ind <- c(3,4)
    fun <- function(x1, x2) {
        ndvi <- 10000 * (x2 - x1)/(x2 + x1)
        return(ndvi)
    }
    return(list(ind=ind,
                fun=fun))
}


# EVI ---------------------------------------------------------------------

.evi <- function(){
    ind <- c(1,3,4)
    fun <- function(x1, x3, x4){ 
        evi <- 10000 * 2.5 * (x4 - x3)/(x4 + 6 * x3 - 7.5 * 1 + 1)
        return(evi)
    }
    return(list(ind=ind,
                fun=fun))
    
}