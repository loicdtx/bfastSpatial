# Loic Dutrieux
# April 2014

# Each function needs to return 

.ndvi <- function() {
    ind <- c(3,4)
    fun <- function(x1, x2) {
        ndvi <- 10000 * (x2 - x1)/(x2 + x1)
    }
    return(list(ind=ind,
                fun=fun))
}


### EVI
.evi <- function(){
    
    
    
}