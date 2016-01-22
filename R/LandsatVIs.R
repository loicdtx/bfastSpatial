# Loic Dutrieux
# April 2014

# Each function needs to return a vector of indices (band numbers) and a function to be passed to overlay


# NDVI --------------------------------------------------------------------

.ndvi <- function(sensor) {
    
    if(sensor == 'OLI') {
        ind <- c('band4', 'band5')
    } else {
        ind <- c('band3', 'band4')
    }
    
    fun <- function(x1, x2) {
        ndvi <- 10000 * (x2 - x1)/(x2 + x1)
        return(ndvi)
    }
    return(list(ind=ind,
                fun=fun))
}


# EVI ---------------------------------------------------------------------

.evi <- function(sensor) {
    
    if(sensor == 'OLI') {
        ind <- c('band2', 'band4', 'band5')
    } else {
        ind <- c('band1', 'band3', 'band4')
    }
    
    fun <- function(x1, x3, x4){ 
        evi <- 10000 * 2.5 * (x4/10000 - x3/10000)/(x4/10000 + 6 * x3/10000 - 7.5 * x1/10000 + 1)
        return(evi)
    }
    return(list(ind=ind,
                fun=fun))
    
}

# SAVI --------------------------------------------------------------------
# Soil-adjusted Vegetation Index:
# SAVI = (1 + L)(B4 - B3)/(B4 + B3 + L)
# where L ~ [0,1] depending on climate, and is often assumed to be 0.5

.savi <- function(sensor, L=0.5) {
    
    if(sensor == 'OLI') {
        ind <- c('band4','band5')
    } else {
        ind <- c('band3', 'band4')
    }
    fun <- function(x1, x2){ 
        savi <- 10000 * (1 + L) * (x2/10000 - x1/10000) / (x2/10000 + x1/10000 + L)
        return(savi)
    }
    return(list(ind=ind,
                fun=fun))
}

# NBR ---------------------------------------------------------------------
# Normalized Burn Ratio:
# NBR = (B4 - B7) / (B4 + B7)

.nbr <- function(sensor) {
    
    if(sensor == 'OLI') {
        ind <- c('band5', 'band7')
    } else {
        ind <- c('band4', 'band7')
    }
    
    fun <- function(x1, x2) {
        nbr <- 10000 * (x1 - x2)/(x1 + x2)
        return(nbr)
    }
    return(list(ind=ind,
                fun=fun))
}

.ndmi <- function(sensor) {
    
    if(sensor == 'OLI') {
        ind <- c('band6', 'band7')
    } else {
        ind <- c('band5', 'band7')
    }
    
    fun <- function(x1, x2) {
        ndmi <- 10000 * (x1 - x2)/(x1 + x2)
        return(ndmi)
    }
    return(list(ind=ind,
                fun=fun))
}


.ndwi <- function(sensor) {
    
    if(sensor == 'OLI') {
        ind <- c('band3', 'band5')
    } else {
        ind <- c('band2', 'band4')
    }
    
    fun <- function(x1, x2) {
        ndwi <- 10000 * (x1 - x2)/(x1 + x2)
        return(ndwi)
    }
    return(list(ind=ind,
                fun=fun))
}

.mndwi <- function(sensor) {
    
    if(sensor == 'OLI') {
        ind <- c('band3', 'band6')
    } else {
        ind <- c('band2', 'band5')
    }
    
    fun <- function(x1, x2) {
        mndwi <- 10000 * (x1 - x2)/(x1 + x2)
        return(mndwi)
    }
    return(list(ind=ind,
                fun=fun))
}


# Tasseled Cap Indices ------------------------------------------------- 


.tasscap <- function(sensor, component) {
    
    if(sensor == 'OLI') {
        ind <- c('band2','band3','band4','band5','band6','band7') 
        tc_coef <- rbind(c(0, 0, 0, 0, 0, 0),
                         c(0, 0, 0, 0, 0, 0),
                         c(0, 0, 0, 0, 0, 0)) ## TODO: find these!
        
    } else if(grepl("ETM+", sensor)) {
        ind <- c('band1', 'band2', 'band3', 'band4', 'band5', 'band7')
        tc_coef <- rbind(c(0.3561, 0.3972, 0.3904, 0.6966, 0.2286, 0.1596),
                         c(-0.3344, -0.3544, -0.4556, 0.6966, -0.0242,-0.2630),
                         c(0.2626, 0.2141, 0.0926, 0.0656, -0.7629, -0.5388))
                         
    } else if(sensor == 'TM') {
        ind <- c('band1', 'band2', 'band3', 'band4', 'band5', 'band7')
        tc_coef <- rbind(c(0.2043, 0.4158, 0.5524, 0.5741, 0.3124, 0.2303),
                         c(-0.1603, -0.2819, -0.4934, 0.7940, 0.0002, -0.1446),
                         c(0.0315,  0.2021,  0.3102,  0.1594, -0.6806, -0.6109))
        
        # TODO: check coefs and get appropriate citations for documentation
    }
    
    # choose component
    if(component == 'brightness') {
        tcc <- tc_coef[1, ]
    } else if(component == 'greenness') {
        tcc <- tc_coef[2, ]
    } else if(component == 'wetness') {
        tcc <- tc_coef[3, ]
    }
    
    # TODO: allow coefficients to be passed by user as vectors for a given index for each sensor type
    
    fun <- function(x1, x2, x3, x4, x5, x6) {
        tcbright <- sum(c(x1, x2, x3, x4, x5, x6) * tcc) 
    } 
    
    return(list(ind=ind, 
                fun=fun)) 
}


