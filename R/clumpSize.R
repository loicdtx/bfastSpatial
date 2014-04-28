#' @title Calculate size of pixel clumps in a RasterLayer
#' 
#' @description Calculate the size of pixel clumps in a RasterLayer. The clump area is assigned to each clump, and araster with identical dimensions, but values representing the clump size is output. The definition of a pixel clump follows that of \code{\link{clump}}, where diagonals are either included or ignored in defining clumps. An optional conversion factor can be supplied based on pixel size.
#' 
#' @param x RasterLayer.
#' @param f Numeric. Optional: conversion factor for number of pixels (e.g. f=900/10000 for converting Landsat pixels to area in hectares).
#' @param stats Logical. Include summary statistics per clump? If TRUE, a list with (1) a raster layer showing clump sizes and (2) a summary table per clump. If FALSE, only a raster layer is returned.
#' @param ... Additional arguments to pass to \code{\link{clump}}
#' 
#' @import raster
#' @import igraph
#' @author Ben DeVries (devries.br@@gmail.com)
#' @export

clumpSize <- function(x, f=1, stats=FALSE, ...){
    # groups raster cells into clumps using clump()
    # assigns area (as # of pixels) to each clump
    # args:
    # x - raster layer
    # f - conversion factor for # of pixels (e.g. 900/10000 for landsat pixels --> hectares)
    # ... - arguments passed to clump()
    
    # identify clumps
    y <- clump(x, ...)
    
    # make a reclass matrix from the frequency table of y
    rcl <- freq(y)
    
    # conversion to other unit if factor is supplied
    if(f != 1)
        rcl[, 2] <- rcl[, 2] * f
    
    # optional: make a summary matrix
    if(stats){
      sumstat <- matrix(nc=1, nr=7, dimnames=list(c("Mean", "Min.", "1st Qu.", "Median", "3rd Qu.", "Max.", "NA's"), c("clump size")))
      sumstat[1,1] <- mean()
    }
    
    # reclassify y based on rcl
    rcl <- cbind(rcl[,1], rcl) # double first column (see ?reclassify)
    z <- reclassify(y, rcl=rcl, right=NA)
    
    # remove NAs
    z[is.na(y)] <- NA
    
    return(z)
}
