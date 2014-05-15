#' @title Calculate size of pixel clumps in a RasterLayer
#' 
#' @description Calculate the size of pixel clumps in a RasterLayer. The clump area is assigned to each clump, and araster with identical dimensions, but values representing the clump size is output. The definition of a pixel clump follows that of \code{\link{clump}}, where diagonals are either included or ignored in defining clumps. An optional conversion factor can be supplied based on pixel size.
#' 
#' @param x RasterLayer.
#' @param f Numeric. Optional: conversion factor for number of pixels (e.g. f=900/10000 for converting Landsat pixels to area in hectares).
#' @param stats Logical. Include summary statistics per clump? If TRUE, a list with (1) a raster layer showing clump sizes and (2) a summary table per clump. If FALSE, only a raster layer is returned.
#' @param ... Additional arguments to pass to \code{\link{clump}}
#' 
#' @return If \code{stats=FALSE}, a raster layer with pixel values indicating the size of the clumps to which those pixels belong. If \code{stats=TRUE}, a list with 2 objects: (1) a raster layer as above; and (2) a summary table of the pixel clump sizes.
#' 
#' @details Note that if \code{stats=TRUE}, a summary table is also produced. This table is not the same table that would result if the resulting raster was passed to \code{summary}. The summary table in this case is based on pixel clumps, rather than individual pixel values. The conversion factor \code{f} is also factored into the summary table.
#' 
#' @import raster
#' @import igraph
#' 
#' @author Ben DeVries
#' @export

clumpSize <- function(x, f=1, stats=FALSE, ...){
    # groups raster cells into clumps using clump()
    # assigns area (as # of pixels) to each clump
    # args:
    # x - raster layer
    # f - conversion factor for # of pixels (e.g. 900/10000 for landsat pixels --> hectares)
    # ... - arguments passed to clump()
    # TODO: write in support for multilayered rasters (see areaSieve)
    
    # identify clumps
    y <- clump(x, ...)
    
    # make a reclass matrix from the frequency table of y
    rcl <- freq(y)
    
    # conversion to other unit if factor is supplied
    if(f != 1)
        rcl[, 2] <- rcl[, 2] * f
    
    # optional: make a summary matrix
    if(stats){
      sumstat <- matrix(nc=1, nr=6, dimnames=list(c("Mean", "Min.", "1st Qu.", "Median", "3rd Qu.", "Max."), c("clump size")))
      sizes <- rcl[!is.na(rcl[, 1]), 2]
      sumstat[1, 1] <- mean(sizes)
      sumstat[c(2:6), 1] <- quantile(sizes)
      
    }
    
    # reclassify y based on rcl
    rcl <- cbind(rcl[,1], rcl) # double first column (see ?reclassify)
    z <- reclassify(y, rcl=rcl, right=NA)
    
    # remove NAs
    z[is.na(y)] <- NA
    
    # make z into a list if stats=TRUE
    if(stats)
        z <- list(clumps = z, stats = sumstat)
    
    return(z)
}
