#' @title Calculate size of pixel clumps in a RasterLayer
#' 
#' @description Calculate the size of pixel clumps in a RasterLayer. The clump area is assigned to each clump, and araster with identical dimensions, but values representing the clump size is output. The definition of a pixel clump follows that of \code{\link{clump}}, where diagonals are either included or ignored in defining clumps. An optional conversion factor can be supplied based on pixel size.
#' 
#' @param x RasterLayer.
#' @param f Numeric. Optional: conversion factor for number of pixels (e.g. f=900/10000 for converting Landsat pixels to area in hectares).
#' @param ... Additional arguments to pass to \code{\link{clump}}
#' 
#' @import raster
#' @import igraph
#' @export

clumpSize <- function(x, f=1, ...){
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
  rcl <- cbind(rcl[,1], rcl)
  
  # reclassify y based on rcl
  z <- reclassify(y, rcl=rcl, right=NA)
  
  # remove NAs
  z[is.na(y)] <- NA
  
  # conversion to other unit if factor is supplied
  if(f != 1)
    z <- z * f
  
  return(z)
}
