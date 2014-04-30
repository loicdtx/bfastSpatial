#' @title Summarize a RasterBrick
#' 
#' @description Computes pixel-based summary statistics for a multi-layered raster object
#' 
#' @param x RasterBrick or RasterStack to be summarized
#' @param fun Function to apply to vectors extracted from each pixel
#' @param minDate Date. Minimum date to include in the calculation (see \link{\code{subsetRasterTS}})
#' @param maxDate Date. Maximum date to include in the calculation (see \link{\code{subsetRasterTS}})
#' @param sensor Character. Optional: limit calculation to selected (Landsat) sensors. Defaults to "all" for all data.
#' @param ... Additional arguments to be passed to \link{\code{mc.calc}}
#' 
#' @return A Raster layer representing the summary statistic of each pixel in the input RasterBrick or RasterStack

summaryBrick <- function(x, fun, minDate=NULL, maxDate=NULL, sensor="all", ...){
    
}