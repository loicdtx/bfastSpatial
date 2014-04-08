#' @title Apply a sieve to a raster layer
#' 
#' @description Applies an areal threshold to a raster layer. A threshold is supplied in square metres (to be generalized in future) and all pixel 'clumps' smaller than this threshold are deleted.
#' 
#' 
#' @param x Input raster layer.
#' @param thresh Numeric. Areal threshold (in square metres). All pixel clumps smaller than thresh will be deleted.
#' @param directions Numeric. Define pixel neighbours using diagonals (diagonals=8; "Queen's case") or without diagonals (diagonals=4; "Rook's case").
#' @param ... Additional arguments to be passed to \link{overlay} (including filename to write to file).
#' @author Ben DeVries \email{devries.br@@gmail.com}
#' @return raster with pixels in clumps smaller than that specified as thresh removed. All spatial parameters and data are otherwise identical to x.
#' @import raster
#' @import igraph
#' @export
#' 

areaSieve <- function(x, thresh=5000, directions=8, verbose=FALSE, ...)
{
  
  # convert thresh from area to pixel threshold
  # TODO: make this applicable to all projections
  thresh <- ceiling(thresh/(res(x)[1]*res(x)[2]))
  if(verbose)
    cat("Converted threshold to ", thresh, " pixels.\n", sep="")
  
  # derive a forest clump raster from x
  clumps <- clump(x, directions=directions)
  
  # calculate pixel frequency for each clumpID
  clumpFreq <- as.data.frame(freq(clumps))
  
  # clumpID to be excluded from output raster
  excludeID <- clumpFreq$value[which(clumpFreq$count < thresh)]
  
  # function to assign NA to x wherever a clump with ID %in% excludeID is found
  subNA <- function(a, b){
      a[b %in% excludeID] <- NA
      return(a)
  }
  
  y <- overlay(x, clumps, fun=subNA, ...)
  
  return(y)
  
}
