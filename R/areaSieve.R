#' @title Apply a sieve to a raster layer
#' 
#' @description Applies an areal threshold to a raster layer. A threshold is supplied in square metres (to be generalized in future) and all pixel 'clumps' smaller than this threshold are deleted.
#' 
#' @param x Input raster layer.
#' @param thresh Numeric. Areal threshold (in square metres). All pixel clumps smaller than this threshold will be deleted.
#' @param directions Numeric. Define pixel neighbours using diagonals (\code{directions=8}; "Queen's case") or without diagonals (\code{directions=4}; "Rook's case"). See \code{\link{clump}} for more information.
#' @param keepzeros Logical. Treat zeros as NA's (default option for \code{\link{clump}}) or as actual pixel values? If \code{FALSE} (default), the default method used in \code{\link{clump}} is used. If \code{TRUE}, an intermediate unit raster is first created, \code{areaSieve} is run on that raster, and the sieved unit raster is used to mask the input raster.
#' @param cores Numeric. Number of cores to use for parallel processing (only valid if x is a RasterBrick or RasterStack)
#' @param ... Additional arguments to be passed to \code{\link{writeRaster}} if \code{nlayers(x) > 1}, or to \code{\link{mask}} if \code{nlayers(x) == 1} and \code{keepzeroes == TRUE}, or to \code{\link{overlay}} if \code{nlayers(x) == 1} and \code{keepzeroes==FALSE}.
#' 
#' @author Ben DeVries
#' 
#' @return raster with pixels in clumps smaller than that specified as thresh removed. All spatial parameters and data are otherwise identical to x.
#' 
#' @details
#' In all cases, a \code{filename} can be passed to \code{areaSieve}, but the arguments allowed by the \code{...} differ depending on whether a multi-layered raster object is supplied - in which case  \code{writeRaster} is used - or if a single-layered raster is supplied - in which case either \code{mask} or \code{overlay} is used.
#' 
#' \code{areaSieve} is based on the \code{raster::clump}, which by default ignores zeroes (ie. considers them as NA's). To consider zeroes as valid pixel values when applying the sieve, set \code{keepzeroes} to \code{TRUE}.
#' 
#' @import raster
#' @import igraph
#' @import foreach
#' @export
#' 
#' @seealso \code{\link{clump}}
#' 
#' @examples
#' # load test raster
#' data(tura)
#' # extract the first layer
#' r <- raster(tura, 1)
#' 
#' # create pixel 'islands' by reassigning some values to NA
#' r[r < 7500] <- NA
#' # zoom into extent where pixel islands were generated
#' e <- extent(c(821958, 822697, 830504, 831070))
#' plot(r, ext=e)
#' 
#' # apply areaSieve with default threshold of 5000 square metres
#' rs <- areaSieve(r)
#' 
#' # compare two rasters
#' op <- par(mfrow=c(1, 2))
#' plot(r, ext=e, legend=FALSE)
#' plot(rs, ext=e, legend=FALSE)
#' par(op)
#' 
#' # same as above, but assign 0 instead of NA
#' # (ie. simulate a situation where 0's are valid pixel values)
#' r <- raster(tura, 1)
#' r[r < 7500] <- 0
#' 
#' # apply areaSieve with default threshold of 5000 square metres and keep zeros
#' rs <- areaSieve(r, keepzeros = TRUE)
#' 
#' # compare two rasters
#' op <- par(mfrow=c(1, 2))
#' plot(r, ext=e, legend=FALSE)
#' plot(rs, ext=e, legend=FALSE)
#' par(op)
#' 

areaSieve <- function(x, thresh=5000, directions=8, verbose=FALSE, keepzeros=FALSE, cores=1, ...)
{
  
  # convert thresh from area to pixel threshold
  # TODO: make this applicable to all projections
  thresh <- ceiling(thresh/(res(x)[1]*res(x)[2]))
  if(verbose)
    cat("Converted threshold to ", thresh, " pixels.\n", sep="")
  
  # generic sieve function
  sieve <- function(inp, ...){
      # derive a forest clump raster from unitRaster
      clumps <- clump(inp, directions=directions)
      
      # calculate pixel frequency for each clumpID
      clumpFreq <- as.data.frame(freq(clumps))
      
      # clumpID to be excluded from output raster
      excludeID <- clumpFreq$value[which(clumpFreq$count < thresh)]
      
      # function to assign NA to x wherever a clump with ID %in% excludeID is found
      subNA <- function(a, b){
          a[b %in% excludeID] <- NA
          return(a)
      }
      
      # apply sieve to unitRaster
      if(!keepzeros){
          y <- overlay(inp, clumps, fun=subNA, ...)
      } else {
          y <- overlay(inp, clumps, fun=subNA)
      }
      
      return(y)
  }

  if(nlayers(x) > 1){
      registerDoMC(cores=cores)
      y <- foreach(i = 1:nlayers(x)) %dopar% {
        if(keepzeros){
          unitRaster <- x[[i]]
          unitRaster[!is.na(unitRaster)] <- 1
          
          # apply sieve on unitRaster
          y <- sieve(unitRaster)
          
          # use sieved unitRaster to mask input raster
          y <- mask(x[[i]], y)
          
        } else {
          y <- sieve(x[[i]])
        }
        return(y)
      }
      y <- do.call("brick", y)
      names(y) <- names(x)
      if(hasArg(filename))
          writeRaster(x, ...)
  } else {
  # create a unit raster if keepzeros==TRUE
      if(keepzeros){
          unitRaster <- x
          unitRaster[!is.na(unitRaster)] <- 1
          
          # apply sieve on unitRaster
          y <- sieve(unitRaster)
          
          # use sieved unitRaster to mask input raster
          y <- mask(x, y, ...)
          
      } else {
          y <- sieve(x, ...)
      }
  }
  
  return(y)
}
