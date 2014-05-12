#' @title Draw an extent on a raster plot and print command to console
#' 
#' @description Identical to \code{\link{drawExtent}} except that the resulting extent is printed to the console with a formatted \code{extent()} command which can be copied and pasted to a script for reproducability.
#' 
#' @param ... Arguments to be passed to \code{\link{drawExtent}}
#' @param digits integer indicating the number of decimal places (passed to \link{\code{round}})
#' 
#' @return an object of class \code{extent} with the side effect of having the \code{extent()} command with resulting parameters printed to the console for reproducability.
#' 
#' @author Ben DeVries
#' 
#' @examples
#' \dontrun{
#' data(tura)
#' plot(tura, 6)
#' e <- saveExtent() # draw extent on the plot, and associated extent() command will be printed to console, while extent is saved to workspace
#' }
#' 
#' @import raster
#' @export
#' 
#' @seealso \link{\code{drawExtent}}

saveExtent <- function(..., digits=0){
  # drawExtent() and print R code for reproducing that extent to console
  # ...: arguments which can be passed to drawExtent()
  
  # user draws an extent on an already plotted image
  e <- drawExtent(...)
  
  # round extent to digits
  e <- round(e, digits=digits)
  
  # print to console
  cat("Reproduceable code:\n\textent(c(", 
      paste(as.vector(e), collapse=", "), "))\n\n", sep="")

  return(e)
}
