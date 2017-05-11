#' @title Bit index to numeric
#' 
#' @description Converts a vector of bit positions to the correspondng integer. This aim of this function is to help users dealing with bit coded information, such as Pixel Quality information provided with Landsat and MODIS data.
#' 
#' @param x Vector of numeric where each value correspond to a bit position
#' 
#' @return Numeric
#' 
#' @export
#' 
#' @author Loic Dutrieux
#' 
#' @examples
#' # We want to know the base 10 numeric value of a binary word that has bit indices 0, 4 and 5 activated (0011 0001)
#' bitToInt(c(0, 4, 5))
bitToInt <- function(x) {
    bit_vector <- rep(0, max(x))
    bit_vector[x + 1] <- 1
    out <- 0
    for (i in rev(bit_vector)) {
        out <- bitwOr(bitwShiftL(out, 1), i)
    }
    return(out)
}