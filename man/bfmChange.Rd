\name{bfmChange}
\alias{bfmChange}
\title{Extract breakpoints from bfm RasterBrick}
\usage{
  bfmChange(bfm, ...)
}
\arguments{
  \item{bfm}{RasterBrick output from
  \code{\link{bfmSpatial}}. 1st layer is the breakpoint
  timing and the 2nd is the change magnitude.}

  \item{...}{Additional arguments to pass to
  \code{\link{writeRaster}}.}
}
\value{
  RasterLayer with breakpoint timing per pixel. Breakpoint
  times are in the format yyyy.jjj (where y is year and j
  is JulianDay/365).
}
\description{
  Creates a raster layer with breakpoint timing values from
  the output bfm raster brick
}
\details{
  The output of this function can also be used as a
  parameter in the \code{\link{bfmMagn}} function to filter
  out pixels where no breakpoint was detected.
}
\author{
  Ben DeVries \email{devries.br@gmail.com}
}
\seealso{
  \code{\link{bfmMagn}}, \code{\link{changeMonth}}
}

