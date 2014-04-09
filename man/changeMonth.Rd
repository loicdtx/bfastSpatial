\name{changeMonth}
\alias{changeMonth}
\title{Extract month of change from bfmChange RasterLayer}
\usage{
  changeMonth(change, ...)
}
\arguments{
  \item{change.}{RasterLayer output from
  \code{\link{bfmChange}} representing pixel breakpoints.}

  \item{...}{Additional arguments to pass to
  \code{\link{writeRaster}}}
}
\value{
  either a RasterLayer with values between 1 to 12
  (representing month of change), or if multiple years are
  represented in the input change RasterLayer, a
  RasterBrick with one layer for each year, and values of 1
  to 12 representing change months for each year.
}
\description{
  Returns one RasterLayer for each year represented by the
  change results. Months are represted by integers from 1
  to 12.
}
\author{
  Ben DeVries \email{devries.br@gmail.com}
}
\seealso{
  \code{\link{bfmChange}}
}

