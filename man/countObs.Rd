\name{countObs}
\alias{countObs}
\title{Valid observations per pixel}
\usage{
countObs(x, navalues = c(NA), sensor = "all", as.perc = FALSE, ...)
}
\arguments{
  \item{x}{RasterBrick or RasterStack}

  \item{navalues}{Integer. Values representing NA's in time
  series. Defaults to NA, but multiple values can be
  supplied (e.g. \code{navalue = c(0, NA)})}

  \item{as.perc}{Logical. Express result as a percentage of
  nlayers(x)?}

  \item{...}{Arguments to be passed to
  \link{\code{mc.calc}}}
}
\description{
Calculates the number of valid observations per pixel of a
time series RasterBrick.
}
\author{
Ben DeVries (\email{devries.br@gmail.com})
}

