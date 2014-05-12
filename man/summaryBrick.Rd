\name{summaryBrick}
\alias{summaryBrick}
\title{Summarize a RasterBrick}
\usage{
summaryBrick(x, fun, dates = NULL, sceneID = NULL, na.rm = NULL,
  minDate = NULL, maxDate = NULL, sensor = "all", ...)
}
\arguments{
  \item{x}{RasterBrick or RasterStack to be summarized}

  \item{fun}{Function to apply to vectors extracted from
  each pixel}

  \item{dates}{Date. Optional: vector of dates
  corresponding exactly to layers of \code{x}}

  \item{sceneID}{Character. Optional: vector of Landsat
  scene ID's corresponding exactly to layers of \code{x}}

  \item{minDate}{Date, Character or Numeric. Optional:
  minimum date to include in the calculation (see
  \link{\code{subsetRasterTS}}). Should either be supplied
  as a \code{date} or \code{numeric} of length 2 (see
  Details)}

  \item{maxDate}{Date, Character or Numeric. Optional:
  maximum date to include in the calculation (see
  \link{\code{subsetRasterTS}}). Should either be supplied
  as a \code{date} or \code{numeric} of length 2 (see
  Details)}

  \item{sensor}{Character. Optional: limit calculation to
  selected (Landsat) sensors. Defaults to "all" for all
  data.}

  \item{...}{Additional arguments to be passed to
  \link{\code{mc.calc}}}
}
\value{
A Raster layer representing the summary statistic of each
pixel in the input RasterBrick or RasterStack
}
\description{
Computes pixel-based summary statistics for a multi-layered
raster object
}
\details{
If \code{fun} takes a \code{na.rm} argument and none is
supplied, it will be ignored and the default value for
\code{na.rm} for that function will be used.

If \code{fun} returns a vector of length greater than one,
a RasterBrick object will be returned (see \code{fun=range}
example in examples)

\code{minDate} and \code{maxDate} are optional arguments to
limit the calculation to a specific date range. These
arguments can be supplied as Date or Character objects in
the form "%Y-%m-%d" or as a Numeric of length 2. In the
latter case, the first element is the year, and the second
is the Julian day (from 1 to 365).
}
\seealso{
\link{\code{annualSummary}}
}

