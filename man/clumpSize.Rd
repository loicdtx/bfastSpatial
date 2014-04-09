\name{clumpSize}
\alias{clumpSize}
\title{Calculate size of pixel clumps in a RasterLayer}
\usage{
  clumpSize(x, f = 1, ...)
}
\arguments{
  \item{x}{RasterLayer.}

  \item{f}{Numeric. Optional: conversion factor for number
  of pixels (e.g. f=900/10000 for converting Landsat pixels
  to area in hectares).}

  \item{...}{Additional arguments to pass to
  \code{\link{clump}}}
}
\description{
  Calculate the size of pixel clumps in a RasterLayer. The
  clump area is assigned to each clump, and araster with
  identical dimensions, but values representing the clump
  size is output. The definition of a pixel clump follows
  that of \code{\link{clump}}, where diagonals are either
  included or ignored in defining clumps. An optional
  conversion factor can be supplied based on pixel size.
}
\author{
  Ben DeVries
}

