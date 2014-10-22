\name{areaSieve}
\alias{areaSieve}
\title{Apply a sieve to a raster layer}
\usage{
areaSieve(x, thresh = 5000, directions = 8, verbose = FALSE, ...)
}
\arguments{
  \item{x}{Input raster layer.}

  \item{thresh}{Numeric. Areal threshold (in square
  metres). All pixel clumps smaller than thresh will be
  deleted.}

  \item{directions}{Numeric. Define pixel neighbours using
  diagonals (diagonals=8; "Queen's case") or without
  diagonals (diagonals=4; "Rook's case").}

  \item{...}{Additional arguments to be passed to
  \link{overlay} (including filename to write to file).}
}
\value{
raster with pixels in clumps smaller than that specified as
thresh removed. All spatial parameters and data are
otherwise identical to x.
}
\description{
Applies an areal threshold to a raster layer. A threshold
is supplied in square metres (to be generalized in future)
and all pixel 'clumps' smaller than this threshold are
deleted.
}
\author{
Ben DeVries \email{devries.br@gmail.com}
}

