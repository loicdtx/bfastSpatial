\name{hdf2ndvi}
\alias{hdf2ndvi}
\title{Calculate NDVI from Landsat hdf file}
\usage{
hdf2ndvi(x, e = NULL, mask = NULL, keep = c(0), ...)
}
\arguments{
  \item{x}{Character. File name of the hdf file containing
  the bands.}

  \item{e}{Extent object or object that can be coerced as
  extent.}

  \item{mask}{Numeric or NULL. The subdataset number of the
  mask to be applied to the bands.}

  \item{keep}{umeric. Can take multiple values. Which
  values of the mask layer should be kept?}

  \item{...}{Arguments to be passed to
  \code{\link{writeRaster}}}
}
\value{
A rasterLayer object
}
\description{
Extract layers, apply mask (optional), crop (optional),
calculate NDVI (possible other indices supported in the
future) and write output to file (optional)
}
\author{
Loic Dutrieux
}
\seealso{
\code{\link{processLandsat}} for wrapper function
}

