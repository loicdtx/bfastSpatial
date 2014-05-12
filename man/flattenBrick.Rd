\name{flattenBrick}
\alias{flattenBrick}
\title{Convert a RasterBrick or RasterStack to a RasterLayer based on the first encountered value}
\usage{
flattenBrick(x, thresh = NULL, ...)
}
\arguments{
  \item{x}{RasterBrick or RasterStack.}

  \item{thresh}{Numeric. Optional: threshold to apply to
  preceding values in deciding whether to mask subsequent
  values.}

  \item{...}{Additional arguments to be bassed to
  \code{\link{mc.calc}}.}
}
\value{
RasterLayer with values representing 'earliest' encountered
values not exceeding \code{thresh}.
}
\description{
Flatten a brick by iteratively masking layer values by
non-NA values in preceding layers. An optional threshold
can be supplied, where preceding values exceeding the
threshold are ignored in the masking procedure (and are
therefore themselves removed).
}
\details{
\code{thresh} only works on a 'greater-than' basis. To
achieve the reverse, first let \code{x <- -1*x} then
convert the result back by \code{y <- -1*y}.
}
\author{
Ben DeVries \email{devries.br@gmail.com}
}

