\name{bfmSpatial}
\alias{bfmSpatial}
\title{Function to run bfastmonitor on any kind of raster brick, with parallel support}
\usage{
bfmSpatial(x, dates = NULL, pptype = "irregular", start,
  formula = response ~ trend + harmon, order = 3, lag = NULL,
  slag = NULL, history = c("ROC", "BP", "all"), type = "OLS-MOSUM",
  h = 0.25, end = 10, level = 0.05, mc.cores = 1, ...)
}
\arguments{
  \item{x}{rasterBrick or rasterStack object, or file name
  to a multilayer raster object stored on disk.}

  \item{dates}{A date vector. The number of dates must
  match the number of layers of x.}

  \item{pptype}{Character. Type of preprocessing to be
  applied to individual time series vectors. The two
  options are 'irregular' and '16-days'. See bfastts for
  more details.}

  \item{start}{See \code{\link{bfastmonitor}}}

  \item{formula}{See \code{\link{bfastmonitor}}}

  \item{order}{See \code{\link{bfastmonitor}}}

  \item{lag}{See \code{\link{bfastmonitor}}}

  \item{slag}{See \code{\link{bfastmonitor}}}

  \item{history}{See \code{\link{bfastmonitor}}}

  \item{type}{See \code{\link{bfastmonitor}}}

  \item{n}{See \code{\link{bfastmonitor}}}

  \item{level}{See \code{\link{bfastmonitor}}}

  \item{mc.cores}{Numeric. Number of cores to be used for
  the job.}

  \item{...}{Arguments to be passed to
  \code{\link{mc.calc}}}
}
\value{
A rasterBrick, with 2 layers. One layer is the timing of
change and the other layer the magnitude of change. See
\code{\link{bfastmonitor}}
}
\description{
Implements bfastmonitor function, from the bfast package on
any kind of rasterBrick object. Time information is
provided as an extra object and the time series can be
regular or irregular.
}
\author{
Loic Dutrieux
}

