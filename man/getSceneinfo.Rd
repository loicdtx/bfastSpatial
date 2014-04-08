\name{getSceneinfo}
\alias{getSceneinfo}
\title{Retrieve Landsat info from filenames}
\usage{
getSceneinfo(sourcefile, filename = NULL, ...)
}
\arguments{
  \item{sourcefile}{Character. Filename of a landsat layer
  or dataset.}

  \item{filename}{Character or NULL. For the output
  data.frame to be written to a csv file.}

  \item{...}{Additional arguments to pass to
  \code{\link{write.csv}}.}
}
\value{
a data.frame with parsed scene information from Landsat
scene names
}
\description{
Parses through typical Landsat filenames and retrieves
information on sensor and acquisition date. Vectorized over
\code{sourcefile}.
}
\author{
Ben DeVries \email{devries.br
}

