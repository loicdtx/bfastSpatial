\name{getSceneinfo}
\alias{getSceneinfo}
\title{Retrieve Landsat info from filenames}
\usage{
getSceneinfo(sourcefile, filename = NULL, ...)
}
\arguments{
  \item{sourcefile}{Character. Filename of a landsat layer
  or dataset.}

  \item{...}{Additional arguments to pass to
  \code{\link{write.csv}}.}
}
\value{
a \code{data.frame} with parsed scene information from
Landsat scene names
}
\description{
Parses through typical Landsat filenames and retrieves
information on sensor and acquisition date. Vectorized over
\code{sourcefile}.
}
\author{
Ben DeVries \email{devries.br@gmail.com}
}

