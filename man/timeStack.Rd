\name{timeStack}
\alias{timeStack}
\title{Creates a time stack of Landsat layers}
\usage{
  timeStack(x, pattern = NULL, ...)
}
\arguments{
  \item{x}{character. dir containing the files to be
  stacked or character list (the files). IN the former
  case, it is recommended to use the \code{pattern}
  argument}

  \item{pattern}{See \link{list.files}}

  \item{...}{Arguments to be passed to \link{writeRaster}.
  If specifying a filename, it is strongly recommended to
  also set a datatype.}
}
\description{
  Creates a stack of Landsat layers, reordering them
  chronologically
}
\author{
  Loic Dutrieux
}

