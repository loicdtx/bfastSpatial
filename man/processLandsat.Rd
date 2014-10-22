\name{processLandsat}
\alias{processLandsat}
\title{Wrapper function to process Landsat data}
\usage{
processLandsat(x, hdfdir, outdir, untar = TRUE, delete = FALSE, ...)
}
\arguments{
  \item{x}{Character. filename of the tarball or of the hdf
  file.}

  \item{hdfdir}{Character. Directory where the tarball
  should be uncompressed. Can be ommited if \code{untar} is
  set to \code{FALSE}}

  \item{outdir}{Character. Directory where the vegetation
  index rasterLayer should be written.}

  \item{untar}{Logical. IS there a need to untar data, or
  have they been previously unpacked.}

  \item{delete}{Logical. Should hdf files be deleted after
  vegetation index calculated? (usefull for disk space
  management; hdf files are very voluminous and a user may
  want to keep the Landsat archive in compressed format
  only)}

  \item{...}{Arguments to be passed to \link{hdf2ndvi}. Do
  not specify \code{filename} since it is automatically
  generated}
}
\value{
rasterLayer Object also written to file (in \code{outdir})
with an automatically generated filename
}
\description{
Processes a single Landsat scene, from tarball (or hdf if
untar is set to FALSE) to vegetation index (only NDVI
supported at the moment). Easy to batch using sapply or
mclapply for parallel implementation.
}
\author{
Loic Dutrieux
}
\seealso{
\link{hdf2ndvi}
}

