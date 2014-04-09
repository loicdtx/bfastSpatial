\name{processLandsatBatch}
\alias{processLandsatBatch}
\title{Process Landsat data in batch mode}
\usage{
  processLandsatBatch(x, pattern = NULL, outdir, hdfdir,
    mc.cores = 1, ...)
}
\arguments{
  \item{x}{Character. Directory where the data is located
  (hdf or tar.gz)}

  \item{pattern.}{Only useful if x if of length 1. See
  \link{list.files} for more details}

  \item{outdir.}{Character. Directory where the vegetation
  index rasterLayer should be written.}

  \item{hdfdir}{Character. Directory where the tarball
  should be uncompressed. Can be ommited if \code{untar} is
  set to \code{FALSE}}

  \item{mc.cores}{Numeric. For multicore implementation
  only. See \link{mclapply}}

  \item{...}{Arguments to be passed to
  \link{processLandsat} (\code{untar}, \code{delete}) or to
  \link{hdf2ndvi} (\code{e}, \code{mask}, \code{keep})}
}
\value{
  Function is used for its side effect of calculating in
  batch Vegetation indices fron surface reflectance Lantsat
  data.
}
\description{
  Batcher to process Landsat data from tarball or hdf to a
  list of Vegetation Index files. Runs
  \link{processLandsat} sequentially or in parallel
}
\author{
  Loic Dutrieux
}
\seealso{
  \link{processLandsat} and \link{hdf2ndvi}
}

