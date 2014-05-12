\name{processLandsatBatch}
\alias{processLandsatBatch}
\title{Process Landsat data in batch mode}
\usage{
processLandsatBatch(x, pattern = NULL, outdir, srdir, mc.cores = 1, ...)
}
\arguments{
  \item{x}{Character. Directory where the data is located.
  Data can be of the following formats: hdf, geoTiff,
  tar.gz or zip)}

  \item{pattern}{Only useful if x if of length 1. See
  \link{list.files} for more details}

  \item{outdir}{Character. Directory where the vegetation
  index rasterLayer should be written.}

  \item{srdir}{Character. Directory where the tarball
  should be uncompressed. Can be ommited if \code{untar} is
  set to \code{FALSE}}

  \item{mc.cores}{Numeric. For multicore implementation
  only. See \link{mclapply}}

  \item{...}{Arguments to be passed to
  \link{processLandsat} (\code{untar}, \code{delete}) or to
  \link{sr2vi} (\code{e}, \code{mask}, \code{keep},
  \code{vi})}
}
\value{
Function is used for its side effect of calculating in
batch mode Vegetation indices fron surface reflectance
Lantsat data.
}
\description{
Batcher to process Landsat data from tarball or hdf to a
list of Vegetation Index files. Runs \link{processLandsat}
sequentially or in parallel
}
\examples{
# Get the directory where the Landsat archives are stored
dir <- system.file('external', package='bfastSpatial')

# Set the location of output and intermediary directories (everything in tmpdir in that case)
srdir <- dirout <- file.path(rasterOptions()$tmpdir, 'bfmspatial')
dir.create(dirout, showWarning=FALSE)
processLandsatBatch(x=dir, pattern=glob2rx('*.zip'), outdir=dirout, srdir=srdir, delete=TRUE, vi='ndvi', mask='fmask', keep=0, overwrite=TRUE)

# Visualize one of the layers produced
list <- list.files(dirout, pattern=glob2rx('*.grd'), full.names=TRUE)
plot(r <- raster(list[1]))
}
\author{
Loic Dutrieux
}
\seealso{
\link{processLandsat} and \link{sr2vi}
}

