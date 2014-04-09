\name{bfmMagn}
\alias{bfmMagn}
\title{Extract change magnitude bfm RasterBrick}
\usage{
  bfmMagn(bfm, change = NULL, thresh = NULL, ...)
}
\arguments{
  \item{bfm}{RasterBrick output from
  \code{\link{bfmSpatial}}. 1st layer is the breakpoint
  timing and the 2nd is the change magnitude.}

  \item{change}{Optional: RasterLayer output from
  \code{\link{bfmChange}}. Used to mask out all
  non-breakpoints from magnitude RasterLayer.}

  \item{thresh}{Numeric. Optional: threshold to apply to
  magnitude thresholds. Any magnitude values above this
  threshold will be excluded.}

  \item{...}{Arguments to be passed to
  \code{\link{writeRaster}}.}
}
\value{
  RasterLayer representing the change magnitude for all
  pixels, unless change filter is applied, in which case
  only representing breakpoint pixels.
}
\description{
  Creates a raster layer representing the change magnitude
  (median of the residuals during the monitoring period)
  from a resulting bfm raster brick. Magnitude is
  calculated for all pixels regardless of whether a
  breakpoint was detected or not. This function allows for
  filtering based on a supplied change (breakpoint) raster.
}
\examples{
# load in raster brick
data(tura)

# run bfm on brick for monitoring period 2009 and using all available data as the stable history period.
bfm <- bfmSpatial(tura, start=c(2009,1))

# extract and plot change magnitude (continuous values)
magn <- bfmMagn(bfm)
plot(magn)

# extract magnitude only for pixels where breaktpoint were detected
change <- bfmChange(bfm)
magn.filt <- bfmMagn(bfm, change = change)
plot(magn.filt)

# extract breakpoint magnitudes for large changes (where dNDVI < -0.03)
magn.large <- bfmMagn(bfm, change = change, thresh=-0.03)
plot(magn.large)

if("rasterVis" \%in\% row.names(installed.packages())){
    library(rasterVis)
    levelplot(brick(magn, magn.filt, magn.large))
}
}
\author{
  Ben DeVries \email{devries.br@gmail.com}
}
\seealso{
  \code{\link{bfmChange}}
}

