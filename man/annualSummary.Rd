\name{annualSummary}
\alias{annualSummary}
\title{Annual summary of a time series RasterBrick}
\usage{
annualSummary(x, fun, sceneID = NULL, years = NULL, sensor = "all",
  na.rm = NULL, ...)
}
\arguments{
  \item{RasterBrick}{or RasterStack}

  \item{fun}{Function to apply over each pixel for each
  year}

  \item{sceneID}{Character. Optional: Landsat scene ID's
  for each layer of the input RasterBrick or RasterStack.
  If not given, sceneID's must be contained in the layer
  names}

  \item{years}{Numeric. Vector of years to which to limit
  the summary.}

  \item{sensor}{Character. Optional: limit calculation to
  images from a particular sensor. Defaults to "all", but
  can take any of "TM", "ETM+", "ETM+ SLC-off" or "ETM+
  SLC-on"}

  \item{...}{Arguments to be passed to
  \link{\code{mc.calc}}}
}
\value{
RasterBrick with results of fun for each year represtented
in the input time series RasterBrick.
}
\description{
Calculates pixel-based statistics for every year
represented by a time series RasterBrick
}
\details{
If \code{fun} takes a \code{na.rm} argument and none is
supplied, it will be ignored and the default value for
\code{na.rm} for that function will be used.
}
\examples{
# load tura RasterBrick
data(tura)

# calculate mean and standard deviation values per year for ETM+ data only
annualMean <- annualSummary(tura, fun=mean, na.rm=TRUE, sensor="ETM+")
plot(annualMean)
annualSD <- annualSummary(tura, fun=sd, na.rm=TRUE, sensor="ETM+")
plot(annualSD)

# custom function to calculate # of non-NA values per pixel per year (similar to countObs())
ff <- function(x)
 length(x[!is.na(x)])
annualObs <- annualSummary(tura, fun=ff, sensor="ETM+")
}
\author{
Ben DeVries \email{devries.br@gmail.com}
}
\seealso{
\link{\code{summaryBrick}}
}

