\name{tura}
\alias{tura}
\docType{data}
\title{
NDVI time series raster brick}
\description{
Time series raster brick for an area in Tura kebele, Kafa Zone, SW Ethiopia. Values are NDVI rescaled by a factor of 10000. Data originate from the Landsat 5 TM and Landsat 7 ETM+ sensors. Original Landsat scene names can be found by typing \code{names(tura)}. Dates are also contained in the z-dimension (\code{getZ(tura)}; see also \code{\link{timeStack}})}.
\usage{data(tura)}
\source{
http://earthexplorer.usgs.gov}
\references{
DeVries B., Verbesselt J., Kooistra L. and Herold M. (2014). Robust Monitoring of Small-Scale Forest Disturbances in an Afromontane Forest Using Landsat Time Series. Remote Sensing of Environment, in review.
}
\examples{
data(tura)

# scene names
names(tura)
s <- getSceneinfo(names(tura))
s$year <- as.numeric(format(s$date, format = "\%Y"))
hist(s$year, breaks = c(1980:2015))

# plot first 9 scenes
plot(tura, 1:9)
plot(tura, 1:9, main = getZ(tura))

# plot a time series of the 50th cell
plot(getZ(tura), tura[50], xlab="time", ylab="NDVI (x 10000)")

# make a new rasterBrick from only ETM+ scenes
x <- subsetRasterTS(tura, sensor = "ETM+")

# plot time series from the revised brick
plot(getZ(x), x[50], xlab="time", ylab="NDVI (x 10000)")
}
\keyword{datasets}
