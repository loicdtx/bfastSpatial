\name{tura}
\alias{tura}
\docType{data}
\title{
NDVI time series raster brick}
\description{
Time series raster brick for an area in Tura kebele, Kafa Zone, SW Ethiopia. Values are NDVI rescaled by a factor of 10000. Data originate from the Landsat 5 TM and Landsat 7 ETM+ sensors. Original Landsat scene names can be found by typing \code{names(tura)}.}
\usage{data(tura)}
\source{
http://earthexplorer.usgs.gov}
\references{
DeVries B., Verbesselt J., Kooistra L. and Herold M. (2014). Robust Monitoring of Small-Scale Forest Disturbances in the Tropics Using Landsat Time Series. Remote Sensing of Environment, in review.
}
\examples{
data(tura)

# scene names
names(tura)
s <- getSceneinfo(names(tura))

# plot first 9 scenes
plot(tura, 1:9)

# plot a time series of the 50th cell
plot(s$date, tura[50], xlab="time", ylab="NDVI")

# remove all TM layers
x <- dropLayer(tura, which(s$sensor == "TM"))

# redefine s
s <- getSceneinfo(names(x))

# plot time series again
plot(s$date, x[50], xlab="time", ylab="NDVI")
}
\keyword{datasets}
