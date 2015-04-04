library(strucchange)
library(bfast)

ndmiZoo <- readRDS('data/maf_NDMI.rds')

ts <- bfastts(ndmiZoo[,1], index(ndmiZoo), 'irregular')
pp <- bfastpp(ts, order = 3)

bp <- breakpoints(response ~ trend, data = pp, h = 0.1)

# Subset df based on breakdates
bDates <- pp$time[bp$breakpoints] # Do we need that
segments <- c(pp$time[c(1,bp$breakpoints, nrow(pp))])
for (i in 1:(length(segments) - 1)) {
    # To deal with begining of time-series boundary
    if (i == 1) {
        subDf <- subset(pp, time <= segments[i + 1])
    } else {
        subDf <- subset(pp, time <= segments[i + 1] & time > segments[i]) 
    }
    m <- mean(subDf$response, na.rm = TRUE)
    model <- lm(response ~ trend, data = subDf)
    sl <- model$coefficients[2]
    n <- nrow(subDf)
    yBegin <- predict(model, newdata = subDf[1,]) # Beginning of segment (!= to intersection with break)
    yEnd <- predict(model, newdata = subDf[n,]) # End of segment (== intersection with breakLine)
    duration <- diff(range(subDf$time))
    segNum <- i - 1
    colNames0 <- c('meanSeg', 'slopeSeg', 'yBeginSeg', 'yEndSeg', 'durationSeg')
    colNames <- sprintf('%s%d', colNames0, i)
    # Fill dataframe 
    dfOut[1, colNames] <- c(m, sl, yBegin, yEnd, duration)
}
