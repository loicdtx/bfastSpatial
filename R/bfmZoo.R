bfmZoo <- function(x, start, mc.cores = 1) {
    
    
    mclapply(X = ts, FUN = bfastmonitor, start = start, mc.cores = mc.cores)
}