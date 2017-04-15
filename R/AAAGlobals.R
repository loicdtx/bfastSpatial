VEGETATION_INDICES <- list(
    ### LAndsat 4/5
    T = list(ndvi = list(bands = c('sr_band3', 'sr_band4'),
                         fun = function(x1, x2){10000 * (x2 - x1) / (x1 + x2)}),
             ndmi = list(bands = c('sr_band5', 'sr_band7'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             ndwi = list(bands = c('sr_band2', 'sr_band4'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             mndwi = list(bands = c('sr_band2', 'sr_band5'),
                          fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             nbr = list(bands = c('sr_band4', 'sr_band7'),
                        fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             savi = list(bands = c('sr_band3', 'sr_band4'),
                         fun = function(x1, x2, L){10000 * (1 + L) * (x2/10000 - x1/10000) / (x2/10000 + x1/10000 + L)}),
             evi = list(bands = c('sr_band1', 'sr_band3', 'sr_band4'),
                        fun = function(x1, x2, x3){10000 * 2.5 * (x3/10000 - x2/10000)/(x3/10000 + 6 * x2/10000 - 7.5 * x1/10000 + 1)}
              ),
             tcb = list(bands = c('sr_band1', 'sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band7'),
                        # TODO: for tassel cap, add reference(s), where did we get these coefficients? (crist, 1985, A TM tasseled cap equivalent transformation for reflectance factor data) ?
                        fun = function(x1, x2, x3, x4, x5, x6){sum(c(0.2043, 0.4158, 0.5524, 0.5741, 0.3124, 0.2303) * c(x1, x2, x3, x4, x5, x6))}),
             tcg = list(bands = c('sr_band1', 'sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){sum(c(-0.1603, 0.2819, -0.4934, 0.7940, -0.0002, -0.1446) * c(x1, x2, x3, x4, x5, x6))}),
             tcw = list(bands = c('sr_band1', 'sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){sum(c(0.0315, 0.2021, 0.3102, 0.1594, -0.6806, -0.6109) * c(x1, x2, x3, x4, x5, x6))})
             
    ),
    ### Landsat 8
    C = list(ndvi = list(bands = c('sr_band4', 'sr_band5'),
                         fun = function(x1, x2){10000 * (x2 - x1) / (x1 + x2)}),
             ndmi = list(bands = c('sr_band6', 'sr_band7'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             ndwi = list(bands = c('sr_band3', 'sr_band5'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             mndwi = list(bands = c('sr_band3', 'sr_band6'),
                          fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             nbr = list(bands = c('sr_band5', 'sr_band7'),
                        fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)}),
             savi = list(bands = c('sr_band4', 'sr_band5'),
                         fun = function(x1, x2, L){10000 * (1 + L) * (x2/10000 - x1/10000) / (x2/10000 + x1/10000 + L)}),
             evi = list(bands = c('sr_band2', 'sr_band4', 'sr_band5'),
                        fun = function(x1, x2, x3){10000 * 2.5 * (x3/10000 - x2/10000)/(x3/10000 + 6 * x2/10000 - 7.5 * x1/10000 + 1)}
             ),
             tcb = list(bands = c('sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band6', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){sum(c(0.2043, 0.4158, 0.5524, 0.5741, 0.3124, 0.2303) * c(x1, x2, x3, x4, x5, x6))}),
             tcg = list(bands = c('sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band6', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){sum(c(-0.1603, 0.2819, -0.4934, 0.7940, -0.0002, -0.1446) * c(x1, x2, x3, x4, x5, x6))}),
             tcw = list(bands = c('sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band6', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){sum(c(0.0315, 0.2021, 0.3102, 0.1594, -0.6806, -0.6109) * c(x1, x2, x3, x4, x5, x6))})
             
    )
)

VEGETATION_INDICES$E <- VEGETATION_INDICES$T
                    
