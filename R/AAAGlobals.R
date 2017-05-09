bfastSpatial.env <- new.env()
bfastSpatial.env$VEGETATION_INDICES <- list(
    ### LAndsat 4/5
    T = list(ndvi = list(bands = c('sr_band3', 'sr_band4'),
                         fun = function(x1, x2){10000 * (x2 - x1) / (x1 + x2)},
                         datatype = 'INT2S'),
             ndmi = list(bands = c('sr_band5', 'sr_band7'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                         datatype = 'INT2S'),
             ndwi = list(bands = c('sr_band2', 'sr_band4'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                         datatype = 'INT2S'),
             mndwi = list(bands = c('sr_band2', 'sr_band5'),
                          fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                          datatype = 'INT2S'),
             nbr = list(bands = c('sr_band4', 'sr_band7'),
                        fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                        datatype = 'INT2S'),
             savi = list(bands = c('sr_band3', 'sr_band4'),
                         fun = function(x1, x2){10000 * (1 + L) * (x2/10000 - x1/10000) / (x2/10000 + x1/10000 + L)},
                         datatype = 'INT2S'),
             evi = list(bands = c('sr_band1', 'sr_band3', 'sr_band4'),
                        fun = function(x1, x2, x3){10000 * 2.5 * (x3/10000 - x2/10000)/(x3/10000 + 6 * x2/10000 - 7.5 * x1/10000 + 1)},
                        datatype = 'INT2S'),
             tcb = list(bands = c('sr_band1', 'sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band7'),
                        # TODO: for tassel cap, add reference(s), where did we get these coefficients? (crist, 1985, A TM tasseled cap equivalent transformation for reflectance factor data) ?
                        fun = function(x1, x2, x3, x4, x5, x6){0.2043 * x1 + 0.4158 * x2 + 0.5524 * x3 + 0.5741 * x4 + 0.3124 * x5 + 0.2303 * x6},
                        datatype = 'FLT4S'), # TODO: Not convinced TC needs to be float
             tcg = list(bands = c('sr_band1', 'sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){-0.1603 * x1 + 0.2819 * x2 - 0.4934 * x3 + 0.7940 * x4 -0.0002 * x5 -0.1446 * x6},
                        datatype = 'FLT4S'),
             tcw = list(bands = c('sr_band1', 'sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){0.0315 * x1 + 0.2021 * x2 + 0.3102 * x3 + 0.1594 * x4 -0.6806 * x5 -0.6109 * x6},
                        datatype = 'FLT4S')
             
    ),
    ### Landsat 8
    C = list(ndvi = list(bands = c('sr_band4', 'sr_band5'),
                         fun = function(x1, x2){10000 * (x2 - x1) / (x1 + x2)},
                         datatype = 'INT2S'),
             ndmi = list(bands = c('sr_band6', 'sr_band7'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                         datatype = 'INT2S'),
             ndwi = list(bands = c('sr_band3', 'sr_band5'),
                         fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                         datatype = 'INT2S'),
             mndwi = list(bands = c('sr_band3', 'sr_band6'),
                          fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                          datatype = 'INT2S'),
             nbr = list(bands = c('sr_band5', 'sr_band7'),
                        fun = function(x1, x2){10000 * (x1 - x2) / (x1 + x2)},
                        datatype = 'INT2S'),
             savi = list(bands = c('sr_band4', 'sr_band5'),
                         fun = function(x1, x2){10000 * (1 + L) * (x2/10000 - x1/10000) / (x2/10000 + x1/10000 + L)},
                         datatype = 'INT2S'),
             evi = list(bands = c('sr_band2', 'sr_band4', 'sr_band5'),
                        fun = function(x1, x2, x3){10000 * 2.5 * (x3/10000 - x2/10000)/(x3/10000 + 6 * x2/10000 - 7.5 * x1/10000 + 1)},
                        datatype = 'INT2S'),
             tcb = list(bands = c('sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band6', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){0.2043 * x1 + 0.4158 * x2 + 0.5524 * x3 + 0.5741 * x4 + 0.3124 * x5 + 0.2303 * x6},
                        datatype = 'FLT4S'),
             tcg = list(bands = c('sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band6', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){- 0.1603 * x1 + 0.2819 * x2 -0.4934 * x3 + 0.7940 * x4 -0.0002 * x5 -0.1446 * x6},
                        datatype = 'FLT4S'),
             tcw = list(bands = c('sr_band2', 'sr_band3', 'sr_band4', 'sr_band5', 'sr_band6', 'sr_band7'),
                        fun = function(x1, x2, x3, x4, x5, x6){0.0315 * x1 + 0.2021 * x2 + 0.3102 * x3 + 0.1594 * x4 -0.6806 * x5 -0.6109 * x6},
                        datatype = 'FLT4S')
             
    )
)

bfastSpatial.env$VEGETATION_INDICES$E <- bfastSpatial.env$VEGETATION_INDICES$T
                    
