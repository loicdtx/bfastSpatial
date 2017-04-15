context("Landsat filename parsing")

test_that("getSceneinfo parses filenames properly for different landsat collections", {
    ##
    name1 <- 'ndvi.LC82300702014234.tar.gz'
    df1 <- data.frame(row.names = 'LC82300702014234',
                      sensor = 'OLI',
                      path = 230,
                      row = 70,
                      date = as.Date('2014234', '%Y%j'),
                      tier = NA,
                      stringsAsFactors = FALSE)
    ##
    name2 <- '/home/username/LT51970291984107-SC20161024054752.tar.gz'
    df2 <- data.frame(row.names = 'LT51970291984107',
                      sensor = 'TM',
                      path = 197,
                      row = 29,
                      date = as.Date('1984107', '%Y%j'),
                      tier = NA,
                      stringsAsFactors = FALSE)
    ##
    name3 <- 'LE71960292016244NSG00_sr_band7.tif'
    df3 <- data.frame(row.names = 'LE71960292016244',
                      sensor = 'ETM+ SLC-off',
                      path = 196,
                      row = 29,
                      date = as.Date('2016244', '%Y%j'),
                      tier = NA,
                      stringsAsFactors = FALSE)
    ##
    name4 <- 'LE71960292002244NSG00-SC20161024054752.tar.gz'
    df4 <- data.frame(row.names = 'LE71960292002244',
                      sensor = 'ETM+ SLC-on',
                      path = 196,
                      row = 29,
                      date = as.Date('2002244', '%Y%j'),
                      tier = NA,
                      stringsAsFactors = FALSE)
    ##
    name5 <- '/home/username/LC08_L1TP_019046_20170311_20170317_01_T1_sr_band4.tif'
    df5 <- data.frame(row.names = 'LC08_L1TP_019046_20170311_20170317_01_T1',
                      sensor = 'OLI',
                      path = 19,
                      row = 46,
                      date = as.Date('20170311', '%Y%m%d'),
                      tier = 'T1',
                      stringsAsFactors = FALSE)
    ##
    name6 <- '/home/username/LE070190462017013001T1-SC20170330202642.tar.gz'
    df6 <- data.frame(row.names = 'LE070190462017013001T1',
                      sensor = 'ETM+ SLC-off',
                      path = 19,
                      row = 46,
                      date = as.Date('20170130', '%Y%m%d'),
                      tier = 'T1',
                      stringsAsFactors = FALSE)
    ##
    name7 <- '/home/username/L2/A2008009000500.L2_LAC_OC.nc'
    df7 <- data.frame(row.names = '/home/username/L2/A2008009000500.L2_LAC_OC.nc',
                      sensor = NA,
                      path = NA,
                      row = NA,
                      date = NA,
                      tier = NA,
                      stringsAsFactors = FALSE)
    ##
    expect_identical(getSceneinfo(name1), df1)
    expect_identical(getSceneinfo(name2), df2)
    expect_identical(getSceneinfo(name3), df3)
    expect_identical(getSceneinfo(name4), df4)
    expect_identical(getSceneinfo(name5), df5)
    expect_identical(getSceneinfo(name6), df6)
    expect_warning(getSceneinfo(name7), 'Some of the characters provided do not contain recognized Landsat5/7/8 scene ID')
    expect_identical(getSceneinfo(name7), df7)
    
})