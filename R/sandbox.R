# Test cleanMODIS
cleanMODIS(x='/media/LP_DUTRIEUX_Data/RS/sandbox/MOD17A2.A2008097.h11v09.005.2008142064019.hdf', data_SDS=1, QC_SDS=3, bit=TRUE, QC_val=0x19, filename='/media/LP_DUTRIEUX_Data/RS/sandbox/MOD17A2.A2008097.h11v09.005.2008142064019.tif', datatype = 'INT2S', overwrite = TRUE)


cleanMODIS(x='/media/LP_DUTRIEUX_Data/RS/sandbox/MOD17A2.A2008097.h11v09.005.2008142064019.hdf', data_SDS=1, QC_SDS=3, bit=TRUE, QC_val=0x19, filename='/media/LP_DUTRIEUX_Data/RS/sandbox/processMODIStest/individual/MOD17A2.A2008097.h11v09.005.2008142064019.tif', datatype = 'INT2S', overwrite = TRUE, fill = (32761:32767))




name <- '/media/LP_DUTRIEUX_Data/RS/sandbox/MOD17A2.A2008097.h11v09.005.2008142064019.tif'
basename(name)
str_extract(basename(name), '\\.A\\d{7}\\.')
str_match(pattern='(\\.A)(\\d{7})(\\.)', basename(name))[,3]


n <- basename('/media/LP_DUTRIEUX_Data/RS/sandbox/MOD17A2.A2008097.h11v09.005.2008142064019.hdf')
s <- str_match(n, pattern='^.*\\.A\\d{7}\\.')[1]
sprintf('%stif', s)


library(gdalUtils)
library(stringr)
library(raster)
library(parallel)
library(bitops)

processMODISbatch(x = '/media/LP_DUTRIEUX_Data/RS//MODIS//MOD17A2/', pattern = glob2rx('*.hdf'), data_SDS = 1, QC_SDS = 3, bit = TRUE, QC_val = 0x19, fill = FALSE, outdir = '/media/LP_DUTRIEUX_Data/RS/sandbox/processMODIStest/mosaic/', mosaic = TRUE, mc.cores = 3)

processMODISbatch(x = '/media/LP_DUTRIEUX_Data/RS//MODIS//MOD17A2/', pattern = glob2rx('*.hdf'), data_SDS = 1, QC_SDS = 3, bit = TRUE, QC_val = 0x19, fill = (32761:32767), outdir = '/media/LP_DUTRIEUX_Data/RS/sandbox/processMODIStest/individual/', mosaic = FALSE, mc.cores = 1)