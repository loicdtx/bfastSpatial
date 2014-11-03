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

# Examples
library(raster)
dir <- file.path(tmpDir(), 'MODIStest'); dir.create(path = dir)
 
fileDL <- 'http://e4ftl01.cr.usgs.gov/MOLT/MOD17A2.055/2003.07.12/MOD17A2.A2003193.h19v07.055.2011269202445.hdf'
modis <- file.path(dir, basename(fileDL)); download.file(url = fileDL, destfile = modis)

# Now We've just downloaded a MODIS file that is stored in a subdirectory of the raster tmp directory
sprintf('These data have been acquired around %s', getMODISinfo(modis)$date)

# Clean dataset and replace fill values by NAs
# Product details at https://lpdaac.usgs.gov/products/modis_products_table/mod17a2
MODISclean <- cleanMODIS(x=modis, data_SDS=1, QC_SDS=3, bit=TRUE, QC_val=0x19, fill = (32761:32767))
plot(MODISclean)

# In that case we did not write the result back to disk, but note that this is possible
# For that use the filename= argument, and the datatype= argument (recommended)




