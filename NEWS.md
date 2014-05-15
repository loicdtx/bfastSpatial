# News

## Initial package (April 2014)
Includes utilities to:

1. Pre-process Landsat data
    - Wrapper to process tarball downloaded from USGS into Vegetation Index layer `processLandat()`
    - Batcher to automate the process over several files `processLandsatBatch()`
    - Utilities for creating multilayer raster objects with time dimention `timeStack()`

2. Run `bfastmonitor()` pixel based, on a stack created with `timeStack()`. See `bfmSpatial()`
3. Inventory data amount
4. Parse through Lansat typical filenames to extract metadata on sensor, data of acquisition, etc (`getSceneinfo()`)
5. Post-process change detection results (filters, etc)