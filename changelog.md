# Change Log - 27-01-16

Major changes on develop branch since December 2014: [4a0d4c4](https://github.com/dutri001/bfastSpatial/commit/4a0d4c48918d3c7f1bd44b44abbefa1eb2246710)

* added ```fileExt``` argument to ```processLandsat()``` to allow for other output formats (GeoTiff, etc.)
* incporated LC8 band naming in ```sr2vi()``` and index functions in ```LandsatVIs.R```
* added new indices to ```sr2vi()``` and ```LandsatVIs.R```: NDWI, MNDWI, tasseled cap components (TCB, TCG, TCW)
* Tasseled cap components based on single coefficient matrix based on surface reflectances. Option to include alternate or multiple sets of coefficients to be considered in future developments. Also need to look into need for different coefficients for OLI data
* added function ```fitHarmonTrend()``` - fits a harmonic-trend model to a raster time series, or a temporal subset thereof and outputs model coefficients (intercept and the appropriate trend and sin/cosine coefficients)
* changed ```annualSummary()``` to ```annualComposite()``` and included option to limit compositing to specific periods (e.g. seasons)

