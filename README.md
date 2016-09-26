[![DOI](https://zenodo.org/badge/21430/dutri001/bfastSpatial.svg)](https://zenodo.org/badge/latestdoi/21430/dutri001/bfastSpatial)

# bfastSpatial

```
    _      __          _   _____             _   _       _ 
   | |    / _|        | | /  ___|           | | (_)     | |
   | |__ | |_ __ _ ___| |_\ `--. _ __   __ _| |_ _  __ _| |
   | '_ \|  _/ _` / __| __|`--. \ '_ \ / _` | __| |/ _` | |
   | |_) | || (_| \__ \ |_/\__/ / |_) | (_| | |_| | (_| | |
   |_.__/|_| \__,_|___/\__\____/| .__/ \__,_|\__|_|\__,_|_|
                                | |                        
                                |_|            
```


Package to pre-process gridded time-series data in order for them to be analyzed with change detection algorithms such as bfast. Uses classes from the raster package and includes utilities to run the algorithms and post-process the results.

## Installation

The package can be installed directly from github using devtools

```R
library(devtools)
install_github('loicdtx/bfastSpatial')
```

## Tutorials

- [Introductory tutorial](http://www.loicdutrieux.com/talks/SCERIN-2015-bfastSpatial/) that presents the general functionalities of the package and how to quickly explore a dataset and generate change results.
- [Full tutorial](http://loicdtx.github.io/bfastSpatial/) in which *everything* is covered, from data download to pre-processing and analysis.
- If you are already familiar with R, the raster package and Landsat/MODIS data, you may skip the detailed tutorial and go directly to the [quick start guide](http://loicdtx.github.io/bfastSpatial/quickStart#/).


Feedback on the package and the documentation is very much welcome. Send your feedback to Ben or Loic

## Citation

The package can be cited in (scientific) publications by using the following citing information.

```
@software{bfastSpatial,
  author = {Dutrieux, Lo\"{i}c and DeVries, Ben},
  title = {{bfastSpatial: Set of utilities and wrappers to perform change detection on satellite image time-series}},
  url = {https://github.com/loicdtx/bfastSpatial},
  version = {0.6.2},
  date = {2014-12-04},
  doi = {10.5281/zenodo.49693}
}
```
