
<!-- README.md is generated from README.Rmd. Please edit that file -->
daRt
====

<!-- badges: start -->
<!-- badges: end -->
This readme is very work in progress.

The daRt package provides a very quick and flexible way to import data that is produced by the Discrete Anisotropic Radiative Transfer (DART) model. The data in daRt are formatted in a way that facilitates rapid data analysis.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("willmorrison1/daRt")
```

Example
-------

Load the package

``` r
library(daRt)
```

Create and modify the "SimulationFilter" object. This defines what data you want to extract from a DART output directory structure

``` r
#define SimulationFilter object - define "directions" as the product
sF <- simulationFilter(product = "directions")

#show the SimulationFilter
sF
#> 'SimulationFilter' object for DART product: directions 
#> 
#> bands:          BAND0 
#> variables:      BRF 
#> iterations:     ITER1, ITERX 
#> variablesRB3D:  Intercepted, Scattered, Emitted, Absorbed, +ZFaceExit, +ZFaceEntry 
#> typeNums:        
#> imageType:       
#> imageNo:

#list the 'setters' and 'accessors'
methods(class = "SimulationFilter")
#>  [1] bands           bands<-         getData         getFiles       
#>  [5] imageFiles      imageNo         imageNo<-       imageType      
#>  [9] imageType<-     iters           iters<-         product        
#> [13] product<-       show            simdir          typeNums       
#> [17] typeNums<-      variables       variables<-     variablesRB3D  
#> [21] variablesRB3D<-
#> see '?methods' for accessing help and source code

#e.g. change the 'bands', then the 'iterations'
bands(sF) <- c("BAND0", "BAND1")
iters(sF) <- "ITER1"
```

Now explore the DART output directory structure

``` r
#define the simulation directory
simulationDir <- "man/sampleSimulation/cesbio"

#define the SimulationFiler as shown above (i.e. 'sF'), but in one line
sF1 <- simulationFilter(product = "directions", 
                       bands = c("BAND0", "BAND1"), 
                       iters = "ITER1")
#get simulation files based on the defined filter
simFiles <- daRt::getFiles(x = simulationDir, sF = sF1)

#show these files are we happy to continue and load the data, or
#do we want to adjust the SimulationFilter? daRt::getFiles is essentially
#a 'dry-run' of the data extraction
files(simFiles)
#> [1] "man/sampleSimulation/cesbio/output//BAND0/BRF/ITER1/brf"
#> [2] "man/sampleSimulation/cesbio/output//BAND1/BRF/ITER1/brf"
```

Now extract DART output data

``` r
#get simulation data
simData <- daRt::getData(x = simulationDir, sF = sF1)
```

Documentation needs updating and finishing from here

``` r
#plot using ggplot2
library(ggplot2)
plotOut <- ggplot(simData@data) +
    geom_point(aes(x = zenith, y = value, colour = azimuth)) +
    facet_wrap(~ band) +
    theme(aspect.ratio = 1)
plot(plotOut)
```

<img src="man/figures/README-plot data example-1.png" width="100%" /> Then alter the SimulationFilter to look at images

``` r
product(sF) <- "images"
simData <- daRt::getData(x = simulationDir, sF = sF)
ggplot(simData@data) + 
    geom_raster(aes(x = x, y = y, fill = value)) +
    facet_grid(band ~ imageNo) +
    theme(aspect.ratio = 1)
```

<img src="man/figures/README-images example-1.png" width="100%" />
