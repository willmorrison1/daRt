
<!-- README.md is generated from README.Rmd. Please edit that file -->
daRt
====

<!-- badges: start -->
<!-- badges: end -->
This readme is very work in progress.

The goal of daRt is to ...

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("willmorrison1/daRt")
```

Example
-------

This is a basic example which shows you how to solve a common problem:

``` r
library(daRt)
#define the simulation directory
simulationDir <- "man/sampleSimulation/cesbio"
#define SimulationFilter object - define "directions" as the product and filter all 
#but the first iteration ("ITER1") and two bands ("BAND1", "BAND2")
sF <- simulationFilter(product = "directions", 
                       iters = "ITER1", 
                       bands = c("BAND1", "BAND2"))
#show contents 
sF
#> 'SimulationFilter' object for DART product: directions 
#> 
#> bands:          BAND1, BAND2 
#> variables:      BRF 
#> iterations:     ITER1 
#> variablesRB3D:  Intercepted, Scattered, Emitted, Absorbed, +ZFaceExit, +ZFaceEntry 
#> typeNums:        
#> imageType:       
#> imageNo:
#get simulation files based on the defined filter
simFiles <- daRt::getFiles(x = simulationDir, sF = sF)
#> Loading required package: dplyr
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> [1] "validating sim handle(s)"
#> Loading required package: stringr
#> [1] "validating directions files"
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
#get simulation data
simData <- daRt::getData(x = simulationDir, sF = sF)
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
#> Loading required package: data.table
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
#> [1] "validating directions"
#plot using ggplot2
library(ggplot2)
plotOut <- ggplot(simData@data) +
    geom_point(aes(x = zenith, y = value, colour = azimuth)) +
    facet_wrap(~ band)
plot(plotOut)
```

<img src="man/figures/README-directions example-1.png" width="100%" /> Then alter the SimulationFilter to look at images

``` r
#instead, look at images
#todo - export "products"
sF@product <- "images"
simData <- daRt::getData(x = simulationDir, sF = sF)
#> Loading required package: tools
#> Loading required package: reshape2
#> 
#> Attaching package: 'reshape2'
#> The following objects are masked from 'package:data.table':
#> 
#>     dcast, melt
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
#> [1] "validating sim handle(s)"
#> [1] "validating directions files"
ggplot(simData@data) + 
    geom_raster(aes(x = Var1, y = Var2, fill = value)) +
    facet_grid(band ~ imageNo)
```

<img src="man/figures/README-images example-1.png" width="100%" />
