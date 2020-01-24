#' @export
setMethod(f = "removeRelief",
          signature = signature(x = "RB3D", DEM = "RasterLayer"),
          definition = function(x, DEM, DARTmodelElevation, maxUndergroundCells = 10, ...) {
browser()
              xSize_simProperty <- getSimulationProperty(x, "cell.size.x")
              zSize_simProperty <- getSimulationProperty(x, "cell.size.z")
              xyzSize <- list()

              for (i in 1:length(xSize_simProperty)) {
                  if (xSize_simProperty[[i]][2] != xSize_simProperty[[i]][2]) {
                      stop(paste("X and Y cell size was not equal for simname:", names(xSize_simProperty)[i]))
                  }
                  xyzSize[[i]] <- data.frame(
                      "xy" = as.numeric(xSize_simProperty[[i]][2]),
                      z = as.numeric(zSize_simProperty[[i]][2]), stringsAsFactors = FALSE)
              }

              xyzDF <- do.call(rbind, xyzSize)
              xyzDF$simName <- names(xSize_simProperty)

              #this transformation works for one RB3D resolution at a time
              for (i in 1:nrow(unique(xyzDF[c("xy", "z")]))) {
                  XYsize <- as.numeric(xyzDF[i, ]$xy)
                  Zsize <- as.numeric(xyzDF[i, ]$z)
                  #crop raster to RB3D extent
                  simValsInd <- x@data$simName == simname(x)[i]
                  xCellsXYZ <- c(max(x@data$X[simValsInd]), max(x@data$Y[simValsInd]), max(x@data$Z[simValsInd]))
                  RB3Dsize <- xCellsXYZ * c(XYsize, XYsize, Zsize)
                  rasterSize <- dim(DEM)[1:2] * res(DEM)
                  if (any(rasterSize < RB3Dsize[1:2])) {
                      stop(paste("The extent (m) of the input DEM is smaller than that of the RB3D dataset.
                                 It needs to be the same size or larger."))
                  }
                  #DART XY is switched for raster XY, so take DART "YX" when doing difference (untested)
                  cropOffset <- rasterSize - RB3Dsize[2:1]
                  DEMc <- raster::crop(DEM, extent(DEM) -
                                           (c(-cropOffset[1], cropOffset[1], -cropOffset[2], cropOffset[2]) / 2))
                  #resample the DEM
                  DEMr <- rasterNewRes(inR = DEMc, newRes_m = XYsize, ...)

                  #how high are all the ground cells above the lowest point of ground?
                  #round this to the resolution of RB3D and floor it to integer values
                  #using the resolution of the RB3D
                  heightDiffRaster <- mround(DEMr - raster::cellStats(DEMr, min), Zsize) / Zsize
                  #convert from raster to data frame
                  heightDiffDF <- reshape2::melt(raster::as.matrix(heightDiffRaster), varnames = c("X", "Y"),
                                                 value.name = "z") %>%
                      dplyr::mutate(z = as.integer(z))
                  rm(heightDiffRaster);gc()

                  #index all simulations with this resolution
                  RB3DresInd <- xyzDF$xy == xyzDF[i, ]$xy & xyzDF$z == xyzDF[i, ]$z
                  simind <-  x@data$simName %in% xyzDF$simName[RB3DresInd]
                  #apply the transformation to these simulations
                  toJoin <- x@data[simind, ] %>%
                      dplyr::left_join(heightDiffDF, by = c("X", "Y")) %>%
                      dplyr::mutate(Z = (Z - z) - DARTmodelElevation) %>%
                      dplyr::select(-z)
                  x@data[simind, ] <- toJoin
                  rm(simind, heightDiffDF, RB3DresInd, DEMc, DEMr, simValsInd, toJoin); gc()
              }

              maxHorizontal <- x@data %>%
                  dplyr::group_by(X, Y, band, iter, typeNum, simName) %>%
                  dplyr::summarise(maxZ = max(Z)) %>%
                  dplyr::group_by(band, iter, typeNum, simName) %>%
                  dplyr::summarise(minZ_perArray = min(maxZ))

              x@data <- x@data %>%
                  dplyr::filter(Z >= -maxUndergroundCells)

              gc()

              x@data <- x@data %>%
                  dplyr::left_join(maxHorizontal, by = c("band", "iter", "typeNum", "simName")) %>%
                  dplyr::filter(Z <= minZ_perArray) %>%
                  dplyr::select(-minZ_perArray)

              return(x)
          }
)

rasterNewRes <- function(inR, newRes_m, ...){
    #resample a raster to a new resolution
    #... passed to raster::aggregate
    require(QOLfunctions)

    requiredRaster <- raster(res = newRes_m)
    extent(requiredRaster) <- QOLfunctions::mround(extent(inR), newRes_m)
    dim(requiredRaster) <- round(c(dim(inR)[1:2] / (newRes_m/res(inR))[1:2], dim(inR)[3]))
    crs(requiredRaster) <- crs(inR)
    outR <- raster::aggregate(inR, fact = res(requiredRaster) / res(inR), ...)

    return(outR)
}
