.readOutputDirections <- function(fileName){

    rawDirectionsData <- data.table::fread(fileName, data.table = FALSE)
    colnames(rawDirectionsData) <- c("zenith", "azimuth", "value")

    return(rawDirectionsData)
}

setMethod(f = "directions",
          signature = signature(x = "SimulationFiles"),
          definition = function(x, nCores = 1){

              #use "as" functionality
              directionsData <- as(object = x, Class = "Directions",
                                   strict = TRUE)
              if (nrow(directionsData@files) < nCores) nCores <- nrow(directionsData@files)
              if (nCores > 1) {
                  cl <- parallel::makeCluster(nCores)
                  doParallel::registerDoParallel(cl)
              }
              dirDataRaw <- foreach(i = 1:nrow(directionsData@files), .export = ".readOutputDirections",
                      .packages = "data.table") %dopar% {
                          fileRow <- directionsData@files[i, ]
                          dirDataRaw <- .readOutputDirections(fileRow$fileName)
                          dirDataRaw$band <- fileRow$band
                          dirDataRaw$variable <- fileRow$variable
                          dirDataRaw$iter <- fileRow$iter
                          dirDataRaw$typeNum <- fileRow$typeNum
                          dirDataRaw$simName <- fileRow$simName
                          return(dirDataRaw)
                      }
              gc()
              if (nCores > 1) stopCluster(cl)
              directionsData@data <- dplyr::bind_rows(dirDataRaw)
              validObject(directionsData)

              return(directionsData)
          }
)
