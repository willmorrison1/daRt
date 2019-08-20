.readOutputDirections <- function(fileName){

    require(data.table)
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
              dirDataRaw <- vector(mode = "list", length = nrow(directionsData@files))
              for (i in 1:nrow(directionsData@files)) {
                  fileRow <- directionsData@files[i, ]
                  dirDataRaw[[i]] <- .readOutputDirections(fileRow$fileName)
                  dirDataRaw[[i]]$band <- fileRow$band
                  dirDataRaw[[i]]$variable <- fileRow$variable
                  dirDataRaw[[i]]$iter <- fileRow$iter
                  dirDataRaw[[i]]$typeNum <- fileRow$typeNum
                  dirDataRaw[[i]]$simName <- fileRow$simName
              }
              gc()
              directionsData@data <- dplyr::bind_rows(dirDataRaw)
              validObject(directionsData)
              return(directionsData)
          }
)
