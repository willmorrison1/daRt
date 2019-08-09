.readOutputDirections <- function(fileName){

    require(data.table)
    rawDirectionsData <- data.table::fread(fileName, data.table = FALSE)
    colnames(rawDirectionsData) <- c("zenith", "azimuth", "value")
    return(rawDirectionsData)
}

setMethod(f = "directions",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              directionsFiles <- getFiles(x, sF)
              directionsData <- new("Directions", simDir = x)
              directionsData@simulationFilter <- sF
              directionsData@files <- directionsFiles@files
              directionsData@isSequence <- directionsFiles@isSequence
              directionsData@sequenceInfo <- directionsFiles@sequenceInfo
              directionsData@sequenceInfoList <- directionsFiles@sequenceInfoList
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
              directionsData@data <- dplyr::bind_rows(dirDataRaw)
              validObject(directionsData)
              return(directionsData)
          }
)
