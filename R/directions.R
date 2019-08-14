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
              #use "as" functionality
              directionsData <- as(object = directionsFiles, Class = "Directions",
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
                  if ((i %% 10) == 0) gc()
              }
              gc()
              directionsData@data <- dplyr::bind_rows(dirDataRaw)
              validObject(directionsData)
              return(directionsData)
          }
)
