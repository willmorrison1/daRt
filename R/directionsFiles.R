setMethod(f = "directionsFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              simHandle <- simulationHandle(x)
              dirctnFiles <- as(object = simHandle, Class = "SimulationFiles")
              #TODOmake "attachSimulationFilter" method - whereby i can e.g. make sure the sF bands are updated accordingly
              #e.g. if sF has integer() bands i need to add the .getWavelengthsDF(SimulationFiles)$band
              #function to get all bands.
              dirctnFiles@simulationFilter <- sF
              subDirs <- simdir(sF)
              fileNames <- file.path(simdir(dirctnFiles), subDirs$dirName, tolower(subDirs$variable))
              fullFiles <- data.frame(subDirs[c("band", "variable", "iter", "typeNum")],
                                      "fileName" = fileNames, stringsAsFactors = FALSE)
              dirctnFiles@files <- fullFiles

              return(dirctnFiles)

          })
