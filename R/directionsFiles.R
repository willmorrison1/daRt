setMethod(f = "directionsFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              simHandle <- simulationHandle(x)
              dirFiles <- as(object = simHandle, Class = "SimulationFiles")
              simulationFilter(dirFiles) <- sF
              subDirs <- subDir(dirFiles)
              fileNames <- file.path(baseDir(dirFiles), subDirs$dirName, tolower(subDirs$variable))
              fullFiles <- data.frame(subDirs[c("band", "variable", "iter", "typeNum")],
                                      "fileName" = fileNames, stringsAsFactors = FALSE)
              dirFiles@files <- fullFiles

              return(dirFiles)

          })
