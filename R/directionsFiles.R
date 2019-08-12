setMethod(f = "directionsFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

            dirctnFiles <- new("SimulationFiles",
                               simDir = x,
                               isSequence = isSequence(x),
                               sequenceInfo = sequenceInfo(x))
            dirctnFiles@simulationFilter <- sF
            subDirs <- simdir(sF)
            fileNames <- file.path(simdir(dirctnFiles), subDirs$dirName, tolower(subDirs$variable))
            fullFiles <- data.frame(subDirs[c("band", "variable", "iter", "typeNum")],
                                    "fileName" = fileNames, stringsAsFactors = FALSE)
            dirctnFiles@files <- fullFiles
            return(dirctnFiles)
          })
