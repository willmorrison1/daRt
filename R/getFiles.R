setMethod(f = "getFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              require(dplyr)

              if (product(sF) == "directions") filesFun <- directionsFiles
              if (product(sF) == "rb3D") filesFun <- rb3DFiles
              if (product(sF) == "images") filesFun <- imageFiles

              simFilesList <- vector(mode = "list", length = length(x))
              for (i in 1:length(x)) {
                  simFilesList[[i]] <- filesFun(x = x[i], sF = sF)
              }
              sequenceInfoList <- lapply(simFilesList, function(x) x@sequenceInfo[-3])
              uniqueSequenceInfo <- unique(sequenceInfoList)
              if (length(uniqueSequenceInfo) > 1) {
                  stop("When merging multiple simulations,
                       there was a sequence info mismatch between simulations
                       i.e. the sequences use different variables")
              }
              sequenceInfoOut <- uniqueSequenceInfo[[1]]
              simFilesStacked <- new(Class = class(simFilesList[[1]])[1],
                                     simDir = sapply(simFilesList, simdir),
                                     isSequence = sapply(simFilesList, function(x) x@isSequence),
                                     sequenceInfo = sequenceInfoOut)
              simFilesStacked@sequenceInfoList <- sequenceInfoList
              simFilesStacked@simulationFilter <- simFilesList[[1]]@simulationFilter
              for (i in 1:length(simFilesList)) {
                  simFilesList[[i]]@files$simName <- simname(simFilesList[[i]])
              }
              simFilesStacked@files <- dplyr::bind_rows(lapply(simFilesList, function(x) x@files))
              validObject(simFilesStacked)

              return(simFilesStacked)
          })
