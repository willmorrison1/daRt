setMethod("initialize", "SimulationHandle",
          function(.Object,
                   baseDir = character(),
                   isSequence = logical(),
                   sequenceInfo = data.frame(),
                   softwareVersion = data.frame()){

            .Object@baseDir <- baseDir
            .Object@simName <- basename(baseDir)
            .Object@isSequence <- isSequence
            .Object@sequenceInfo <- sequenceInfo
            .Object@softwareVersion <- softwareVersion

            return(.Object)
          }
)
