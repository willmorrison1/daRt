setMethod("initialize", "SimulationHandle",
          function(.Object,
                   simDir = character(),
                   isSequence = logical(),
                   sequenceInfo = data.frame(),
                   softwareVersion = data.frame()){

            .Object@simDir <- simDir
            .Object@simName <- basename(simDir)
            .Object@isSequence <- isSequence
            .Object@sequenceInfo <- sequenceInfo
            .Object@softwareVersion <- softwareVersion

            return(.Object)
          }
)
