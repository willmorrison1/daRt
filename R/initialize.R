setMethod("initialize", "SimulationHandle",
          function(.Object,
                   simDir = character(),
                   isSequence = logical(),
                   sequenceInfo = data.frame()){

            .Object@simDir <- simDir
            .Object@simName <- basename(simDir)
            .Object@isSequence <- isSequence
            .Object@sequenceInfo <- sequenceInfo

            return(.Object)
          }
)
