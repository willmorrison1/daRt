simulationHandle <- function(x) {

   simHandleObj <- new("SimulationHandle",
        simDir = x,
        isSequence = isSequence(x),
        sequenceInfo = sequenceInfo(x),
        softwareVersion = versionInfo(x))
   validObject(simHandleObj)

   return(simHandleObj)

}
