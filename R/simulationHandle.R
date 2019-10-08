simulationHandle <- function(x) {

   simHandleObj <- new("SimulationHandle",
        baseDir = x,
        isSequence = isSequence(x),
        sequenceInfo = sequenceInfo(x),
        softwareVersion = versionInfo(x))
   validObject(simHandleObj)

   return(simHandleObj)

}
