setMethod("imageType", "SimulationFilter", function(x) x@imageType)

setMethod("imageType<-", "SimulationFilter", function(x, value){
    x@imageType <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("imageType", "SimulationFiles", function(x) x@simulationFilter@imageType)
