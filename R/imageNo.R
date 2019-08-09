setMethod("imageNo", "SimulationFilter", function(x) x@imageNo)

setMethod("imageNo<-", "SimulationFilter", function(x, value){
    x@imageNo <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("imageNo", "SimulationFiles", function(x) x@simulationFilter@imageNo)
