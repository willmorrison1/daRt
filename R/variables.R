setMethod("variables", "SimulationFilter", function(x) x@variables)

setMethod("variables<-", "SimulationFilter", function(x, value){
    x@variables <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("variables", "SimulationFiles", function(x) x@simulationFilter@variables)
