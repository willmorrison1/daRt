setMethod("variablesRB3D", "SimulationFilter", function(x) x@variablesRB3D)

setMethod("variablesRB3D<-", "SimulationFilter", function(x, value){
    x@variablesRB3D <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("variablesRB3D", "SimulationFiles", function(x) x@simulationFilter@variablesRB3D)
