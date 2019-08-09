setMethod("bands", "SimulationFilter", function(x) x@bands)

setMethod("bands<-", "SimulationFilter", function(x, value){
    x@bands <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("bands", "SimulationFiles", function(x) x@simulationFilter@bands)
