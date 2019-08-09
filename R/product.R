setMethod("product", "SimulationFilter", function(x) x@product)

setMethod("product<-", "SimulationFilter", function(x, value){
    x@product <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("product", "SimulationFiles", function(x) x@simulationFilter@product)
