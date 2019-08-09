setMethod("typeNums", "SimulationFilter", function(x) x@typeNums)

setMethod("typeNums<-", "SimulationFilter", function(x, value){
    x@typeNums <- value
    if (validObject(x, complete = TRUE)) return(x)
})

setMethod("typeNums", "SimulationFiles", function(x) x@simulationFilter@typeNums)
