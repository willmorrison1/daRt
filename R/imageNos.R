#' @export
setMethod("imageNos", "SimulationFilter", function(x) x@imageNos)
#' @export
setMethod("imageNos<-", "SimulationFilter", function(x, value){
    x@imageNos <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("imageNos", "SimulationFiles", function(x) x@simulationFilter@imageNos)
