#' @export
setMethod("imageTypes", "SimulationFilter", function(x) x@imageTypes)
#' @export
setMethod("imageTypes<-", "SimulationFilter", function(x, value){
    x@imageTypes <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("imageTypes", "SimulationFiles", function(x) x@simulationFilter@imageTypes)
