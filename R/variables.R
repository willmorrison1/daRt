#' @export
setMethod("variables", "SimulationFilter", function(x) x@variables)
#' @export
setMethod("variables<-", "SimulationFilter", function(x, value){
    x@variables <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("variables", "SimulationFiles", function(x) x@simulationFilter@variables)
